use crate::{
    env::EnvStore,
    error::{Error, Result, WithContext},
    midi::{Event, Header, Track},
    parser::{AstPtr, Expr, Parser, StrId, NIL},
    value::Value,
    FnvIndexMap, FnvIndexSet,
};
use std::convert::TryInto;
use std::io::Write;
use std::path::PathBuf;

const INCLUDE_PATH: &str = "./include";

type BuiltinFn<T> = fn(&mut T, AstPtr) -> Result<Value>;
type UnOpFn = fn(Value) -> Result<Value>;
type OpFn = fn(Value, Value) -> Result<Value>;

struct Lambda(u32, Box<[StrId]>, AstPtr);

pub struct Interpreter {
    parser: Parser,
    env: EnvStore,
    builtins: Vec<BuiltinFn<Self>>,
    paths: Vec<PathBuf>,
    lambdas: Vec<Lambda>,
    events: FnvIndexSet<Event>,
    tracks: Vec<Track>,
    c_path: Option<usize>,
    c_ident: Option<StrId>,
    log: FnvIndexMap<StrId, u32>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut itp = Self {
            parser: Parser::new(),
            env: EnvStore::new(),
            builtins: Vec::with_capacity(40),
            paths: Vec::new(),
            lambdas: Vec::new(),
            events: Default::default(),
            tracks: Vec::new(),
            c_path: None,
            c_ident: None,
            log: Default::default(),
        };
        itp._define_builtins();
        itp
    }

    pub fn run<W: Write>(&mut self, writer: &mut W, src: &str) -> Result<Value> {
        let exprs = self.parser.parse(src)?;
        let mut retval = Value::Nil;

        for expr in exprs.iter() {
            retval = self.eval(*expr)?;
        }

        self.write_out(writer)?;
        Ok(retval)
    }

    pub fn get_context(&self, err: Error) -> WithContext {
        WithContext {
            path: self
                .c_path
                .map(|i| &self.paths[i])
                .and_then(|p| p.file_name())
                .map(|p| p.to_owned()),
            line: self.parser.line,
            ident: self.c_ident.map(|i| self.parser.get_str(i).to_string()),
            source: err,
            log: self.get_log(),
        }
    }

    pub fn get_log(&self) -> String {
        let mut log = String::new();

        for (id, count) in self.log.iter() {
            log.push_str(self.parser.get_str(*id));

            if *count > 1 {
                log.push_str(&format!(" ({})", count));
            }

            log.push('\n');
        }

        log
    }

    fn write_out<W: Write>(&mut self, writer: &mut W) -> Result<()> {
        let fmt: u16 = self.get(0).map(|v| v.to_u16().unwrap_or(0))?;
        let div: u16 = self.get(1).map(|v| v.to_u16().unwrap_or(96))?;
        self.c_ident = None;
        let ntrks: u16 = self
            .tracks
            .iter()
            .filter(|t| !t.is_empty())
            .count()
            .try_into()
            .map_err(|_| Error::TooManyTracks)?;

        if ntrks > 0 {
            let header = Header::new(fmt, ntrks, div)?;
            writer.write_all(&header.0).map_err(|_| Error::IO)?;

            for trk in self.tracks.iter_mut().filter(|t| !t.is_empty()) {
                let data = trk.finish()?;
                writer.write_all(data).map_err(|_| Error::IO)?;
            }
        }

        Ok(())
    }

    fn adv_clock(&mut self, expr: AstPtr) -> Result<Value> {
        let (trk, next) = self.expect_u32(expr)?;
        let (dur, next) = self.expect_u32(next)?;
        self.expect_nil(next)?;
        self.get_track(trk).advance(dur)?;
        Ok(Value::Nil)
    }

    fn apply(&mut self, val: Value, cdr: AstPtr) -> Result<Value> {
        match val {
            Value::Builtin(i) => self.builtins[i as usize](self, cdr),
            Value::Lambda(i) => self.call(i, cdr),
            val if cdr == NIL => Ok(val),
            Value::I32(n) => self.repeat(n, cdr),
            _ => Err(Error::CannotApply),
        }
    }

    fn assert(&mut self, expr: AstPtr) -> Result<Value> {
        let (val, nil) = self.expect_arg(expr)?;
        self.expect_nil(nil)?;

        if val != Value::Bool(true) {
            return Err(Error::Assert);
        }

        Ok(Value::Nil)
    }

    fn un_op(&mut self, expr: AstPtr, f: UnOpFn) -> Result<Value> {
        let (lhs, next) = self.expect_arg(expr)?;
        self.expect_nil(next)?;
        f(lhs)
    }

    fn bin_op(&mut self, expr: AstPtr, f: OpFn) -> Result<Value> {
        let (lhs, next) = self.expect_arg(expr)?;
        let (rhs, next) = self.expect_arg(next)?;
        self.expect_nil(next)?;
        f(lhs, rhs)
    }

    fn var_op(&mut self, expr: AstPtr, f: OpFn) -> Result<Value> {
        let (mut acc, next) = self.expect_arg(expr)?;
        let (mut rhs, mut next) = self.expect_arg(next)?;

        loop {
            acc = f(acc, rhs)?;

            if let Ok((val, cdr)) = self.expect_arg(next) {
                rhs = val;
                next = cdr;
            } else {
                break;
            }
        }

        self.expect_nil(next)?;
        Ok(acc)
    }

    fn call(&mut self, lambda: u32, mut arglist: AstPtr) -> Result<Value> {
        let body = self.lambdas[lambda as usize].2;
        let parent_env = self.lambdas[lambda as usize].0;
        let lambda_env = self.env.create(parent_env);
        let mut argc = 0_u32;

        while arglist != NIL {
            let (car, cdr) = self.expect_cons(arglist)?;
            let id = self.get_arg_id(lambda, argc).ok_or(Error::ExtraArgument)?;
            let val = self.eval(car)?;

            if !self.env.extend(lambda_env, id, val) {
                return Err(Error::DuplicateArg);
            }

            arglist = cdr;
            argc += 1;
        }

        if self.get_arg_id(lambda, argc).is_some() {
            return Err(Error::NilArgument);
        }

        self.eval_body(body, lambda_env)
    }

    fn car(&mut self, expr: AstPtr) -> Result<Value> {
        let q = self.expect_quote(expr)?;
        let (car, _) = self.expect_cons(q)?;
        self.quote(car)
    }

    fn cdr(&mut self, expr: AstPtr) -> Result<Value> {
        let q = self.expect_quote(expr)?;
        let (_, cdr) = self.expect_cons(q)?;
        self.quote(cdr)
    }

    fn define(&mut self, expr: AstPtr) -> Result<Value> {
        let (car, cdr) = self.expect_cons(expr)?;
        let id = self.expect_ident(car)?;
        let (expr, nil) = self.expect_cons(cdr)?;
        self.expect_nil(nil)?;
        let val = self.eval(expr)?;
        self.env.extend(self.env.current, id, val);
        Ok(Value::Nil)
    }

    fn eval(&mut self, expr: AstPtr) -> Result<Value> {
        match self.parser.get(expr) {
            Expr::Cons(car, cdr) => {
                let val = self.eval(car)?;
                self.apply(val, cdr)
            }
            Expr::Atom(Value::Ident(id)) => self.get(id),
            Expr::Atom(val) => Ok(val),
        }
    }

    fn eval_body(&mut self, mut next: AstPtr, lambda_env: u32) -> Result<Value> {
        let mut retval = Value::Nil;
        let real_env = self.env.current;
        self.env.current = lambda_env;
        self.env.capture = false;

        while next != NIL {
            let (expr, more) = self.expect_cons(next)?;
            retval = self.eval(expr)?;
            next = more;
        }

        self.env.current = real_env;
        self.env.pop(lambda_env);
        Ok(retval)
    }

    fn expect_arg(&mut self, expr: AstPtr) -> Result<(Value, AstPtr)> {
        let (car, cdr) = self.expect_cons(expr)?;
        Ok((self.eval(car)?, cdr))
    }

    fn expect_cons(&self, expr: AstPtr) -> Result<(AstPtr, AstPtr)> {
        match self.parser.get(expr) {
            Expr::Cons(car, cdr) => Ok((car, cdr)),
            _ => Err(Error::NilArgument),
        }
    }

    fn expect_ident(&self, expr: AstPtr) -> Result<u32> {
        match self.parser.get(expr) {
            Expr::Atom(Value::Ident(i)) => Ok(i),
            _ => Err(Error::NotAnIdent),
        }
    }

    fn expect_midi(&mut self, expr: AstPtr) -> Result<(u32, AstPtr)> {
        let (car, cdr) = self.expect_cons(expr)?;
        Ok((self.eval(car)?.to_midi()?, cdr))
    }

    fn expect_nil(&mut self, expr: AstPtr) -> Result<()> {
        if expr == NIL {
            Ok(())
        } else {
            Err(Error::ExtraArgument)
        }
    }

    fn expect_quote(&mut self, expr: AstPtr) -> Result<AstPtr> {
        match self.eval(expr)? {
            Value::Quote(i) => Ok(i),
            _ => Err(Error::NotAQuote),
        }
    }

    fn expect_u32(&mut self, expr: AstPtr) -> Result<(u32, AstPtr)> {
        let (car, cdr) = self.expect_cons(expr)?;
        Ok((self.eval(car)?.to_u32()?, cdr))
    }

    fn expect_u7(&mut self, expr: AstPtr) -> Result<(u8, AstPtr)> {
        let (car, cdr) = self.expect_cons(expr)?;
        Ok((self.eval(car)?.to_u7()?, cdr))
    }

    fn get(&mut self, id: StrId) -> Result<Value> {
        self.c_ident = Some(id);
        self.env.get(id).ok_or(Error::Undefined)
    }

    fn get_arg_id(&self, lambda: u32, argc: u32) -> Option<u32> {
        self.lambdas[lambda as usize].1.get(argc as usize).copied()
    }

    fn get_event(&self, id: u32) -> Event {
        *self
            .events
            .get_index(id as usize)
            .expect("BUG: Invalid event id")
    }

    fn get_track(&mut self, id: u32) -> &mut Track {
        let trk = id as usize;

        if trk >= self.tracks.len() {
            self.tracks.resize_with(trk + 1, Track::new);
        }

        self.tracks.get_mut(trk).unwrap()
    }

    fn if_(&mut self, expr: AstPtr) -> Result<Value> {
        let (cond, next) = self.expect_arg(expr)?;
        let (car, cdr) = self.expect_cons(next)?;

        if let Ok((_, nil)) = self.expect_cons(cdr) {
            self.expect_nil(nil)?;
        }

        if cond == Value::Bool(true) {
            self.eval(car)
        } else if cond == Value::Bool(false) {
            self.eval(cdr)
        } else {
            Err(Error::TypeErr)
        }
    }

    fn include(&mut self, expr: AstPtr) -> Result<Value> {
        let mut args = expr;

        while args != NIL {
            let (arg, more) = self.expect_cons(args)?;
            let id = self.eval(arg)?.to_str()?;
            self._include(id)?;
            args = more;
        }

        Ok(Value::Nil)
    }

    fn lambda(&mut self, expr: AstPtr) -> Result<Value> {
        let (mut args, body) = self.expect_cons(expr)?;
        let mut argv = Vec::new();

        while args != NIL {
            let (arg, more) = self.expect_cons(args)?;
            let id = self.expect_ident(arg)?;
            argv.push(id);
            args = more;
        }

        self.env.capture = true;
        self.lambdas
            .push(Lambda(self.env.current, argv.into_boxed_slice(), body));
        Ok(Value::Lambda((self.lambdas.len() - 1) as u32))
    }

    fn let_(&mut self, expr: AstPtr) -> Result<Value> {
        let (mut args, body) = self.expect_cons(expr)?;
        let new_env = self.env.create(self.env.current);

        while args != NIL {
            let (arg, more) = self.expect_cons(args)?;
            let (car, cdr) = self.expect_cons(arg)?;
            let id = self.expect_ident(car)?;
            let val = self.eval(cdr)?;
            self.env.extend(new_env, id, val);
            args = more;
        }

        self.eval_body(body, new_env)
    }

    fn log_str(&mut self, s: &str) {
        let id = self.parser.add_str(s);
        self.log_str_id(id);
    }

    fn log_str_id(&mut self, id: StrId) {
        if let Some(count) = self.log.get_mut(&id) {
            *count += 1;
        } else {
            self.log.insert(id, 1);
        }
    }

    fn _log(&mut self, expr: AstPtr, fmt: char) -> Result<Value> {
        let (val, nil) = self.expect_arg(expr)?;
        self.expect_nil(nil)?;

        match (val, fmt) {
            (Value::Str(id), _) => self.log_str_id(id),
            (val, 'x') => self.log_str(&format!("{:#x}", val)),
            (val, 'b') => self.log_str(&format!("{:#b}", val)),
            (val, _) => self.log_str(&val.to_string()),
        }

        Ok(Value::Nil)
    }

    fn log(&mut self, expr: AstPtr) -> Result<Value> {
        self._log(expr, 'd')
    }

    fn logx(&mut self, expr: AstPtr) -> Result<Value> {
        self._log(expr, 'x')
    }

    fn logb(&mut self, expr: AstPtr) -> Result<Value> {
        self._log(expr, 'b')
    }

    fn bitnot(&mut self, expr: AstPtr) -> Result<Value> {
        let (lhs, next) = self.expect_u32(expr)?;
        self.expect_nil(next)?;
        Ok(Value::U32(!lhs))
    }

    fn quote(&mut self, expr: AstPtr) -> Result<Value> {
        match self.parser.get(expr) {
            Expr::Atom(v @ Value::U32(_)) => Ok(v),
            Expr::Atom(v @ Value::I32(_)) => Ok(v),
            Expr::Atom(v @ Value::F32(_)) => Ok(v),
            Expr::Atom(v @ Value::Str(_)) => Ok(v),
            Expr::Atom(v @ Value::Nil) => Ok(v),
            _ => Ok(Value::Quote(expr)),
        }
    }

    fn repeat(&mut self, times: i32, expr: AstPtr) -> Result<Value> {
        let mut retval = Value::Nil;

        for _ in 0..times.abs() {
            retval = self.eval_body(expr, self.env.current)?;
        }

        Ok(retval)
    }

    fn set(&mut self, expr: AstPtr) -> Result<Value> {
        let (car, cdr) = self.expect_cons(expr)?;
        let id = self.expect_ident(car)?;
        let (expr, nil) = self.expect_cons(cdr)?;
        self.expect_nil(nil)?;
        let val = self.eval(expr)?;

        if self.env.set(id, val) {
            Ok(Value::Nil)
        } else {
            Err(Error::Undefined)
        }
    }

    fn note_off(&mut self, next: AstPtr) -> Result<Value> {
        let (chn, next) = self.expect_u7(next)?;
        let (num, next) = self.expect_u7(next)?;
        let (vel, next) = self.expect_u7(next)?;
        self.expect_nil(next)?;
        self.add_event(Event::NoteOff(chn, num, vel))
    }

    fn note_on(&mut self, next: AstPtr) -> Result<Value> {
        let (chn, next) = self.expect_u7(next)?;
        let (num, next) = self.expect_u7(next)?;
        let (vel, next) = self.expect_u7(next)?;
        self.expect_nil(next)?;
        self.add_event(Event::NoteOn(chn, num, vel))
    }

    fn control_change(&mut self, next: AstPtr) -> Result<Value> {
        let (chn, next) = self.expect_u7(next)?;
        let (id, next) = self.expect_u7(next)?;
        let (val, next) = self.expect_u7(next)?;
        self.expect_nil(next)?;
        self.add_event(Event::ControlChange(chn, id, val))
    }

    fn program_change(&mut self, next: AstPtr) -> Result<Value> {
        let (chn, next) = self.expect_u7(next)?;
        let (prg, next) = self.expect_u7(next)?;
        self.expect_nil(next)?;
        self.add_event(Event::ProgramChange(chn, prg))
    }

    fn set_tempo(&mut self, next: AstPtr) -> Result<Value> {
        let (tempo, next) = self.expect_u32(next)?;
        self.expect_nil(next)?;
        self.add_event(Event::SetTempo(tempo))
    }

    fn time_signature(&mut self, next: AstPtr) -> Result<Value> {
        let (nn, next) = self.expect_u7(next)?;
        let (dd, next) = self.expect_u7(next)?;
        let (cc, next) = self.expect_u7(next)?;
        let (bb, next) = self.expect_u7(next)?;
        self.expect_nil(next)?;
        self.add_event(Event::TimeSignature(nn, dd, cc, bb))
    }

    fn put_event(&mut self, next: AstPtr) -> Result<Value> {
        let (trk, next) = self.expect_u32(next)?;
        let (dur, next) = self.expect_u32(next)?;
        let (evn, next) = self.expect_midi(next)?;
        self.expect_nil(next)?;
        let event = self.get_event(evn);
        self.get_track(trk).put_event(dur, event)?;
        Ok(Value::Nil)
    }

    fn add_event(&mut self, e: Event) -> Result<Value> {
        Ok(Value::Midi(self.events.insert_full(e).0 as u32))
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn _include(&mut self, id: StrId) -> Result<()> {
        use std::env;
        use std::fs;

        let mut path: PathBuf = env::var_os("MIDILISP_INCLUDE_PATH")
            .map(PathBuf::from)
            .unwrap_or_else(|| PathBuf::from(INCLUDE_PATH));

        path.push(self.parser.get_str(id));

        if !path.exists() {
            path.set_extension("midilisp");
        }

        if !path.is_file() {
            return Err(Error::InvalidPath);
        }

        path.canonicalize().map_err(|_| Error::InvalidPath)?;

        if !self.paths.contains(&path) {
            let src = fs::read_to_string(&path).map_err(|_| Error::IO)?;
            self.c_path = Some(self.paths.len());
            self.paths.push(path);
            let exprs = self.parser.parse(&src)?;

            for expr in exprs.iter() {
                self.eval(*expr)?;
            }

            self.c_path = None;
        }

        Ok(())
    }

    #[cfg(target_arch = "wasm32")]
    fn _include(&mut self, id: StrId) -> Result<()> {
        use crate::wasm;
        let s = self.parser.get_str(id);
        let name = s.strip_suffix(".midilisp").unwrap_or(s);

        let src = match name {
            "default" => wasm::DEFAULT,
            _ => return Err(Error::InvalidPath),
        };

        let path = PathBuf::from(name);

        if !self.paths.contains(&path) {
            let exprs = self.parser.parse(&src)?;

            for expr in exprs.iter() {
                self.eval(*expr)?;
            }

            self.paths.push(path);
        }

        Ok(())
    }

    fn _define(&mut self, s: &'static str, val: Value) {
        let id = self.parser.add_str(s);
        self.env.extend(0, id, val);
    }

    fn _builtin(&mut self, s: &'static str, f: fn(&mut Self, AstPtr) -> Result<Value>) {
        self.builtins.push(f);
        self._define(s, Value::Builtin((self.builtins.len() - 1) as u32));
    }

    fn _define_builtins(&mut self) {
        self._define("fmt", Value::U32(0));
        self._define("div", Value::U32(96));
        self._define("true", Value::Bool(true));
        self._define("false", Value::Bool(false));
        self._builtin("+", |i, e| i.var_op(e, Value::add));
        self._builtin("-", |i, e| i.var_op(e, Value::sub));
        self._builtin("*", |i, e| i.var_op(e, Value::mul));
        self._builtin("/", |i, e| i.var_op(e, Value::div));
        self._builtin("%", |i, e| i.var_op(e, Value::rem));
        self._builtin("**", |i, e| i.var_op(e, Value::pow));
        self._builtin("<<", |i, e| i.var_op(e, Value::shl));
        self._builtin(">>", |i, e| i.var_op(e, Value::shr));
        self._builtin("&", |i, e| i.bin_op(e, Value::bitand));
        self._builtin("|", |i, e| i.bin_op(e, Value::bitor));
        self._builtin("^", |i, e| i.bin_op(e, Value::bitxor));
        self._builtin("&&", |i, e| i.bin_op(e, Value::and));
        self._builtin("||", |i, e| i.bin_op(e, Value::or));
        self._builtin("=", |i, e| i.bin_op(e, Value::eq));
        self._builtin("!=", |i, e| i.bin_op(e, Value::ne));
        self._builtin(">", |i, e| i.bin_op(e, Value::gt));
        self._builtin("<", |i, e| i.bin_op(e, Value::lt));
        self._builtin(">=", |i, e| i.bin_op(e, Value::ge));
        self._builtin("<=", |i, e| i.bin_op(e, Value::le));
        self._builtin("!", |i, e| i.un_op(e, Value::not));
        self._builtin("abs", |i, e| i.un_op(e, Value::abs));
        self._builtin("neg", |i, e| i.un_op(e, Value::neg));
        self._builtin("assert", Self::assert);
        self._builtin("car", Self::car);
        self._builtin("cdr", Self::cdr);
        self._builtin("quote", Self::quote);
        self._builtin("'", Self::quote);
        self._builtin("if", Self::if_);
        self._builtin("define", Self::define);
        self._builtin("set", Self::set);
        self._builtin("let", Self::let_);
        self._builtin("log", Self::log);
        self._builtin("logx", Self::logx);
        self._builtin("logb", Self::logb);
        self._builtin("include", Self::include);
        self._builtin("adv-clock", Self::adv_clock);
        self._builtin("lambda", Self::lambda);
        self._builtin("note-off", Self::note_off);
        self._builtin("note-on", Self::note_on);
        self._builtin("control-change", Self::control_change);
        self._builtin("program-change", Self::program_change);
        self._builtin("set-tempo", Self::set_tempo);
        self._builtin("time-signature", Self::time_signature);
        self._builtin("put-event", Self::put_event);
    }
}

#[cfg(test)]
mod tests {
    use super::{Interpreter, Value};
    use crate::error::Error;

    #[test]
    fn assert() {
        let src = "(assert true)
                   (assert (&& true true))
                   (assert (|| false false))";
        let mut itp = Interpreter::new();
        let exprs = itp.parser.parse(src).unwrap();
        assert_eq!(Value::Nil, itp.eval(exprs[0]).unwrap());
        assert_eq!(Value::Nil, itp.eval(exprs[1]).unwrap());
        assert_eq!(Error::Assert, itp.eval(exprs[2]).unwrap_err());
    }

    #[test]
    fn arithmetic() {
        let src = "(+ (+ 1 (+ 4 3)) (+ (+ 4 5 7) 96))
                   (- (- 100 2) (- (- 20 10 3) 3))
                   (* (* 2 (* 2 3)) (* 4 5))
                   (/ (/ 64 2) (/ 8 4))
                   (% (% 10 6) (% 10 7))
                   (** (** 2 2) (** 2 3))
                   (abs -0)
                   (abs -6)
                   (abs -12.5)
                   (neg 6.0)
                   (neg -12)";
        let mut itp = Interpreter::new();
        let exprs = itp.parser.parse(src).unwrap();
        assert_eq!(Value::I32(120), itp.eval(exprs[0]).unwrap());
        assert_eq!(Value::I32(94), itp.eval(exprs[1]).unwrap());
        assert_eq!(Value::I32(240), itp.eval(exprs[2]).unwrap());
        assert_eq!(Value::I32(16), itp.eval(exprs[3]).unwrap());
        assert_eq!(Value::I32(1), itp.eval(exprs[4]).unwrap());
        assert_eq!(Value::I32(65536), itp.eval(exprs[5]).unwrap());
        assert_eq!(Value::I32(0), itp.eval(exprs[6]).unwrap());
        assert_eq!(Value::I32(6), itp.eval(exprs[7]).unwrap());
        assert_eq!(Value::F32(12.5), itp.eval(exprs[8]).unwrap());
        assert_eq!(Value::F32(-6.0), itp.eval(exprs[9]).unwrap());
        assert_eq!(Value::I32(12), itp.eval(exprs[10]).unwrap());
    }

    #[test]
    fn bitwise() {
        let src = "(<< (<< 8 4) (<< 1 3))
                   (>> (>> 80040 3) (>> 3 1))
                   (& (& 16 17) (& 59 24))
                   (| (| 8 16) (| 4 32))
                   (^ (^ 8 17) (^ 5 27))
                   (! (! (! 0xffff)))";
        let mut itp = Interpreter::new();
        let exprs = itp.parser.parse(src).unwrap();
        assert_eq!(Value::I32(32768), itp.eval(exprs[0]).unwrap());
        assert_eq!(Value::I32(5002), itp.eval(exprs[1]).unwrap());
        assert_eq!(Value::I32(16), itp.eval(exprs[2]).unwrap());
        assert_eq!(Value::I32(60), itp.eval(exprs[3]).unwrap());
        assert_eq!(Value::I32(7), itp.eval(exprs[4]).unwrap());
        assert_eq!(Value::U32(0xffff0000), itp.eval(exprs[5]).unwrap());
    }

    #[test]
    fn cmp() {
        let src = "(= 1 1)
                   (!= 1 0)
                   (> 1 0)
                   (< 0 1)
                   (>= 99 99)
                   (<= 98 99)";
        let mut itp = Interpreter::new();
        let exprs = itp.parser.parse(src).unwrap();
        assert_eq!(Value::Bool(true), itp.eval(exprs[0]).unwrap());
        assert_eq!(Value::Bool(true), itp.eval(exprs[1]).unwrap());
        assert_eq!(Value::Bool(true), itp.eval(exprs[2]).unwrap());
        assert_eq!(Value::Bool(true), itp.eval(exprs[3]).unwrap());
        assert_eq!(Value::Bool(true), itp.eval(exprs[4]).unwrap());
        assert_eq!(Value::Bool(true), itp.eval(exprs[5]).unwrap());
    }

    #[test]
    fn globals() {
        let src = "(define zero 1)
                   (define one 2)
                   (define two (- 5 one))
                   (define three (/ 12 3))
                   (set zero (- zero 1))
                   (set one (- one 1))
                   (set two (- two 1))
                   (set three (- three 1))
                   zero
                   one
                   two
                   three";
        let mut itp = Interpreter::new();
        let exprs = itp.parser.parse(src).unwrap();
        for n in 0..8_usize {
            itp.eval(exprs[n]).unwrap();
        }
        assert_eq!(Value::I32(0), itp.eval(exprs[8]).unwrap());
        assert_eq!(Value::I32(1), itp.eval(exprs[9]).unwrap());
        assert_eq!(Value::I32(2), itp.eval(exprs[10]).unwrap());
        assert_eq!(Value::I32(3), itp.eval(exprs[11]).unwrap());
    }

    #[test]
    fn boolean() {
        let src = "(&& true true)
                   (&& true false)
                   (|| false false)
                   (|| true false)
                   (! true)
                   (! false)";
        let mut itp = Interpreter::new();
        let exprs = itp.parser.parse(src).unwrap();
        assert_eq!(Value::Bool(true), itp.eval(exprs[0]).unwrap());
        assert_eq!(Value::Bool(false), itp.eval(exprs[1]).unwrap());
        assert_eq!(Value::Bool(false), itp.eval(exprs[2]).unwrap());
        assert_eq!(Value::Bool(true), itp.eval(exprs[3]).unwrap());
        assert_eq!(Value::Bool(false), itp.eval(exprs[4]).unwrap());
        assert_eq!(Value::Bool(true), itp.eval(exprs[5]).unwrap());
    }

    #[test]
    fn if_() {
        let src = "(if true 10 99)
                   (if false 10 99)
                   (if true (+ 1 9) (+ 90 9))
                   (if false (+ 1 9) (+ 90 9))
                   (if true (+ 10 10))
                   (if false (+ 10 10))";
        let mut itp = Interpreter::new();
        let exprs = itp.parser.parse(src).unwrap();
        assert_eq!(Value::I32(10), itp.eval(exprs[0]).unwrap());
        assert_eq!(Value::I32(99), itp.eval(exprs[1]).unwrap());
        assert_eq!(Value::I32(10), itp.eval(exprs[2]).unwrap());
        assert_eq!(Value::I32(99), itp.eval(exprs[3]).unwrap());
        assert_eq!(Value::I32(20), itp.eval(exprs[4]).unwrap());
        assert_eq!(Value::Nil, itp.eval(exprs[5]).unwrap());
    }

    #[test]
    fn lambdas() {
        let src = "(define foo
                       (lambda (a b c)
                           (+ a (- b c))))
                   (define bar
                       (lambda ()
                           (foo 2 7 3)))
                   (define baz
                       (lambda (a)
                           (+ (foo a 6 3) (bar))))
                   (define fact
                       (lambda (acc n)
                           (if (= n 0)
                               acc
                               (fact (* acc n) (- n 1)))))
                   (foo 2 6 3)
                   (bar)
                   (baz 2)
                   (fact 1 12)";
        let mut itp = Interpreter::new();
        let exprs = itp.parser.parse(src).unwrap();
        for n in 0..4_usize {
            itp.eval(exprs[n]).unwrap();
        }
        assert_eq!(Value::I32(5), itp.eval(exprs[4]).unwrap());
        assert_eq!(Value::I32(6), itp.eval(exprs[5]).unwrap());
        assert_eq!(Value::I32(11), itp.eval(exprs[6]).unwrap());
        assert_eq!(Value::I32(479001600), itp.eval(exprs[7]).unwrap());
    }

    #[test]
    fn scoping() {
        let src = "(define foo
                       (lambda (a b c)
                           (lambda ()
                               (set a (+ a 1))
                               (+ a b c))))
                   (define bar (foo 1 2 3))
                   (define baz
                       ((lambda ()
                           (let ((a 1) (b 2) (c 33))
                               (lambda () (+ a b c))))))
                   (define qux
                       ((lambda ()
                           (let ((a 1) (b 2) (c 23))
                               (lambda ()
                                   (set a (+ a 1))
                                   (+ a b c))))))
                   (define xyz
                       (let ((a 1))
                           (lambda (b) 
                               (set a (+ a b)) a)))
                   (define asd
                       (let ((a 1))
                           (define b 99)
                           (lambda () (set b (+ b 1)) b)))
                   (bar)
                   (bar)
                   (baz)
                   (qux)
                   (qux)
                   (xyz 1)
                   (xyz 2)
                   (asd)
                   (asd)";
        let mut itp = Interpreter::new();
        let exprs = itp.parser.parse(src).unwrap();
        for n in 0..6_usize {
            itp.eval(exprs[n]).unwrap();
        }
        assert_eq!(Value::I32(7), itp.eval(exprs[6]).unwrap());
        assert_eq!(Value::I32(8), itp.eval(exprs[7]).unwrap());
        assert_eq!(Value::I32(36), itp.eval(exprs[8]).unwrap());
        assert_eq!(Value::I32(27), itp.eval(exprs[9]).unwrap());
        assert_eq!(Value::I32(28), itp.eval(exprs[10]).unwrap());
        assert_eq!(Value::I32(2), itp.eval(exprs[11]).unwrap());
        assert_eq!(Value::I32(4), itp.eval(exprs[12]).unwrap());
        assert_eq!(Value::I32(100), itp.eval(exprs[13]).unwrap());
        assert_eq!(Value::I32(101), itp.eval(exprs[14]).unwrap());
    }

    #[test]
    fn lists() {
        let src = "(define a (quote 1 2 3 4))
                   (define b (' (1 2) (3 4)))
                   (car a)
                   (car (cdr a))
                   (car (cdr (cdr a)))
                   (car (car b))
                   (car (cdr (car (cdr b))))";
        let mut itp = Interpreter::new();
        let exprs = itp.parser.parse(src).unwrap();
        itp.eval(exprs[0]).unwrap();
        itp.eval(exprs[1]).unwrap();
        assert_eq!(Value::I32(1), itp.eval(exprs[2]).unwrap());
        assert_eq!(Value::I32(2), itp.eval(exprs[3]).unwrap());
        assert_eq!(Value::I32(3), itp.eval(exprs[4]).unwrap());
        assert_eq!(Value::I32(1), itp.eval(exprs[5]).unwrap());
        assert_eq!(Value::I32(4), itp.eval(exprs[6]).unwrap());
    }
}
