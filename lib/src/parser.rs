use crate::{
    error::{Error, Result},
    lexer::{Lexer, Token},
    value::Value,
};
use fnv::FnvBuildHasher;
use indexmap::IndexSet;
use smol_str::SmolStr;
use Token::*;

use std::cmp::PartialEq;

type FnvIndexSet<T> = IndexSet<T, FnvBuildHasher>;
pub type AstPtr = u32;
pub type StrId = u32;
pub const NIL: AstPtr = 0;
const LPAR: AstPtr = 1;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Expr {
    Atom(Value),
    Cons(AstPtr, AstPtr),
}

pub struct Parser {
    ast: Vec<Expr>,
    stack: Vec<AstPtr>,
    strings: FnvIndexSet<SmolStr>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            ast: vec![Expr::Atom(Value::Nil), Expr::Atom(Value::Nil)],
            stack: Vec::new(),
            strings: Default::default(),
        }
    }

    fn push_expr(&mut self, e: Expr) -> AstPtr {
        self.ast.push(e);
        (self.ast.len() - 1) as AstPtr
    }

    pub fn get(&self, ptr: AstPtr) -> Expr {
        *self.ast.get(ptr as usize).expect("BUG: invalid ast ptr")
    }

    pub fn add_str(&mut self, s: &str) -> StrId {
        self.strings.insert_full(SmolStr::new(s)).0 as StrId
    }

    pub fn get_str(&self, id: StrId) -> &str {
        self.strings
            .get_index(id as usize)
            .map(|s| s.as_str())
            .expect("BUG: invalid str id")
    }

    pub fn parse(&mut self, src: &str) -> Result<Vec<AstPtr>> {
        let mut lex = Lexer::new(src);
        let mut exprs = Vec::new();

        while let Some(tok) = lex.next() {
            match tok {
                LeftParen => exprs.push(self.cons(&mut lex)?),
                RightParen => return Err(Error::InvalidParen),
                _ => exprs.push(self.atom(tok, &lex)?),
            }
        }

        Ok(exprs)
    }

    fn cons(&mut self, lex: &mut Lexer) -> Result<AstPtr> {
        assert!(self.stack.is_empty());
        self.stack.push(LPAR);

        while self.stack[0] == LPAR {
            match lex.next() {
                Some(LeftParen) => self.stack.push(LPAR),
                Some(RightParen) => {
                    let cons = self.fold()?;
                    self.stack.push(cons);
                }
                Some(tok) => {
                    let a = self.atom(tok, lex)?;
                    self.stack.push(a);
                }
                None => return Err(Error::UnclosedParen),
            }
        }

        assert_eq!(self.stack.len(), 1);
        Ok(self.stack.pop().unwrap())
    }

    fn fold(&mut self) -> Result<AstPtr> {
        let mut cdr = NIL;

        loop {
            match self.stack.pop() {
                Some(LPAR) => return Ok(cdr),
                Some(car) => cdr = self.push_expr(Expr::Cons(car, cdr)),
                None => return Err(Error::InvalidParen),
            }
        }
    }

    fn atom(&mut self, tok: Token, lex: &Lexer) -> Result<AstPtr> {
        let s = lex.get_str();

        let val = match tok {
            Ident => Value::Ident(self.add_str(s)),
            Str => Value::Str(self.add_str(s)),
            Uint => Value::parse_unsigned(s)?,
            Int => Value::parse_signed(s)?,
            Float => Value::parse_float(s)?,
            Hex => Value::parse_hex(s)?,
            Bin => Value::parse_bin(s)?,
            StrError => return Err(Error::UnclosedStr),
            NumError => return Err(Error::ParseNum),
            _ => unreachable!(),
        };

        Ok(self.push_expr(Expr::Atom(val)))
    }
}

#[cfg(test)]
mod tests {
    use super::{AstPtr, Expr, Parser, NIL};
    use crate::error::Error;
    use crate::value::Value;
    use Expr::*;
    use Value::*;

    fn exp_cons(p: &Parser, e: AstPtr) -> (AstPtr, AstPtr) {
        match p.get(e) {
            Cons(car, cdr) => (car, cdr),
            _ => panic!(),
        }
    }

    #[test]
    fn list() {
        let src = "(1 2 3)";
        let mut parser = Parser::new();
        let exprs = parser.parse(src).unwrap();
        let (car, cdr) = exp_cons(&parser, exprs[0]);
        assert_eq!(Atom(U32(1)), parser.get(car));
        let (car, cdr) = exp_cons(&parser, cdr);
        assert_eq!(Atom(U32(2)), parser.get(car));
        let (car, cdr) = exp_cons(&parser, cdr);
        assert_eq!(Atom(U32(3)), parser.get(car));
        assert_eq!(NIL, cdr);
    }

    #[test]
    fn tree() {
        let src = "((1 2) 3 4)";
        let mut parser = Parser::new();
        let exprs = parser.parse(src).unwrap();
        let (car0, cdr0) = exp_cons(&parser, exprs[0]);
        let (one, cdr1) = exp_cons(&parser, car0);
        let (two, nil0) = exp_cons(&parser, cdr1);
        let (three, cdr2) = exp_cons(&parser, cdr0);
        let (four, nil1) = exp_cons(&parser, cdr2);
        assert_eq!(Atom(U32(1)), parser.get(one));
        assert_eq!(Atom(U32(2)), parser.get(two));
        assert_eq!(Atom(U32(3)), parser.get(three));
        assert_eq!(Atom(U32(4)), parser.get(four));
        assert_eq!(NIL, nil0);
        assert_eq!(NIL, nil1);
    }

    #[test]
    fn seq() {
        let src = "(1 2) (3 4)";
        let mut parser = Parser::new();
        let exprs = parser.parse(src).unwrap();
        let (one, cdr) = exp_cons(&parser, exprs[0]);
        assert_eq!(Atom(U32(1)), parser.get(one));
        let (two, nil) = exp_cons(&parser, cdr);
        assert_eq!(Atom(U32(2)), parser.get(two));
        assert_eq!(NIL, nil);
        let (three, cdr) = exp_cons(&parser, exprs[1]);
        assert_eq!(Atom(U32(3)), parser.get(three));
        let (four, nil) = exp_cons(&parser, cdr);
        assert_eq!(Atom(U32(4)), parser.get(four));
        assert_eq!(NIL, nil);
    }

    #[test]
    fn errors() {
        let src1 = "(1 2))(3 4)";
        let src2 = "((1 2 3 4)";
        {
            let mut parser = Parser::new();
            assert_eq!(Error::InvalidParen, parser.parse(src1).unwrap_err());
        }
        {
            let mut parser = Parser::new();
            assert_eq!(Error::UnclosedParen, parser.parse(src2).unwrap_err());
        }
    }
}
