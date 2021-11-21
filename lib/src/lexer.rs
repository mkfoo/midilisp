use std::cmp::PartialEq;
use std::str::CharIndices;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Token {
    Bin,
    Eof,
    Float,
    Hex,
    Ident,
    Int,
    LeftParen,
    NumError,
    RightParen,
    Str,
    StrError,
}

use Token::*;

type LexFn<T> = fn(&mut T) -> Token;

pub struct Lexer<'a> {
    f: LexFn<Self>,
    iter: CharIndices<'a>,
    src: &'a str,
    start: usize,
    end: usize,
    pub line: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Self {
        Self {
            f: Self::begin,
            iter: s.char_indices(),
            src: s,
            start: 0,
            end: 0,
            line: 1,
        }
    }

    pub fn next(&mut self) -> Option<Token> {
        Some((self.f)(self)).filter(|t| t != &Eof)
    }

    pub fn get_str(&self) -> &str {
        &self.src[self.start..self.end]
    }

    fn advance(&mut self) -> Option<char> {
        match self.iter.next() {
            Some((idx, chr)) => {
                if chr == '\n' {
                    self.line += 1;
                }
                self.end = idx;
                Some(chr)
            }
            None => {
                self.end = self.src.len();
                None
            }
        }
    }

    fn begin(&mut self) -> Token {
        let mut opt;

        loop {
            opt = self.advance();
            self.start = self.end;

            match opt {
                Some('0') => break self.zero(),
                Some(c) if c.is_ascii_digit() => break self.int(),
                Some(c) if c.is_whitespace() => continue,
                Some(c) if c.is_control() => continue,
                Some('.') => break self.float(),
                Some('(') => break self.lpar(),
                Some(')') => break self.rpar(),
                Some(';') => break self.comment(),
                Some('"') => break self.string(),
                Some('-') => break self.minus(),
                Some(_) => break self.ident(),
                None => break self.ret(Self::eof, Eof),
            }
        }
    }

    fn default_match(&self, opt: Option<char>) -> Option<LexFn<Self>> {
        match opt {
            Some('(') => Some(Self::lpar),
            Some(')') => Some(Self::rpar),
            Some(';') => Some(Self::comment),
            Some('"') => Some(Self::string),
            Some(c) if c.is_whitespace() => Some(Self::begin),
            Some(c) if c.is_control() => Some(Self::begin),
            Some(_) => None,
            None => Some(Self::eof),
        }
    }

    fn default_ret(&mut self, c: Option<char>, ok: Token, err: Token) -> Token {
        match self.default_match(c) {
            Some(f) => self.ret(f, ok),
            None => self.ret(Self::eof, err),
        }
    }

    fn ret(&mut self, f: LexFn<Self>, tok: Token) -> Token {
        self.f = f;
        tok
    }

    fn skip_to(&mut self, c: char) {
        self.end = self
            .iter
            .find(|t| t.1 == c)
            .map(|t| t.0)
            .unwrap_or(self.src.len());
    }

    fn lpar(&mut self) -> Token {
        self.ret(Self::begin, LeftParen)
    }

    fn rpar(&mut self) -> Token {
        self.ret(Self::begin, RightParen)
    }

    fn comment(&mut self) -> Token {
        self.skip_to('\n');
        self.begin()
    }

    fn string(&mut self) -> Token {
        self.start = self.end + 1;
        self.skip_to('"');

        if self.end == self.src.len() {
            self.ret(Self::eof, StrError)
        } else {
            self.ret(Self::begin, Str)
        }
    }

    fn zero(&mut self) -> Token {
        match self.advance() {
            Some('x') => self.hex(),
            Some('b') => self.bin(),
            Some('.') => self.float(),
            Some(c) if c.is_ascii_digit() => self.int(),
            opt => self.default_ret(opt, Int, NumError),
        }
    }

    fn minus(&mut self) -> Token {
        match self.advance() {
            Some(c) if c.is_ascii_digit() => self.int(),
            opt => match self.default_match(opt) {
                Some(f) => self.ret(f, Ident),
                None => self.ident(),
            },
        }
    }

    fn int(&mut self) -> Token {
        loop {
            match self.advance() {
                Some(c) if c.is_ascii_digit() => continue,
                Some('.') => break self.float(),
                opt => break self.default_ret(opt, Int, NumError),
            }
        }
    }

    fn float(&mut self) -> Token {
        loop {
            match self.advance() {
                Some(c) if c.is_ascii_digit() => continue,
                opt => break self.default_ret(opt, Float, NumError),
            }
        }
    }

    fn hex(&mut self) -> Token {
        self.start += 2;

        loop {
            match self.advance() {
                Some(c) if c.is_ascii_hexdigit() => continue,
                opt => break self.default_ret(opt, Hex, NumError),
            }
        }
    }

    fn bin(&mut self) -> Token {
        self.start += 2;

        loop {
            match self.advance() {
                Some('0') | Some('1') => continue,
                opt => break self.default_ret(opt, Bin, NumError),
            }
        }
    }

    fn ident(&mut self) -> Token {
        let mut opt;

        loop {
            opt = self.advance();

            match self.default_match(opt) {
                None => continue,
                Some(f) => break self.ret(f, Ident),
            }
        }
    }

    fn eof(&mut self) -> Token {
        Eof
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, Token};
    use Token::*;

    fn expect(lex: &mut Lexer, exp_tok: Token) {
        assert_eq!(exp_tok, lex.next().unwrap());
    }

    fn expect_s(lex: &mut Lexer, exp_tok: Token, exp_s: &str) {
        expect(lex, exp_tok);
        assert_eq!(exp_s, lex.get_str());
    }

    #[test]
    fn parens() {
        let src = "())(";
        let mut lex = Lexer::new(src);
        expect(&mut lex, LeftParen);
        expect(&mut lex, RightParen);
        expect(&mut lex, RightParen);
        expect(&mut lex, LeftParen);
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn numbers() {
        let src = "(0) 1 256 0xc0ffee 0b110101 00000001903 -99 0.5 .5 -9.234 123a1";
        let mut lex = Lexer::new(src);
        expect(&mut lex, LeftParen);
        expect_s(&mut lex, Int, "0");
        expect(&mut lex, RightParen);
        expect_s(&mut lex, Int, "1");
        expect_s(&mut lex, Int, "256");
        expect_s(&mut lex, Hex, "c0ffee");
        expect_s(&mut lex, Bin, "110101");
        expect_s(&mut lex, Int, "00000001903");
        expect_s(&mut lex, Int, "-99");
        expect_s(&mut lex, Float, "0.5");
        expect_s(&mut lex, Float, ".5");
        expect_s(&mut lex, Float, "-9.234");
        expect(&mut lex, NumError);
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn comments() {
        let src = ";ignore this
                   123 456 ;and this
                   0xb;and this";
        let mut lex = Lexer::new(src);
        expect_s(&mut lex, Int, "123");
        expect_s(&mut lex, Int, "456");
        expect_s(&mut lex, Hex, "b");
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn idents() {
        let src = "(foobar-01 🍔) + !/#¤=&* - 絵文字 🍓🍐";
        let mut lex = Lexer::new(src);
        expect(&mut lex, LeftParen);
        expect_s(&mut lex, Ident, "foobar-01");
        expect_s(&mut lex, Ident, "🍔");
        expect(&mut lex, RightParen);
        expect_s(&mut lex, Ident, "+");
        expect_s(&mut lex, Ident, "!/#¤=&*");
        expect_s(&mut lex, Ident, "-");
        expect_s(&mut lex, Ident, "絵文字");
        expect_s(&mut lex, Ident, "🍓🍐");
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn strings() {
        let src = "\"\"\"foobar00\"029\"3#8(9;)49\"(\"Quux-029+Z\")\"s*fdkf";
        let mut lex = Lexer::new(src);
        expect_s(&mut lex, Str, "");
        expect_s(&mut lex, Str, "foobar00");
        expect_s(&mut lex, Int, "029");
        expect_s(&mut lex, Str, "3#8(9;)49");
        expect(&mut lex, LeftParen);
        expect_s(&mut lex, Str, "Quux-029+Z");
        expect(&mut lex, RightParen);
        expect(&mut lex, StrError);
        assert_eq!(lex.next(), None);
    }
}
