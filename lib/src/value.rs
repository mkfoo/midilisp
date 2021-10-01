use crate::{
    error::{Error, Result},
    parser::{AstPtr, StrId},
};
use std::convert::TryInto;
use std::fmt;
use std::ops::{Add, Div, Mul, Rem, Sub};
use std::str::FromStr;
use Value::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Bool(bool),
    Builtin(u32),
    F32(f32),
    I32(i32),
    Ident(StrId),
    Lambda(u32),
    Midi(u32),
    Nil,
    Quote(AstPtr),
    Str(StrId),
    U32(u32),
}

impl Value {
    pub fn parse_unsigned(s: &str) -> Result<Self> {
        u32::from_str(s).map(Self::U32).map_err(|_| Error::ParseNum)
    }

    pub fn parse_signed(s: &str) -> Result<Self> {
        i32::from_str(s).map(Self::I32).map_err(|_| Error::ParseNum)
    }

    pub fn parse_float(s: &str) -> Result<Self> {
        f32::from_str(s).map(Self::F32).map_err(|_| Error::ParseNum)
    }

    pub fn parse_hex(s: &str) -> Result<Self> {
        u32::from_str_radix(s, 16)
            .map(Self::U32)
            .map_err(|_| Error::ParseNum)
    }

    pub fn parse_bin(s: &str) -> Result<Self> {
        u32::from_str_radix(s, 2)
            .map(Self::U32)
            .map_err(|_| Error::ParseNum)
    }

    pub fn to_u32(self) -> Result<u32> {
        match self {
            Self::U32(v) => Ok(v),
            Self::I32(v) => v.try_into().map_err(|_| Error::Convert),
            Self::F32(v) => Ok(v as u32),
            Self::Nil => Err(Error::NilArgument),
            _ => Err(Error::NotANumber),
        }
    }

    pub fn to_i32(self) -> Result<Value> {
        match self {
            v @ Self::I32(_) => Ok(v),
            Self::U32(v) => v.try_into().map(Self::I32).map_err(|_| Error::Convert),
            Self::F32(v) => Ok(Self::I32(v as i32)),
            Self::Nil => Err(Error::NilArgument),
            _ => Err(Error::NotANumber),
        }
    }

    pub fn to_f32(self) -> Result<Value> {
        match self {
            v @ Self::F32(_) => Ok(v),
            Self::U32(v) => Ok(Self::F32(v as f32)),
            Self::I32(v) => Ok(Self::F32(v as f32)),
            Self::Nil => Err(Error::NilArgument),
            _ => Err(Error::NotANumber),
        }
    }

    pub fn to_midi(self) -> Result<u32> {
        match self {
            Self::Midi(e) => Ok(e),
            Self::Nil => Err(Error::NilArgument),
            _ => Err(Error::NotMidi),
        }
    }

    pub fn to_u7(self) -> Result<u8> {
        match self {
            Self::U32(i) => Ok((i & 0x7f) as u8),
            Self::Nil => Err(Error::NilArgument),
            _ => Err(Error::NotANumber),
        }
    }

    pub fn to_u16(self) -> Result<u16> {
        match self {
            Self::U32(i) => Ok((i & 0xffff) as u16),
            Self::Nil => Err(Error::NilArgument),
            _ => Err(Error::NotANumber),
        }
    }

    pub fn convert(self, other: Self) -> Result<(Self, Self)> {
        Ok(match (self, other) {
            (I32(_), U32(_)) => (self, other.to_i32()?),
            (U32(_), I32(_)) => (self.to_i32()?, other),
            (F32(_), rhs) => (self, rhs.to_f32()?),
            (lhs, F32(_)) => (lhs.to_f32()?, other),
            _ => (self, other),
        })
    }

    pub fn add(self, other: Self) -> Result<Self> {
        match self.convert(other)? {
            (I32(lhs), I32(rhs)) => lhs.checked_add(rhs).map(I32),
            (U32(lhs), U32(rhs)) => lhs.checked_add(rhs).map(U32),
            (F32(lhs), F32(rhs)) => Some(lhs.add(rhs)).map(F32),
            _ => return Err(Error::TypeErr),
        }
        .ok_or(Error::Overflow)
    }

    pub fn sub(self, other: Self) -> Result<Self> {
        match self.convert(other)? {
            (I32(lhs), I32(rhs)) => lhs.checked_sub(rhs).map(I32),
            (U32(lhs), U32(rhs)) => lhs.checked_sub(rhs).map(U32),
            (F32(lhs), F32(rhs)) => Some(lhs.sub(rhs)).map(F32),
            _ => return Err(Error::TypeErr),
        }
        .ok_or(Error::Overflow)
    }

    pub fn mul(self, other: Self) -> Result<Self> {
        match self.convert(other)? {
            (I32(lhs), I32(rhs)) => lhs.checked_mul(rhs).map(I32),
            (U32(lhs), U32(rhs)) => lhs.checked_mul(rhs).map(U32),
            (F32(lhs), F32(rhs)) => Some(lhs.mul(rhs)).map(F32),
            _ => return Err(Error::TypeErr),
        }
        .ok_or(Error::Overflow)
    }

    pub fn div(self, other: Self) -> Result<Self> {
        match self.convert(other)? {
            (I32(lhs), I32(rhs)) => lhs.checked_div(rhs).map(I32),
            (U32(lhs), U32(rhs)) => lhs.checked_div(rhs).map(U32),
            (F32(lhs), F32(rhs)) => Some(lhs.div(rhs)).map(F32),
            _ => return Err(Error::TypeErr),
        }
        .ok_or(Error::DivideByZero)
    }

    pub fn rem(self, other: Self) -> Result<Self> {
        match self.convert(other)? {
            (I32(lhs), I32(rhs)) => lhs.checked_rem(rhs).map(I32),
            (U32(lhs), U32(rhs)) => lhs.checked_rem(rhs).map(U32),
            (F32(lhs), F32(rhs)) => Some(lhs.rem(rhs)).map(F32),
            _ => return Err(Error::TypeErr),
        }
        .ok_or(Error::DivideByZero)
    }

    pub fn pow(self, other: Self) -> Result<Self> {
        match self.convert(other)? {
            (I32(lhs), I32(rhs)) => lhs
                .checked_pow(rhs.try_into().map_err(|_| Error::Convert)?)
                .map(I32),
            (U32(lhs), U32(rhs)) => lhs.checked_pow(rhs).map(U32),
            (F32(lhs), F32(rhs)) => Some(lhs.powf(rhs)).map(F32),
            _ => return Err(Error::TypeErr),
        }
        .ok_or(Error::Overflow)
    }

    pub fn shl(self, other: Self) -> Result<Self> {
        match self.convert(other)? {
            (I32(lhs), I32(rhs)) => lhs
                .checked_shl(rhs.try_into().map_err(|_| Error::Convert)?)
                .map(I32),
            (U32(lhs), U32(rhs)) => lhs.checked_shl(rhs).map(U32),
            _ => return Err(Error::TypeErr),
        }
        .ok_or(Error::Overflow)
    }

    pub fn shr(self, other: Self) -> Result<Self> {
        match self.convert(other)? {
            (I32(lhs), I32(rhs)) => lhs
                .checked_shr(rhs.try_into().map_err(|_| Error::Convert)?)
                .map(I32),
            (U32(lhs), U32(rhs)) => lhs.checked_shr(rhs).map(U32),
            _ => return Err(Error::TypeErr),
        }
        .ok_or(Error::Overflow)
    }

    pub fn bitand(self, other: Self) -> Result<Self> {
        match self.convert(other)? {
            (I32(lhs), I32(rhs)) => Ok(I32(lhs & rhs)),
            (U32(lhs), U32(rhs)) => Ok(U32(lhs & rhs)),
            _ => Err(Error::TypeErr),
        }
    }

    pub fn bitor(self, other: Self) -> Result<Self> {
        match self.convert(other)? {
            (I32(lhs), I32(rhs)) => Ok(I32(lhs | rhs)),
            (U32(lhs), U32(rhs)) => Ok(U32(lhs | rhs)),
            _ => Err(Error::TypeErr),
        }
    }

    pub fn bitxor(self, other: Self) -> Result<Self> {
        match self.convert(other)? {
            (I32(lhs), I32(rhs)) => Ok(I32(lhs ^ rhs)),
            (U32(lhs), U32(rhs)) => Ok(U32(lhs ^ rhs)),
            _ => Err(Error::TypeErr),
        }
    }

    pub fn and(self, other: Self) -> Result<Self> {
        match self.convert(other)? {
            (Bool(true), Bool(true)) => Ok(Bool(true)),
            (Bool(_), Bool(_)) => Ok(Bool(false)),
            _ => Err(Error::TypeErr),
        }
    }

    pub fn or(self, other: Self) -> Result<Self> {
        match self.convert(other)? {
            (Bool(true), Bool(_)) => Ok(Bool(true)),
            (Bool(_), Bool(true)) => Ok(Bool(true)),
            (Bool(_), Bool(_)) => Ok(Bool(false)),
            _ => Err(Error::TypeErr),
        }
    }

    pub fn eq(self, other: Self) -> Result<Self> {
        match self.convert(other)? {
            (lhs, rhs) if lhs == rhs => Ok(Bool(true)),
            _ => Ok(Bool(false)),
        }
    }

    pub fn ne(self, other: Self) -> Result<Self> {
        match self.convert(other)? {
            (lhs, rhs) if lhs != rhs => Ok(Bool(true)),
            _ => Ok(Bool(false)),
        }
    }

    pub fn gt(self, other: Self) -> Result<Self> {
        match self.convert(other)? {
            (I32(lhs), I32(rhs)) => Ok(Bool(lhs > rhs)),
            (U32(lhs), U32(rhs)) => Ok(Bool(lhs > rhs)),
            (F32(lhs), F32(rhs)) => Ok(Bool(lhs > rhs)),
            _ => Err(Error::TypeErr),
        }
    }

    pub fn lt(self, other: Self) -> Result<Self> {
        match self.convert(other)? {
            (I32(lhs), I32(rhs)) => Ok(Bool(lhs < rhs)),
            (U32(lhs), U32(rhs)) => Ok(Bool(lhs < rhs)),
            (F32(lhs), F32(rhs)) => Ok(Bool(lhs < rhs)),
            _ => Err(Error::TypeErr),
        }
    }

    pub fn ge(self, other: Self) -> Result<Self> {
        match self.convert(other)? {
            (I32(lhs), I32(rhs)) => Ok(Bool(lhs >= rhs)),
            (U32(lhs), U32(rhs)) => Ok(Bool(lhs >= rhs)),
            (F32(lhs), F32(rhs)) => Ok(Bool(lhs >= rhs)),
            _ => Err(Error::TypeErr),
        }
    }

    pub fn le(self, other: Self) -> Result<Self> {
        match self.convert(other)? {
            (I32(lhs), I32(rhs)) => Ok(Bool(lhs <= rhs)),
            (U32(lhs), U32(rhs)) => Ok(Bool(lhs <= rhs)),
            (F32(lhs), F32(rhs)) => Ok(Bool(lhs <= rhs)),
            _ => Err(Error::TypeErr),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{}", b),
            Self::Builtin(_) => write!(f, "<builtin>"),
            Self::Ident(_) => write!(f, "<ident>"),
            Self::Lambda(_) => write!(f, "<lambda>"),
            Self::Midi(_) => write!(f, "<midi event>"),
            Self::Nil => write!(f, "()"),
            Self::Quote(_) => write!(f, "<quote>"),
            Self::Str(_) => write!(f, "<str>"),
            Self::U32(n) => write!(f, "{}", n),
            Self::I32(n) => write!(f, "{}", n),
            Self::F32(n) => write!(f, "{}", n),
        }
    }
}
