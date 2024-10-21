use crate::lexer::{Token::{self, *}, Lexer};
use crate::ast::*;

use super::type_only;


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum Terminal {
    RightParen  = 1,
    RightBrack  = 1 << 1,
    RightBrace  = 1 << 2,
    Semicolon   = 1 << 3,
    Colon       = 1 << 4,
    Comma       = 1 << 5,
}


fn is_set(a: u32, b: Terminal) -> bool {
    (a & (b as u32)) != 0
}

fn is_terminal(t: &Token, term: u32) -> bool {
    match t {
        Token::RightParen => is_set(term, Terminal::RightParen),
        Token::RightBrack => is_set(term, Terminal::RightBrack),
        Token::RightBrace => is_set(term, Terminal::RightBrace),
        Token::Semicolon  => is_set(term, Terminal::Semicolon),
        Token::Colon      => is_set(term, Terminal::Colon),
        Token::Comma      => is_set(term, Terminal::Comma),
        _ => false
    }
}


pub fn parse(ts: &mut Lexer, next: Option<Token>, term: u32) -> Option<Expression> {
    let mut next = if let Some(n) = next {
        n
    } else {
        ts.next()?.0
    };

    match next {
        Token::Semicolon  if is_set(term, Terminal::Semicolon)  => None,
        Token::RightParen if is_set(term, Terminal::RightParen) => None,
        _ => parse_as(ts, &mut next, term),
    }
}

pub fn parse_rhs(ts: &mut Lexer, term: u32) -> Option<Expression> {
    let mut next = ts.next()?.0;

    match next {
        Token::Semicolon  if is_set(term, Terminal::Semicolon)  => None,
        Token::RightParen if is_set(term, Terminal::RightParen) => None,
        _ => parse_or(ts, &mut next, term)
    }
}


fn parse_as(ts: &mut Lexer, next: &mut Token, term: u32) -> Option<Expression> {
    let a = parse_or(ts, next, term)?;
    if !matches!(*next, AddEq|SubEq|MulEq|DivEq|ModEq|OrEq|XorEq|AndEq|LShiftEq|RShiftEq|Equal) {
        if is_terminal(next, term) {
            return Some(a)
        } else {
            return None
        }
    }
    let b = parse_rhs(ts, term)?;
    let right = match next {
        Token::AddEq    => Expression::Add   (Box::new(a.clone()), Box::new(b)),
        Token::SubEq    => Expression::Sub   (Box::new(a.clone()), Box::new(b)),
        Token::MulEq    => Expression::Mul   (Box::new(a.clone()), Box::new(b)),
        Token::DivEq    => Expression::Div   (Box::new(a.clone()), Box::new(b)),
        Token::ModEq    => Expression::Mod   (Box::new(a.clone()), Box::new(b)),
        Token::OrEq     => Expression::BitOr (Box::new(a.clone()), Box::new(b)),
        Token::XorEq    => Expression::BitXor(Box::new(a.clone()), Box::new(b)),
        Token::AndEq    => Expression::BitAnd(Box::new(a.clone()), Box::new(b)),
        Token::LShiftEq => Expression::BitShl(Box::new(a.clone()), Box::new(b)),
        Token::RShiftEq => Expression::BitShr(Box::new(a.clone()), Box::new(b)),
        Token::Equal    => b,
        _ => unreachable!()
    };
    Some(Expression::Assign(Box::new(a), Box::new(right)))
}

fn parse_or(ts: &mut Lexer, next: &mut Token, term: u32) -> Option<Expression> {
    let mut a = parse_an(ts, next, term)?;
    while matches!(*next, Token::LogOr) {
        *next = ts.next()?.0;
        let b = parse_an(ts, next, term)?;
        a = Expression::LogOr(Box::new(a), Box::new(b));
    }
    if matches!(*next, Equal|AddEq|SubEq|MulEq|DivEq|ModEq|AndEq|OrEq|XorEq) || is_terminal(next, term) {
        Some(a)
    } else {
        None
    }
}

fn parse_an(ts: &mut Lexer, next: &mut Token, term: u32) -> Option<Expression> {
    let mut a = parse_bo(ts, next, term)?;
    while matches!(*next, Token::LogAnd) {
        *next = ts.next()?.0;
        let b = parse_bo(ts, next, term)?;
        a = Expression::LogAnd(Box::new(a), Box::new(b));
    }
    if matches!(*next, Equal|AddEq|SubEq|MulEq|DivEq|ModEq|AndEq|OrEq|XorEq|LogOr) || is_terminal(next, term) {
        Some(a)
    } else {
        None
    }
}

fn parse_bo(ts: &mut Lexer, next: &mut Token, term: u32) -> Option<Expression> {
    let mut a = parse_bx(ts, next, term)?;
    while matches!(*next, Token::BitOr) {
        *next = ts.next()?.0;
        let b = parse_bx(ts, next, term)?;
        a = Expression::BitOr(Box::new(a), Box::new(b));
    }
    if matches!(*next, Equal|AddEq|SubEq|MulEq|DivEq|ModEq|AndEq|OrEq|XorEq|LogOr|LogAnd) || is_terminal(next, term) {
        Some(a)
    } else {
        None
    }
}

fn parse_bx(ts: &mut Lexer, next: &mut Token, term: u32) -> Option<Expression> {
    let mut a = parse_ba(ts, next, term)?;
    while matches!(*next, Token::BitXor) {
        *next = ts.next()?.0;
        let b = parse_ba(ts, next, term)?;
        a = Expression::BitXor(Box::new(a), Box::new(b));
    }
    if matches!(*next, Equal|AddEq|SubEq|MulEq|DivEq|ModEq|AndEq|OrEq|XorEq|LogOr|LogAnd|BitOr) || is_terminal(next, term) {
        Some(a)
    } else {
        None
    }
}

fn parse_ba(ts: &mut Lexer, next: &mut Token, term: u32) -> Option<Expression> {
    let mut a = parse_eq(ts, next, term)?;
    while matches!(*next, Token::BitAnd) {
        *next = ts.next()?.0;
        let b = parse_eq(ts, next, term)?;
        a = Expression::BitAnd(Box::new(a), Box::new(b));
    }
    if matches!(*next, Equal|AddEq|SubEq|MulEq|DivEq|ModEq|AndEq|OrEq|XorEq|LogOr|LogAnd|BitOr|BitXor) || is_terminal(next, term) {
        Some(a)
    } else {
        None
    }
}

fn parse_eq(ts: &mut Lexer, next: &mut Token, term: u32) -> Option<Expression> {
    let mut a = parse_cm(ts, next, term)?;
    while matches!(*next, Token::EqualEq|Token::BangEq) {
        let n = next.clone();
        *next = ts.next()?.0;
        let b = parse_cm(ts, next, term)?;
        match n {
            Token::EqualEq => a = Expression::Eq(Box::new(a), Box::new(b)),
            Token::BangEq  => a = Expression::Ne(Box::new(a), Box::new(b)),
            _ => unreachable!()
        }
    }
    if matches!(*next, Equal|AddEq|SubEq|MulEq|DivEq|ModEq|AndEq|OrEq|XorEq|LogOr|LogAnd|BitOr|BitXor|BitAnd) || is_terminal(next, term) {
        Some(a)
    } else {
        None
    }
}

fn parse_cm(ts: &mut Lexer, next: &mut Token, term: u32) -> Option<Expression> {
    let mut a = parse_sh(ts, next, term)?;
    while matches!(*next, Token::Less|Token::Greater|Token::LessEq|Token::GreaterEq) {
        let n = next.clone();
        *next = ts.next()?.0;
        let b = parse_sh(ts, next, term)?;
        match n {
            Token::Less      => a = Expression::Lt(Box::new(a), Box::new(b)),
            Token::Greater   => a = Expression::Gt(Box::new(a), Box::new(b)),
            Token::LessEq    => a = Expression::Le(Box::new(a), Box::new(b)),
            Token::GreaterEq => a = Expression::Ge(Box::new(a), Box::new(b)),
            _ => unreachable!()
        }
    }
    if matches!(*next, Equal|AddEq|SubEq|MulEq|DivEq|ModEq|AndEq|OrEq|XorEq|LogOr|LogAnd|BitOr|BitXor|BitAnd|EqualEq|BangEq) || is_terminal(next, term) {
        Some(a)
    } else {
        None
    }
}

fn parse_sh(ts: &mut Lexer, next: &mut Token, term: u32) -> Option<Expression> {
    let mut a = parse_ad(ts, next, term)?;
    while matches!(*next, Token::LShift|Token::RShift) {
        let n = next.clone();
        *next = ts.next()?.0;
        let b = parse_ad(ts, next, term)?;
        match n {
            Token::LShift => a = Expression::BitShl(Box::new(a), Box::new(b)),
            Token::RShift => a = Expression::BitShr(Box::new(a), Box::new(b)),
            _ => unreachable!()
        }
    }
    if matches!(*next, Equal|AddEq|SubEq|MulEq|DivEq|ModEq|AndEq|OrEq|XorEq|LogOr|LogAnd|BitOr|BitXor|BitAnd|EqualEq|BangEq|Less|Greater|LessEq|GreaterEq) || is_terminal(next, term) {
        Some(a)
    } else {
        None
    }
}

fn parse_ad(ts: &mut Lexer, next: &mut Token, term: u32) -> Option<Expression> {
    let mut a = parse_mu(ts, next, term)?;
    while matches!(*next, Token::Add|Token::Sub) {
        let n = next.clone();
        *next = ts.next()?.0;
        let b = parse_mu(ts, next, term)?;
        match n {
            Token::Add => a = Expression::Add(Box::new(a), Box::new(b)),
            Token::Sub => a = Expression::Sub(Box::new(a), Box::new(b)),
            _ => unreachable!()
        }
    }
    if matches!(*next, Equal|AddEq|SubEq|MulEq|DivEq|ModEq|AndEq|OrEq|XorEq|LogOr|LogAnd|BitOr|BitXor|BitAnd|EqualEq|BangEq|Less|Greater|LessEq|GreaterEq|LShift|RShift) || is_terminal(next, term) {
        Some(a)
    } else {
        None
    }
}

fn parse_mu(ts: &mut Lexer, next: &mut Token, term: u32) -> Option<Expression> {
    let mut a = parse_un(ts, next)?;
    while matches!(*next, Token::Mul|Token::Div|Token::Mod) {
        let n = next.clone();
        *next = ts.next()?.0;
        let b = parse_un(ts, next)?;
        match n {
            Token::Mul => a = Expression::Mul(Box::new(a), Box::new(b)),
            Token::Div => a = Expression::Div(Box::new(a), Box::new(b)),
            Token::Mod => a = Expression::Mod(Box::new(a), Box::new(b)),
            _ => unreachable!()
        }
    }
    if matches!(*next, Equal|AddEq|SubEq|MulEq|DivEq|ModEq|AndEq|OrEq|XorEq|LogOr|LogAnd|BitOr|BitXor|BitAnd|EqualEq|BangEq|Less|Greater|LessEq|GreaterEq|LShift|RShift|Add|Sub) || is_terminal(next, term) {
        Some(a)
    } else {
        None
    }
}

fn parse_un(ts: &mut Lexer, next: &mut Token) -> Option<Expression> {
    match next {
        Token::Identifier(id) => {
            let id = std::mem::take(id);
            *next = ts.next()?.0;
            let (mut exp, mut found) = parse_po(ts, next, Expression::Identifier(id))?;
            while found {
                (exp, found) = parse_po(ts, next, exp)?;
            }
            Some(exp)
        }
        Token::Integer(i) => {
            let i = *i;
            *next = ts.next()?.0;
            let (mut exp, mut found) = parse_po(ts, next, Expression::IntLit(i))?;
            while found {
                (exp, found) = parse_po(ts, next, exp)?;
            }
            Some(exp)
        }
        Token::Floating(f) => {
            let f = *f;
            *next = ts.next()?.0;
            let (mut exp, mut found) = parse_po(ts, next, Expression::FltLit(f))?;
            while found {
                (exp, found) = parse_po(ts, next, exp)?;
            }
            Some(exp)
        }
        Token::Boolean(b) => {
            let b = *b;
            *next = ts.next()?.0;
            let (mut exp, mut found) = parse_po(ts, next, Expression::BoolLit(b))?;
            while found {
                (exp, found) = parse_po(ts, next, exp)?;
            }
            Some(exp)
        }
        Token::Character(c) => {
            let c = *c;
            *next = ts.next()?.0;
            let (mut exp, mut found) = parse_po(ts, next, Expression::CharLit(c))?;
            while found {
                (exp, found) = parse_po(ts, next, exp)?;
            }
            Some(exp)
        }
        Token::StrLit(s) => {
            let s = std::mem::take(s);
            *next = ts.next()?.0;
            let (mut exp, mut found) = parse_po(ts, next, Expression::StrLit(s))?;
            while found {
                (exp, found) = parse_po(ts, next, exp)?;
            }
            Some(exp)
        }
        Token::Sub => {
            *next = ts.next()?.0;
            let val = Box::new(parse_un(ts, next)?);
            Some(Expression::Neg(val))
        }
        Token::Not => {
            *next = ts.next()?.0;
            let val = Box::new(parse_un(ts, next)?);
            Some(Expression::LogNot(val))
        }
        Token::BitNot => {
            *next = ts.next()?.0;
            let val = Box::new(parse_un(ts, next)?);
            Some(Expression::BitNot(val))
        }
        Token::Mul => {
            *next = ts.next()?.0;
            let val = Box::new(parse_un(ts, next)?);
            Some(Expression::Deref(val))
        }
        Token::BitAnd => {
            *next = ts.next()?.0;
            let val = Box::new(parse_un(ts, next)?);
            Some(Expression::AddOf(val))
        }
        Token::Inc => {
            *next = ts.next()?.0;
            let val = Box::new(parse_un(ts, next)?);
            Some(Expression::PreInc(val))
        }
        Token::Dec => {
            *next = ts.next()?.0;
            let val = Box::new(parse_un(ts, next)?);
            Some(Expression::PreDec(val))
        }
        Token::LeftParen => {
            *next = ts.next()?.0;
            let exp = parse_or(ts, next, Terminal::RightParen as u32)?;
            if let Token::RightParen = next {
                *next = ts.next()?.0;
                let (mut exp, mut found) = parse_po(ts, next, exp)?;
                while found {
                    (exp, found) = parse_po(ts, next, exp)?;
                }
                Some(exp)
            } else {
                unreachable!()
            }
        }
        Token::Sizeof => {
            if ts.next()?.0 != Token::LeftParen { return None };
            *next = ts.next()?.0;
            let exp = parse_or(ts, next, Terminal::RightParen as u32)?;
            if let Token::RightParen = next {
                *next = ts.next()?.0;
                let (mut exp, mut found) = parse_po(ts, next, exp)?;
                while found {
                    (exp, found) = parse_po(ts, next, exp)?;
                }
                Some(Expression::Sizeof(Box::new(exp)))
            } else {
                unreachable!()
            }
        }
        Token::Owned => {
            if ts.next()?.0 != Token::LeftParen { return None };
            let mut n = ts.next()?;
            *next = n.0.clone();
            let t = type_only(ts, &mut n).ok()?;
            if let Token::RightParen = next {
                let exp = Expression::Owned(t);
                *next = ts.next()?.0;
                let (mut exp, mut found) = parse_po(ts, next, exp)?;
                while found {
                    (exp, found) = parse_po(ts, next, exp)?;
                }
                Some(exp)
            } else {
                unreachable!()
            }
        }
        _ => None
    }
}

fn parse_po(ts: &mut Lexer, next: &mut Token, left: Expression) -> Option<(Expression, bool)> {
    match next {
        Token::Inc => {
            *next = ts.next()?.0;
            Some((Expression::PostInc(Box::new(left)), true))
        }
        Token::Dec => {
            *next = ts.next()?.0;
            Some((Expression::PostDec(Box::new(left)), true))
        }
        Token::Dot => {
            *next = ts.next()?.0;
            if let Token::Identifier(name) = next {
                let name = std::mem::take(name);
                *next = ts.next()?.0;
                Some((Expression::Member(Box::new(left), Box::new(Expression::Identifier(name))), true))
            } else {
                None
            }
        }
        Token::Arrow => {
            *next = ts.next()?.0;
            if let Token::Identifier(name) = next {
                let name = std::mem::take(name);
                *next = ts.next()?.0;
                let left = Expression::Deref(Box::new(left));
                Some((Expression::Member(Box::new(left), Box::new(Expression::Identifier(name))), true))
            } else {
                None
            }
        }
        Token::LeftBrack => {
            *next = ts.next()?.0;
            let exp = parse_or(ts, next, Terminal::RightBrack as u32)?;
            if let Token::RightBrack = next {
                *next = ts.next()?.0;
                Some((Expression::ArrGet(Box::new(left), Box::new(exp)), true))
            } else {
                None
            }
        }
        Token::LeftParen => {
            *next = ts.next()?.0;
            let mut args = Vec::new();
            while *next != Token::RightParen {
                let exp = parse_or(ts, next, Terminal::RightParen as u32 | Terminal::Comma as u32)?;
                args.push(exp);
                if *next == Token::Comma {
                    *next = ts.next()?.0;
                }
            }
            if let Expression::Identifier(name) = left {
                *next = ts.next()?.0;
                Some((Expression::FnCall(name, args), true))
            } else {
                None
            }
        }
        Token::LeftBrace => {
            *next = ts.next()?.0;
            let mut args = Vec::new();
            while *next != Token::RightBrace {
                let left = if let Token::Identifier(id) = next {
                    std::mem::take(id)
                } else {
                    return None
                };
                *next = ts.next()?.0;
                match next {
                    Token::Comma => {
                        args.push((left.clone(), Expression::Identifier(left)));
                        *next = ts.next()?.0;
                    }
                    Token::Colon => {
                        *next = ts.next()?.0;
                        let exp = parse_or(ts, next, Terminal::RightBrace as u32 | Terminal::Comma as u32)?;
                        args.push((left, exp));
                        if *next == Token::Comma {
                            *next = ts.next()?.0;
                        }
                    }
                    _ => return None
                }
            }
            if let Expression::Identifier(name) = left {
                *next = ts.next()?.0;
                Some((Expression::StructLit(name, args), true))
            }
            else {
                None
            } 
        }
        _ => Some((left, false))
    }
}

