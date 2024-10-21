
use crate::lexer::{Lexer, Token};
use crate::ast::*;
use span::Span;
use super::{ParserError, ParserErrors, error::seek_to};


pub fn type_only(tokens: &mut Lexer, next: &mut (Token, Span)) -> Result<Type, ParserErrors> {
    let mut errs = ParserErrors::new();
    let mut ty = Type::new();
    let mut can_id = true;
    while let Token::Const|Token::Signed|Token::Unsigned|Token::Short|Token::Long|Token::Void|
              Token::Bool|Token::Char|Token::Int|Token::Float|Token::Double|Token::Identifier(_) = next.0 {
        if let Token::Identifier(_) = next.0 {
            if !can_id { break; }
        }
        if next.0 != Token::Const { can_id = false; }
        ty.type_info(std::mem::take(&mut next.0));
        *next = tokens.next().expect("EOF shouldve been handled as token");
    }

    let mut ty = ty.lock()
        .ok_or(ParserError::TypeContradiction(next.1))
        .inspect_err(|e| errs.push(e.clone()))
        .unwrap_or_default();
    while let Token::Const|Token::Mul|Token::BitAnd = next.0 {
        ty.ptr_status(std::mem::take(&mut next.0));
        *next = tokens.next().expect("EOF shouldve been handled as token");
    }

    errs.wrap(ty.build().unwrap())
}

pub fn type_and_name(tokens: &mut Lexer, next: &mut (Token, Span)) -> Result<(Type, String), ParserErrors> {
    let mut errs = ParserErrors::new();
    let ty = type_only(tokens, next)
        .inspect_err(|e| errs.extend(e.clone()))
        .unwrap_or_default();
    errs.eof()?;
    match next {
        (Token::EndOfFile, span) => { errs.push(ParserError::EndOfFileIn(*span, "type declaration")); Err(errs) }
        (Token::Identifier(id), _) => {
            let id = std::mem::take(id);
            *next = tokens.next().expect("EOF shouldve been handled as token");
            errs.wrap((ty, id))
        }
        _ => seek_to!(tokens,
            (Token::EndOfFile, span) => { errs.push(ParserError::EndOfFileIn(span, "type declaration")); Err(errs) }
            (Token::Equal,     span) => { errs.push(ParserError::ElemIsMissing(span, "name", "variable declaration")); Err(errs) }
            (Token::Semicolon, span) => { errs.push(ParserError::BadTokensIn(span, "variable declaration")); Err(errs) }
        )
    }
}

