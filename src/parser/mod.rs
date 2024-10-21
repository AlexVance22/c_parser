mod types;
mod exprs;
mod block;
#[macro_use]
mod error;

use crate::lexer::{Lexer, Token};
use crate::ast::{span::Span, *};
use types::*;
use exprs::Terminal;
use block::fetch_block;
use error::{ParserError, ParserErrors};


pub fn parse(mut tokens: Lexer) -> Result<Ast, ParserErrors> {
    let mut errs = ParserErrors::new();
    let mut ast = Ast::new();
    let mut types = Vec::new();

    while let Some(next) = tokens.next() {
        match next {
            (Token::Const|Token::Signed|Token::Unsigned|Token::Short|Token::Long|Token::Void|
             Token::Bool|Token::Char|Token::Int|Token::Float|Token::Double|Token::Identifier(_), _) => {
                match fetch_declaration(&mut tokens, &types, next) {
                    Ok(d) => ast.push(d),
                    Err(e) => { errs.extend(e); errs.eof()?; }
                }
            }
            (Token::Struct, _) => match fetch_struct(&mut tokens) {
                Ok(s) => { types.push(s.name.clone()); ast.push(GlobalItem::Struct(s)); }
                Err(e) => { errs.extend(e); errs.eof()?; }
            }
            (Token::Typedef, _) => match fetch_typedef(&mut tokens) {
                Ok(t) => { types.push(t.id.clone()); ast.push(GlobalItem::TypeDef(t)); }
                Err(e) => { errs.extend(e); errs.eof()?; }
            }
            (Token::EndOfFile, _) => return errs.wrap(ast),
            (_, span) => {
                errs.push(ParserError::BadTokensIn(span, "global scope"));
                seek_to!(&mut tokens,
                    (Token::EndOfFile, _) => { return Err(errs) }
                    (Token::Semicolon, _) => {}
                )
            }
        }
    }

    errs.wrap(ast)
}


fn fetch_declaration(tokens: &mut Lexer, types: &[String], mut next: (Token, Span)) -> Result<GlobalItem, ParserErrors> {
    let mut errs = ParserErrors::new();
    let (ty, id) = type_and_name(tokens, &mut next)
        .inspect_err(|e| errs.extend(e.clone()))
        .unwrap_or_default();
    errs.eof()?;

    match next {
        (Token::Semicolon, _) => errs.wrap(GlobalItem::GlobalVarDecl(VarDecl{ ty, id, val: None })),
        (Token::Equal, span) => {
            let expr = exprs::parse_rhs(tokens, Terminal::Semicolon as u32)
                .ok_or(ParserError::GenericExprError(span))
                .inspect_err(|e| errs.push(e.clone()))
                .unwrap_or_default();
            errs.wrap(GlobalItem::GlobalVarDecl(VarDecl{ ty, id, val: Some(expr) }))
        }
        (Token::LeftParen, _) => {
            let func = fetch_function(tokens, types, ty, id)
                .inspect_err(|e| errs.extend(e.clone()))
                .unwrap_or_default();
            errs.wrap(GlobalItem::Function(func))
        }
        (Token::EndOfFile, span) => {
            errs.push(ParserError::EndOfFileIn(span, "declaration"));
            Err(errs)
        }
        _ => seek_to!(tokens,
            (Token::EndOfFile, span) => {
                errs.push(ParserError::BadTokensIn(span, "declaration"));
                errs.push(ParserError::EndOfFileIn(span, "declaration"));
                Err(errs)
            }
            (Token::Semicolon, span) => {
                errs.push(ParserError::BadTokensIn(span, "declaration"));
                Err(errs)
            }
            (Token::Equal, span) => {
                errs.push(ParserError::BadTokensIn(span, "function declaration"));
                let _ = exprs::parse_rhs(tokens, Terminal::Semicolon as u32)
                    .ok_or(ParserError::GenericExprError(span))
                    .inspect_err(|e| errs.push(e.clone()));
                Err(errs)
            }
            (Token::LeftParen, span) => {
                errs.push(ParserError::BadTokensIn(span, "function declaration"));
                let _ = fetch_function(tokens, types, ty, id)
                    .inspect_err(|e| errs.extend(e.clone()));
                Err(errs)
            }
        )
    }
}

fn fetch_function(tokens: &mut Lexer, types: &[String], ty: Type, id: String) -> Result<Function, ParserErrors> {
    let mut errs = ParserErrors::new();
    let mut params = Vec::new();
    let mut next = tokens.next().expect("EOF shouldve been handled as token");
    while next.0 != Token::RightParen {
        params.push(type_and_name(tokens, &mut next)
            .inspect_err(|e| errs.extend(e.clone()))
            .unwrap_or_default());
        errs.eof()?;
        if next.0 == Token::Comma { next = tokens.next().expect("EOF shouldve been handled as token"); }
    }
    match tokens.next().expect("EOF shouldve been handled as token") {
        (Token::EndOfFile, span) => Err(errs.end_file(span, "function declaration")),
        (Token::Semicolon,    _) => errs.wrap(Function{ name: id, params, rettype: ty, body: None }),
        (Token::LeftBrace,    _) => {
            let body = fetch_block(tokens, types)
                .inspect_err(|e| errs.extend(e.clone()))
                .unwrap_or_default();
            errs.wrap(Function{ name: id, params, rettype: ty, body: Some(body) })
        }
        (_, span) => {
            errs.push(ParserError::BadTokensIn(span, "function declaration"));
            seek_to!(tokens,
                (Token::EndOfFile, span) => { Err(errs.end_file(span, "function declaration")) }
                (Token::Semicolon,    _) => { Err(errs) }
                (Token::LeftBrace,    _) => {
                    let _ = fetch_block(tokens, types).inspect_err(|e| errs.extend(e.clone()));
                    Err(errs)
                }
            )
        }
    }
}

fn fetch_struct(tokens: &mut Lexer) -> Result<Struct, ParserErrors> {
    let mut errs = ParserErrors::new();
    let name = match tokens.next().expect("EOF shouldve been handled as token") {
        (Token::EndOfFile, span ) => { return Err(errs.end_file(span, "struct declaration")) }
        (Token::Semicolon, span ) => { errs.push(ParserError::ElemIsMissing(span, "struct declaration", "name")); return Err(errs) }
        (Token::LeftBrace, span ) => { errs.push(ParserError::ElemIsMissing(span, "struct declaration", "name")); "".to_string() }
        (Token::Identifier(name), _ ) => name,
        (_, span) => {
            errs.push(ParserError::BadTokensIn(span, "struct declaration"));
            seek_to!(tokens,
                (Token::EndOfFile, span) => { return Err(errs.end_file(span, "struct declaration")) }
                (Token::Semicolon, _) => { return Err(errs) }
                (Token::LeftBrace, _) => { "".to_string() }
            )
        }
    };
    match tokens.next().expect("EOF shouldve been handled as token") {
        (Token::EndOfFile, span) => { return Err(errs.end_file(span, "struct declaration")) }
        (Token::Semicolon, _) => { return errs.wrap(Struct{ name, members: None }) }
        (Token::LeftBrace, _) => {},
        (_, span) => {
            errs.push(ParserError::BadTokensIn(span, "struct declaration"));
            seek_to!(tokens,
                (Token::EndOfFile, span) => { return Err(errs.end_file(span, "struct declaration")) }
                (Token::Semicolon, _) => { return Err(errs) }
            )
        }
    }
    let mut members = Vec::new();
    let mut next = tokens.next().expect("EOF shouldve been handled as token");
    while next.0 != Token::RightBrace {
        members.push(type_and_name(tokens, &mut next)
            .inspect_err(|e| errs.extend(e.clone()))
            .unwrap_or_default());
        errs.eof()?;
        match next {
            (Token::EndOfFile, span) => { return Err(errs.end_file(span, "sruct declaration")) }
            (Token::RightBrace, _) => {
                match tokens.next().expect("EOF shouldve been handled as token") {
                    (Token::Semicolon, span) => {
                        errs.push(ParserError::ElemIsMissing(span, "member declaration", "trailing semicolon"));
                        return Err(errs)
                    }
                    (_, span) => {
                        errs.push(ParserError::BadTokensIn(span, "struct declaration"));
                        seek_to!(tokens,
                            (Token::EndOfFile, span) => { return Err(errs.end_file(span, "struct declaration")) }
                            (Token::Semicolon, _) => { return Err(errs) }
                        )
                    }
                }
            }
            (Token::Semicolon, _) => {}
            (_, span) => {
                errs.push(ParserError::BadTokensIn(span, "struct declaration"));
                seek_to!(tokens,
                    (Token::EndOfFile, span) => { return Err(errs.end_file(span, "struct declaration")) }
                    (Token::Semicolon, _) => {}
                    (Token::RightBrace, _) => {
                        match tokens.next().expect("EOF shouldve been handled as token") {
                            (Token::Semicolon, _) => return Err(errs),
                            (_, span) => {
                                errs.push(ParserError::BadTokensIn(span, "struct declaration"));
                                seek_to!(tokens,
                                    (Token::EndOfFile, span) => { return Err(errs.end_file(span, "struct declaration")) }
                                    (Token::Semicolon, _) => { return Err(errs) }
                                )
                            }
                        }
                    }
                )
            }
        }
        next = tokens.next().expect("EOF shouldve been handled as token");
    }
    match tokens.next().expect("EOF shouldve been handled as token") {
        (Token::Semicolon, _) => errs.wrap(Struct{ name, members: Some(members) }),
        (_, span) => {
            errs.push(ParserError::BadTokensIn(span, "struct declaration"));
            seek_to!(tokens,
                (Token::EndOfFile, span) => { Err(errs.end_file(span, "struct declaration")) }
                (Token::Semicolon, _) => { Err(errs) }
            )
        }
    }
}

fn fetch_typedef(tokens: &mut Lexer) -> Result<TypeDef, ParserErrors> {
    let mut errs = ParserErrors::new();
    let mut next = tokens.next().unwrap();
    let (ty, id) = match next{
        (Token::Const|Token::Signed|Token::Unsigned|Token::Short|Token::Long|Token::Void|
        Token::Bool|Token::Char|Token::Int|Token::Float|Token::Double, _) => {
            type_and_name(tokens, &mut next)
                .inspect_err(|e| errs.extend(e.clone()))
                .unwrap_or_default()
        }
        (_, span) => {
            errs.push(ParserError::BadTokensIn(span, "typedef"));
            seek_to!(tokens,
                (Token::EndOfFile, span) => { return Err(errs.end_file(span, "typedef")) }
                (Token::Semicolon, _) => { return Err(errs) }
            )
        }
    };
    errs.eof()?;
    if next.0 == Token::Semicolon {
        errs.wrap(TypeDef{ ty, id })
    } else {
        errs.push(ParserError::BadTokensIn(next.1, "typedef"));
        seek_to!(tokens,
            (Token::EndOfFile, span) => { Err(errs.end_file(span, "typedef")) }
            (Token::Semicolon, _) => { Err(errs) }
        )
    }
}

