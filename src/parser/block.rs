use crate::{
    lexer::{Lexer, Token},
    ast::{span::Span, *},
};
use super::{
    error::{seek_to, ParserError, ParserErrors},
    exprs::{self, Terminal},
    types::type_and_name
};


pub fn fetch_block(tokens: &mut Lexer, types: &[String]) -> Result<Vec<Statement>, ParserErrors> {
    let mut errs = ParserErrors::new();
    let mut result = Vec::new();

    while let Some(next) = tokens.next() {
        match next.0 {
            Token::Const|Token::Signed|Token::Unsigned|Token::Short|Token::Long|Token::Void|
            Token::Bool|Token::Char|Token::Int|Token::Float|Token::Double => {
                let decl = fetch_decl(tokens, next)
                    .inspect_err(|e| errs.extend(e.clone()))
                    .unwrap_or_default();
                errs.eof()?;
                result.push(decl);
            }
            Token::Identifier(ref id) if types.contains(id) => {
                let decl = fetch_decl(tokens, next)
                    .inspect_err(|e| errs.extend(e.clone()))
                    .unwrap_or_default();
                errs.eof()?;
                result.push(decl);
            }
            Token::If => match tokens.next().expect("EOF shouldve been handled as token") {
                (Token::EndOfFile, span) => { return Err(errs.end_file(span, "if statement")) }
                (Token::Semicolon, span) => { errs.push(ParserError::ElemIsMissing(span, "if statement", "condition")); }
                (Token::LeftParen, _) => {
                    let cond = conditional_body(tokens, types, "if statement")
                        .inspect_err(|e| errs.extend(e.clone()))
                        .unwrap_or_default();
                    errs.eof()?;
                    result.push(Statement::If(cond));
                }
                (_, span) => {
                    errs.push(ParserError::BadTokensIn(span, "if statement"));
                    seek_to!(tokens,
                        (Token::EndOfFile, span) => { return Err(errs.end_file(span, "if statement")) }
                        (Token::Semicolon, _) => {}
                        (Token::LeftParen, _) => {
                            let _ = conditional_body(tokens, types, "if statement").inspect_err(|e| errs.extend(e.clone()));
                            errs.eof()?;
                        }
                    )
                }
            }
            Token::Else => match tokens.next().expect("EOF shouldve been handled as token") {
                (Token::EndOfFile, span) => { return Err(errs.end_file(span, "else statement")) }
                (Token::Semicolon, ..) => result.push(Statement::Else(None)),
                (Token::LeftBrace, ..) => {
                    let block = fetch_block(tokens, types)
                        .inspect_err(|e| errs.extend(e.clone()))
                        .unwrap_or_default();
                    errs.eof()?;
                    result.push(Statement::Else(Some(block)));
                }
                (Token::If, ..) => match tokens.next().expect("EOF shouldve been handled as token") {
                    (Token::EndOfFile, span) => { return Err(errs.end_file(span, "if statement")) }
                    (Token::Semicolon, span) => { errs.push(ParserError::ElemIsMissing(span, "if statement", "condition")); }
                    (Token::LeftParen, ..) => {
                        let cond = conditional_body(tokens, types, "if statement")
                            .inspect_err(|e| errs.extend(e.clone()))
                            .unwrap_or_default();
                        errs.eof()?;
                        result.push(Statement::Elif(cond));
                    }
                    (_, span) => {
                        errs.push(ParserError::BadTokensIn(span, "if statement"));
                        seek_to!(tokens,
                            (Token::EndOfFile, span) => { return Err(errs.end_file(span, "if statement")) }
                            (Token::Semicolon, _) => {}
                            (Token::LeftParen, _) => {
                                let _ = conditional_body(tokens, types, "if statement").inspect_err(|e| errs.extend(e.clone()));
                                errs.eof()?;
                            }
                        )
                    }
                }
                (_, span) => {
                    errs.push(ParserError::BadTokensIn(span, "else statement"));
                    seek_to!(tokens,
                        (Token::EndOfFile, span) => { return Err(errs.end_file(span, "else statement")) }
                        (Token::Semicolon, _) => {}
                        (Token::LeftBrace, _) => {
                            let _ = fetch_block(tokens, types).inspect_err(|e| errs.extend(e.clone()));
                            errs.eof()?;
                        }
                    )
                }
            }
            Token::While => match tokens.next().expect("EOF shouldve been handled as token") {
                (Token::EndOfFile, span) => { return Err(errs.end_file(span, "while loop head")) }
                (Token::Semicolon, span) => { errs.push(ParserError::ElemIsMissing(span, "while loop head", "condition")); }
                (Token::LeftParen, ..) => {
                    let cond = conditional_body(tokens, types, "while loop")
                        .inspect_err(|e| errs.extend(e.clone()))
                        .unwrap_or_default();
                    errs.eof()?;
                    result.push(Statement::While(cond));
                }
                (_, span) => {
                    errs.push(ParserError::BadTokensIn(span, "while loop head"));
                    seek_to!(tokens,
                        (Token::EndOfFile, span) => { return Err(errs.end_file(span, "while loop head")) }
                        (Token::Semicolon, _) => {}
                        (Token::LeftParen, _) => {
                            let _ = conditional_body(tokens, types, "while loop").inspect_err(|e| errs.extend(e.clone()));
                            errs.eof()?;
                        }
                    )
                }
            }
            Token::For => match tokens.next().expect("EOF shouldve been handled as token") {
                (Token::EndOfFile, span) => { return Err(errs.end_file(span, "for loop head")) }
                (Token::Semicolon, span) => { errs.push(ParserError::ElemIsMissing(span, "for loop head", "expression")); }
                (Token::LeftParen, ..) => {
                    let for_loop = for_loop_body(tokens, types)
                        .inspect_err(|e| errs.extend(e.clone()))
                        .unwrap_or_default();
                    errs.eof()?;
                    result.push(for_loop);
                }
                (_, span) => {
                    errs.push(ParserError::BadTokensIn(span, "for loop head"));
                    seek_to!(tokens,
                        (Token::EndOfFile, span) => { return Err(errs.end_file(span, "for loop head")) }
                        (Token::Semicolon, _) => {}
                        (Token::LeftParen, _) => {
                            let _ = for_loop_body(tokens, types).inspect_err(|e| errs.extend(e.clone()));
                            errs.eof()?;
                        }
                    )
                }
            }
            Token::Switch => match tokens.next().expect("EOF shouldve been handled as token") {
                (Token::EndOfFile, span) => { return Err(errs.end_file(span, "switch statement head")) }
                (Token::Semicolon, span) => { errs.push(ParserError::ElemIsMissing(span, "switch statement head", "expression")); }
                (Token::LeftParen, ..) => {
                    let cond = conditional_body(tokens, types, "switch statement")
                        .inspect_err(|e| errs.extend(e.clone()))
                        .unwrap_or_default();
                    errs.eof()?;
                    result.push(Statement::Switch(cond));
                }
                (_, span) => {
                    errs.push(ParserError::BadTokensIn(span, "switch statement head"));
                    seek_to!(tokens,
                        (Token::EndOfFile, span) => { return Err(errs.end_file(span, "switch statement head")) }
                        (Token::Semicolon, _) => {}
                        (Token::LeftParen, _) => {
                            let _ = conditional_body(tokens, types, "switch statement").inspect_err(|e| errs.extend(e.clone()));
                            errs.eof()?;
                        }
                    )
                }
            }
            Token::Break => match tokens.next().expect("EOF shouldve been handled as token") {
                (Token::EndOfFile, span) => { return Err(errs.end_file(span, "break statement")) }
                (Token::Semicolon, ..) => result.push(Statement::Break),
                (_, span) => {
                    errs.push(ParserError::BadTokensIn(span, "break statement"));
                    seek_to!(tokens,
                        (Token::EndOfFile, span) => { return Err(errs.end_file(span, "break statement")) }
                        (Token::Semicolon, _) => {}
                    )
                }
            }
            Token::Continue => match tokens.next().expect("EOF shouldve been handled as token") {
                (Token::EndOfFile, span) => { return Err(errs.end_file(span, "continue statement")) }
                (Token::Semicolon,   ..) => result.push(Statement::Continue),
                (_, span) => {
                    errs.push(ParserError::BadTokensIn(span, "continue statement"));
                    seek_to!(tokens,
                        (Token::EndOfFile, span) => { return Err(errs.end_file(span, "continue statement")) }
                        (Token::Semicolon, _) => {}
                    )
                }
            }
            Token::Return => match tokens.next().expect("EOF shouldve been handled as token") {
                (Token::EndOfFile, span) => { return Err(errs.end_file(span, "return statement")) }
                (Token::Semicolon,   ..) => result.push(Statement::Return(Expression::Empty)),
                (token, span) => {
                    let expr = exprs::parse(tokens, Some(token), Terminal::Semicolon as u32)
                        .ok_or(ParserError::GenericExprError(span))
                        .inspect_err(|e| errs.push(e.clone()))
                        .unwrap_or_default();
                    result.push(Statement::Return(expr));
                }
            }
            Token::Case => match tokens.next().expect("EOF shouldve been handled as token") {
                (Token::EndOfFile, span) => { return Err(errs.end_file(span, "case label")) }
                (Token::Semicolon, span) => { errs.push(ParserError::ElemIsMissing(span, "case label", "constant")); }
                (Token::Colon,     span) => { errs.push(ParserError::ElemIsMissing(span, "case label", "constant")); }
                (token, span) => {
                    let expr = exprs::parse(tokens, Some(token), Terminal::Colon as u32)
                        .ok_or(ParserError::GenericExprError(span))
                        .inspect_err(|e| errs.push(e.clone()))
                        .unwrap_or_default();
                    result.push(Statement::Case(expr));
                }
            }
            Token::Def => match tokens.next().expect("EOF shouldve been handled as token") {
                (Token::EndOfFile, span) => { return Err(errs.end_file(span, "default label")) }
                (Token::Semicolon, ..) => result.push(Statement::Def),
                (_, span) => {
                    errs.push(ParserError::BadTokensIn(span, "default label"));
                    seek_to!(tokens,
                        (Token::EndOfFile, span) => { return Err(errs.end_file(span, "default label")) }
                        (Token::Semicolon, _) => {}
                    )
                }
            }
            Token::LeftBrace => {
                let block = fetch_block(tokens, types)
                    .inspect_err(|e| errs.extend(e.clone()))
                    .unwrap_or_default();
                errs.eof()?;
                result.push(Statement::Scope(block));
            }
            Token::RightBrace => return errs.wrap(result),
            _ => {
                let expr = exprs::parse(tokens, Some(next.0.clone()), Terminal::Semicolon as u32)
                    .ok_or(ParserError::GenericExprError(next.1))
                    .inspect_err(|e| errs.push(e.clone()))
                    .unwrap_or_default();
                result.push(Statement::Expression(expr));
            }
        }
    }

    Err(errs.end_file(Span::default(), "block"))
}



fn conditional_body(tokens: &mut Lexer, types: &[String], name: &'static str) -> Result<ExprBlock, ParserErrors> {
    let mut errs = ParserErrors::new();
    let expr = exprs::parse(tokens, None, Terminal::RightParen as u32)
        .ok_or(ParserError::GenericExprError(Span::default()))
        .inspect_err(|e| errs.push(e.clone()))
        .unwrap_or_default();
    match tokens.next().expect("EOF shouldve been handled as token") {
        (Token::EndOfFile, span) => { Err(errs.end_file(span, name)) }
        (Token::Semicolon, ..) => errs.wrap(ExprBlock{ expr, body: None }),
        (Token::LeftBrace, ..) => {
            let block = fetch_block(tokens, types)
                .inspect_err(|e| errs.extend(e.clone()))
                .unwrap_or_default();
            errs.wrap(ExprBlock{ expr, body: Some(block) })
        }
        (_, span) => {
            errs.push(ParserError::BadTokensIn(span, name));
            seek_to!(tokens,
                (Token::EndOfFile, span) => { Err(errs.end_file(span, name)) }
                (Token::Semicolon, _) => { Err(errs) }
                (Token::LeftBrace, _) => {
                    let _ = fetch_block(tokens, types).inspect_err(|e| errs.extend(e.clone()));
                    Err(errs)
                }
            )
        }
    }
}


fn fetch_decl(tokens: &mut Lexer, mut next: (Token, Span)) -> Result<Statement, ParserErrors> {
    let mut errs = ParserErrors::new();
    let (ty, id) = type_and_name(tokens, &mut next)
        .inspect_err(|e| errs.extend(e.clone()))
        .unwrap_or_default();
    errs.eof()?;
    match next.0 {
        Token::Semicolon => errs.wrap(Statement::VarDecl(VarDecl{ ty, id, val: None })),
        Token::Equal => {
            let expr = exprs::parse_rhs(tokens, Terminal::Semicolon as u32)
                .ok_or(ParserError::GenericExprError(next.1))
                .inspect_err(|e| errs.push(e.clone()))
                .unwrap_or_default();
            errs.wrap(Statement::VarDecl(VarDecl{ ty, id, val: Some(expr) }))
        }
        _ => {
            errs.push(ParserError::BadTokensIn(next.1, "variable declaration"));
            seek_to!(tokens,
                (Token::EndOfFile, span) => { Err(errs.end_file(span, "variable declaration")) }
                (Token::Semicolon, _) => { Err(errs) }
            )
        }
    }
}

fn for_loop_body(tokens: &mut Lexer, types: &[String]) -> Result<Statement, ParserErrors> {
    let mut errs = ParserErrors::new();
    let mut next = tokens.next().unwrap();
    let e1 = match next {
        (Token::Const|Token::Signed|Token::Unsigned|Token::Short|Token::Long|Token::Void|
         Token::Bool|Token::Char|Token::Int|Token::Float|Token::Double, _) => {
            let (ty, id) = type_and_name(tokens, &mut next)
                .inspect_err(|e| errs.extend(e.clone()))
                .unwrap_or_default();
            errs.eof()?;
            match next {
                (Token::EndOfFile, span) => { return Err(errs.end_file(span, "variable declaration")) }
                (Token::Semicolon,    _) => Statement::VarDecl(VarDecl{ ty, id, val: None }),
                (Token::Equal,     span) => {
                    let expr = exprs::parse_rhs(tokens, Terminal::Semicolon as u32)
                        .ok_or(ParserError::GenericExprError(span))
                        .inspect_err(|e| errs.push(e.clone()))
                        .unwrap_or_default();
                    Statement::VarDecl(VarDecl{ ty, id, val: Some(expr) })
                }
                _ => {
                    errs.push(ParserError::BadTokensIn(next.1, "variable declaration"));
                    seek_to!(tokens,
                        (Token::EndOfFile, span) => { return Err(errs.end_file(span, "variable declaration")) }
                        (Token::Semicolon,    _) => { Statement::Empty }
                        (Token::Equal    , span) => {
                            let _ = exprs::parse_rhs(tokens, Terminal::Semicolon as u32)
                                .ok_or(ParserError::GenericExprError(span))
                                .inspect_err(|e| errs.push(e.clone()));
                            Statement::Empty
                        }
                    )
                }
            }
        }
        (token, span) => Statement::Expression(exprs::parse(tokens, Some(token), Terminal::Semicolon as u32)
                                        .ok_or(ParserError::GenericExprError(span))
                                        .inspect_err(|e| errs.push(e.clone()))
                                        .unwrap_or_default())
    };
    let e1 = Box::new(e1);
    next = tokens.next().expect("EOF shouldve been handled as token");
    let e2 = exprs::parse(tokens, Some(next.0.clone()), Terminal::Semicolon as u32)
        .ok_or(ParserError::GenericExprError(next.1))
        .inspect_err(|e| errs.push(e.clone()))
        .unwrap_or_default();
    next = tokens.next().expect("EOF shouldve been handled as token");
    let e3 = exprs::parse(tokens, Some(next.0.clone()), Terminal::RightParen as u32)
        .ok_or(ParserError::GenericExprError(next.1))
        .inspect_err(|e| errs.push(e.clone()))
        .unwrap_or_default();
    match tokens.next().expect("EOF shouldve been handled as token") {
        (Token::EndOfFile, span) => { Err(errs.end_file(span, "for loop head")) }
        (Token::Semicolon, _) => errs.wrap(Statement::For(ForLoop{ e1, e2, e3, body: None })),
        (Token::LeftBrace, _) => {
            let body = fetch_block(tokens, types)
                .inspect_err(|e| errs.extend(e.clone()))
                .unwrap_or_default();
            errs.wrap(Statement::For(ForLoop{ e1, e2, e3, body: Some(body) }))
        }
        (_, span) => {
            errs.push(ParserError::BadTokensIn(span, "for loop head"));
            seek_to!(tokens,
                (Token::EndOfFile, span) => { Err(errs.end_file(span, "for loop head")) }
                (Token::Semicolon, _) => { Err(errs) }
                (Token::LeftBrace, _) => {
                    let _ = fetch_block(tokens, types).inspect_err(|e| errs.extend(e.clone()));
                    Err(errs)
                }
            )
        }
    }
}

