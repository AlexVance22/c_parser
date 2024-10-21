mod token;

use std::str::Chars;
use crate::ast::span::{Span, SpanPoint};
pub use token::Token;


pub struct Lexer<'a> {
    src: Chars<'a>,
    next: Option<char>,
    loc: SpanPoint,
    eof: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        let mut src = src.chars();
        let next = src.next();
        Self { src, next, eof: false, loc: SpanPoint::default() }
    }

    fn read_id(&mut self) -> (Token, Span) {
        let mut id = self.next.unwrap().to_string();
        let begin = self.loc;
        self.loc.shift_col(1);
        self.next = self.src.next();

        while let Some(c @ ('_'|'a'..='z'|'A'..='Z'|'0'..='9')) = self.next {
            id.push(c);
            self.loc.shift_col(1);
            self.next = self.src.next();
            if self.next.is_none() {
                return Token::from_str(&id, Span::new(begin, self.loc)).unwrap()
            }
        }

        Token::from_str(&id, Span::new(begin, self.loc)).unwrap()
    }

    fn read_num(&mut self) -> (Token, Span) {
        let mut num = self.next.unwrap().to_string();
        let begin = self.loc;
        self.loc.shift_col(1);
        self.next = self.src.next();
        let mut dot = false;
        let mut i = 1;

        while let Some(c) = self.next {
            if (i != 1 && !matches!(c, '0'..='9'|'a'..='f'|'A'..='F'|'.')) || (i == 1 && !matches!(c, '0'..='9'|'x'|'b')) {
                break
            }
            if c == '.' {
                if !dot {
                    dot = true;
                } else {
                    break
                }
            }
            num.push(c);
            self.loc.shift_col(1);
            self.next = self.src.next();
            if self.next.is_none() {
                return Token::from_str(&num, Span::new(begin, self.loc)).unwrap()
            }
            i += 1;
        }

        Token::from_str(&num, Span::new(begin, self.loc)).unwrap()
    }

    fn read_str(&mut self) -> (Token, Span) {
        let mut val = self.next.unwrap().to_string();
        let begin = self.loc;
        self.loc.shift_col(1);
        self.next = self.src.next();
        let mut esc = false;

        while let Some(c) = self.next {
            if esc {
                match c {
                    't' =>  val.push('\t'),
                    'r' =>  val.push('\r'),
                    'n' =>  val.push('\n'),
                    '0' =>  val.push('\0'),
                    '"' =>  val.push('"'),
                    '\\' => val.push('\\'),
                    _ => ()
                }
                esc = false;
            } else if c == '"' {
                val.push(c);
                self.loc.shift_col(1);
                self.next = self.src.next();
                return Token::from_str(&val, Span::new(begin, self.loc)).unwrap()
            } else if c == '\\' {
                esc = true;
            } else {
                val.push(c);
            }
            self.loc.shift_col(1);
            self.next = self.src.next();
        }

        Token::from_str(&val, Span::new(begin, self.loc)).unwrap()
    }

    fn read_char(&mut self) -> (Token, Span) {
        let mut val = self.next.unwrap().to_string();
        let begin = self.loc;
        self.loc.shift_col(1);
        self.next = self.src.next();

        if let Some(c) = self.next {
            if c == '\\' {
                self.loc.shift_col(1);
                self.next = self.src.next();
                if let Some(c) = self.next {
                    match c {
                        't' =>  val.push('\t'),
                        'r' =>  val.push('\r'),
                        'n' =>  val.push('\n'),
                        '0' =>  val.push('\0'),
                        '"' =>  val.push('"'),
                        '\\' => val.push('\\'),
                        _ => ()
                    }
                }
            } else {
                val.push(c);
            }
        }

        self.next = self.src.next();
        val.push(self.next.unwrap());
        self.loc.shift_col(2);
        self.next = self.src.next();
        Token::from_str(&val, Span::new(begin, self.loc)).unwrap()
    }

    fn read_op(&mut self) -> Option<(Token, Span)> {
        let mut op = self.next.unwrap().to_string();
        let begin = self.loc;

        match self.next.unwrap() {
            '*'|'%'|'~'|'&'|'|'|'^'|'!'|'=' => {
                self.loc.shift_col(1);
                self.next = self.src.next();
                match self.next {
                    Some('=') => {
                        op.push(self.next.unwrap());
                        self.loc.shift_col(1);
                        self.next = self.src.next();
                        Token::from_str(&op, Span::new(begin, self.loc)).ok()
                    }
                    _ => Token::from_str(&op, Span::new(begin, self.loc)).ok()
                }
            }
            '+' => {
                self.loc.shift_col(1);
                self.next = self.src.next();
                match self.next {
                    Some('=')|Some('+') => {
                        op.push(self.next.unwrap());
                        self.loc.shift_col(1);
                        self.next = self.src.next();
                        Token::from_str(&op, Span::new(begin, self.loc)).ok()
                    }
                    _ => Token::from_str(&op, Span::new(begin, self.loc)).ok()
                }
            }
            '-' => {
                self.loc.shift_col(1);
                self.next = self.src.next();
                match self.next {
                    Some('=')|Some('-')|Some('>') => {
                        op.push(self.next.unwrap());
                        self.loc.shift_col(1);
                        self.next = self.src.next();
                        Token::from_str(&op, Span::new(begin, self.loc)).ok()
                    }
                    _ => Token::from_str(&op, Span::new(begin, self.loc)).ok()
                }
            }
            '/' => {
                self.loc.shift_col(1);
                self.next = self.src.next();
                match self.next {
                    Some('=') => {
                        op.push(self.next.unwrap());
                        self.loc.shift_col(1);
                        self.next = self.src.next();
                        Token::from_str(&op, Span::new(begin, self.loc)).ok()
                    }
                    Some('/') => {
                        self.loc.shift_col(1);
                        self.next = self.src.next();
                        self.skip_line_comment();
                        self.next()
                    }
                    Some('*') => {
                        self.loc.shift_col(1);
                        self.next = self.src.next();
                        self.skip_block_comment();
                        self.next()
                    }
                    _ => Token::from_str(&op, Span::new(begin, self.loc)).ok()
                }
            }
            '>' => {
                self.loc.shift_col(1);
                self.next = self.src.next();
                match self.next {
                    Some('=') => {
                        op.push(self.next.unwrap());
                        self.loc.shift_col(1);
                        self.next = self.src.next();
                        Token::from_str(&op, Span::new(begin, self.loc)).ok()
                    }
                    Some('>') => {
                        self.loc.shift_col(1);
                        self.next = self.src.next();
                        match self.next {
                            Some('=') => {
                                op.push(self.next.unwrap());
                                self.loc.shift_col(1);
                                self.next = self.src.next();
                                Token::from_str(&op, Span::new(begin, self.loc)).ok()
                            }
                            _ => Token::from_str(&op, Span::new(begin, self.loc)).ok()
                        }
                    }
                    _ => Token::from_str(&op, Span::new(begin, self.loc)).ok()
                }
            }
            '<' => {
                self.loc.shift_col(1);
                self.next = self.src.next();
                match self.next {
                    Some('=') => {
                        op.push(self.next.unwrap());
                        self.loc.shift_col(1);
                        self.next = self.src.next();
                        Token::from_str(&op, Span::new(begin, self.loc)).ok()
                    }
                    Some('<') => {
                        self.loc.shift_col(1);
                        self.next = self.src.next();
                        match self.next {
                            Some('=') => {
                                op.push(self.next.unwrap());
                                self.loc.shift_col(1);
                                self.next = self.src.next();
                                Token::from_str(&op, Span::new(begin, self.loc)).ok()
                            }
                            _ => Token::from_str(&op, Span::new(begin, self.loc)).ok()
                        }
                    }
                    _ => Token::from_str(&op, Span::new(begin, self.loc)).ok()
                }
            }
            _ => unreachable!()
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.next {
            if c == ' ' || c == '\t' || c == '\r' {
                self.loc.shift_col(1);
                self.next = self.src.next();
            } else if c == '\n' {
                self.loc.shift_row(1);
                self.next = self.src.next();
            } else {
                return
            }
        }
    }

    fn skip_line_comment(&mut self) {
        while let Some(c) = self.next {
            if c == '\n' {
                self.loc.shift_row(1);
                return
            } else {
                self.next = self.src.next();
            }
        }
    }

    fn skip_block_comment(&mut self) {
        while let Some(c) = self.next {
            if c == '\n' {
                self.loc.shift_row(1);
            } else if c == '*' {
                self.loc.shift_col(1);
                self.next = self.src.next();
                if let Some('/') = self.next {
                    return
                } else {
                    self.loc.shift_col(1);
                    self.next = self.src.next();
                }
            } else {
                self.loc.shift_col(1);
                self.next = self.src.next();
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Token, Span);

    fn next(&mut self) -> Option<Self::Item> {
        let c = if let Some(c) = self.next {
            c
        } else if !self.eof {
            self.eof = true;
            return Some((Token::EndOfFile, Span::empty(self.loc)))
        } else {
            return None
        };

        match c {
            '_'|'a'..='z'|'A'..='Z' => Some(self.read_id()),
            '0'..='9' =>               Some(self.read_num()),
            '"' =>                     Some(self.read_str()),
            '\'' =>                    Some(self.read_char()),
            '+'|'-'|'*'|'/'|'%'|'>'|'<'|'~'|'&'|'|'|'^'|'!'|'=' => self.read_op(),
            '{'|'}'|'('|')'|'['|']'|'.'|','|':'|';' => {
                let begin = self.loc;
                self.loc.shift_col(1);
                self.next = self.src.next();
                Token::from_str(&c.to_string(), Span::new(begin, self.loc)).ok()
            }
            ' '|'\t'|'\r'|'\n' => {
                self.skip_whitespace();
                self.next()
            }
            _ => None
        }
    }
}

