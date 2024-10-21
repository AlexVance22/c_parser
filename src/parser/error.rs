use std::fmt::Display;
use thiserror::Error;
use crate::ast::span::Span;


#[derive(Debug, Clone, Error, Default)]
pub enum ParserError {
    #[error("{0}: unexpected end of file reached in {1}")]
    EndOfFileIn(Span, &'static str),
    #[error("{0}: unexpected tokens in {1}")]
    BadTokensIn(Span, &'static str),
    #[error("{0}: {1} must have {2}")]
    ElemIsMissing(Span, &'static str, &'static str),
    #[error("{0}: type contains contradictory type specifiers")]
    TypeContradiction(Span),
    #[error("{0}: expression contains errors")]
    GenericExprError(Span),
    #[error("an unknown error occurred")]
    #[default]
    UnknownError,
}

#[derive(Debug, Clone, Error, Default)]
pub struct ParserErrors {
    errors: Vec<ParserError>,
    eof: bool,
}

impl Display for ParserErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for err in &self.errors {
            writeln!(f, "{}", err)?;
        }
        Ok(())
    }
}



impl ParserErrors {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, err: ParserError) {
        if let ParserError::EndOfFileIn(..) = err {
            self.eof = true;
        }
        self.errors.push(err);
    }

    pub fn extend(&mut self, errs: impl IntoIterator<Item=ParserError>) {
        for err in errs {
            if let ParserError::EndOfFileIn(..) = err {
                self.eof = true;
            }
            self.errors.push(err);
        }
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn wrap<T>(self, val: T) -> Result<T, Self> {
        if self.is_empty() {
            Ok(val)
        } else {
            Err(self)
        }
    }

    pub fn eof(&mut self) -> Result<(), Self> {
        if self.eof {
            Err(std::mem::take(self))
        } else {
            Ok(())
        }
    }

    pub fn end_file(mut self, span: Span, loc: &'static str) -> Self {
        self.eof = true;
        self.errors.push(ParserError::EndOfFileIn(span, loc));
        self
    }
}

impl IntoIterator for ParserErrors {
    type Item = ParserError;
    type IntoIter = <Vec::<ParserError> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.errors.into_iter()
    }
}


macro_rules! seek_to {
    ($tokens:expr, $(($t:pat, $s:pat) => $b:block)+) => { {
        let mut __next = $tokens.next();
        loop {
            match __next {
                Some(($($t)|+, _)) => break,
                _ => __next = $tokens.next(),
            }
        }
        match __next.unwrap() {
            $(($t, $s) => $b),+
            _ => unreachable!(),
        }
    } };
}


pub(crate) use seek_to;


#[cfg(test)]
mod tests {
    use super::ParserError;

    #[test]
    fn test_seek_to() -> Result<(), Vec<ParserError>> {
        use crate::lexer::{Lexer, Token};
        let mut tokens = Lexer::new("");
        let mut errs = Vec::new();

        seek_to!(&mut tokens,
            (Token::Semicolon, span) => { errs.push(ParserError::BadTokensIn(span, "function declaration")); return Err(errs) }
            (Token::LeftParen, _) => { return Err(errs) }
        )
    }
}

