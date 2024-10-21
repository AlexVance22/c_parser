use crate::ast::span::Span;
use std::fmt::Display;


#[derive(Debug, Default, Clone, PartialEq)]
pub enum Token{
    // single-character tokens
    LeftParen, RightParen, LeftBrace, RightBrace,
    LeftBrack, RightBrack, Comma, Dot, Colon, Semicolon,
    Add, Sub, Mul, Div, Mod, Equal, Not, Less, Greater, 
    BitAnd, BitOr, BitXor, BitNot,

    // double-character tokens
    LogAnd, LogOr, LShift, RShift,
    AddEq, SubEq, MulEq, DivEq, ModEq,
    GreaterEq, LessEq, BangEq, EqualEq,
    AndEq, OrEq, XorEq,
    Arrow, Inc, Dec,

    LShiftEq, RShiftEq, 

    // keywords
    Struct, Switch, If, Else, For, While, Break, Continue,
    Def, Return, Const, Case, Owned, Typedef, Sizeof, Goto,

    //types
    #[default]
    Void,
    Signed, Unsigned, Short, Long, Bool, Char, Int, Float, Double,
  
    // literals.
    Boolean(bool), Integer(i64), Floating(f64), Character(char), StrLit(String),

    Identifier(String),

    EndOfFile,
}


impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Token::*;

        write!(f, "{}", 
        match self {
            LeftParen => "(", RightParen => ")", LeftBrace => "{", RightBrace => "}",
            LeftBrack => "[", RightBrack => "]", Comma     => ",", Dot =>        ".",
            Colon     => ":", Semicolon =>  ";", Add       => "+", Sub =>        "-",
            Mul       => "*", Div =>        "/", Mod       => "%", Equal =>      "=",
            Not       => "!", Less =>       "<", Greater   => ">", BitAnd =>     "&",
            BitOr     => "|", BitXor =>     "^", BitNot    => "~",

            LogAnd => "&&", LogOr   => "||", LShift => "<<", RShift    => ">>",
            AddEq  => "+=", SubEq   => "-=", MulEq  => "*=", DivEq     => "/=",
            ModEq  => "%=", AndEq   => "&=", OrEq   => "|=", XorEq     => "^=",
            BangEq => "!=", EqualEq => "==", LessEq => "<=", GreaterEq => ">=",
            Arrow  => "->", Inc     => "++", Dec    => "--",

            LShiftEq => "<<=",  RShiftEq => ">>=",

            If      => "if",      Else  => "else",  For   => "for",   While    => "while",
            Switch  => "switch",  Case  => "case",  Break => "break", Def      => "default",
            Return  => "return",  Const => "const", Owned => "owned", Typedef  => "typedef", 
            Sizeof  => "sizeof",  Goto  => "goto",  Continue => "continue",

            Bool  => "_Bool", Char => "char", Int => "int", Float => "float", Double => "double",
            Short => "short", Long => "long", Signed => "signed", Unsigned => "unsigned",
            Void  => "void",  Struct => "struct",

            EndOfFile => "[EOF]",

            _ => ""
        }
        )?;

        match &self {
            Boolean(b)     => write!(f, "{}", b),
            Integer(i)     => write!(f, "{}", i),
            Floating(fl)   => write!(f, "{}", fl),
            Character(c)   => write!(f, "{}", c),
            StrLit(s)      => write!(f, "{:?}", s),
            Identifier(id) => write!(f, "{}", id),

            _ => write!(f, "")
        }
    }
}

impl Token {
    pub fn from_str(lexeme: &str, span: Span) -> Result<(Self, Span), ()> {
        use Token::*;

        Ok((
            if lexeme.starts_with('"') && lexeme.ends_with('"') {
                StrLit(lexeme.strip_prefix('"').unwrap()
                             .strip_suffix('"').unwrap()
                             .to_string())
            } else if lexeme.starts_with('\'') && lexeme.ends_with('\'') {
                Character(lexeme.chars().nth(1).unwrap())
            } else if let Some(lex) = lexeme.strip_prefix("0x") {
                Integer(i64::from_str_radix(lex, 16).map_err(|_| ())?)
            } else if let Some(lex) = lexeme.strip_prefix("0b") {
                Integer(i64::from_str_radix(lex, 2).map_err(|_| ())?)
            } else if let Ok(val) = lexeme.parse::<i64>() {
                Integer(val)
            } else if let Ok(val) = lexeme.parse::<f64>() {
                Floating(val)
            } else {
                match lexeme {
                    "(" => LeftParen,   ")" => RightParen, "{" => LeftBrace, "}" => RightBrace,
                    "[" => LeftBrack,   "]" => RightBrack, "," => Comma,     "." => Dot,
                    ":" => Colon,       ";" => Semicolon,  "+" => Add,       "-" => Sub,
                    "*" => Mul,         "/" => Div,        "%" => Mod,       "=" => Equal,
                    "!" => Not,         "<" => Less,       ">" => Greater,   "&" => BitAnd,
                    "|" => BitOr,       "^" => BitXor,     "~" => BitNot,

                    "&&" => LogAnd,     "||" => LogOr,     "<<" => LShift,   ">>" => RShift,
                    "+=" => AddEq,      "-=" => SubEq,     "*=" => MulEq,    "/=" => DivEq,
                    "%=" => ModEq,      "&=" => AndEq,     "|=" => OrEq,     "^=" => XorEq,
                    "!=" => BangEq,     "==" => EqualEq,   "<=" => LessEq,   ">=" => GreaterEq,
                    "->" => Arrow,      "++" => Inc,       "--" => Dec,

                    "<<=" => LShiftEq,  ">>=" => RShiftEq,

                    "if"     => If,     "else"  => Else,   "for"   => For,   "while" => While,
                    "switch" => Switch, "case"  => Case,   "break" => Break, "default" => Def,
                    "return" => Return, "const" => Const,  "owned" => Owned, "typedef" => Typedef, 
                    "sizeof" => Sizeof, "goto"  => Goto,   "continue" => Continue,
                    "true"   => Boolean(true),             "false" => Boolean(false),

                    "_Bool" => Bool,  "char"   => Char, "int" => Int, "float" => Float, "double" => Double,
                    "short" => Short, "long"   => Long, "signed" => Signed, "unsigned" => Unsigned,
                    "void"  => Void,  "struct" => Struct,

                    _ => Identifier(lexeme.to_string())
                }
            },
            span
        ))
    }
}

