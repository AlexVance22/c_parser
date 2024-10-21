mod lexer;
mod ast;
mod parser;

use std::fs;


fn main() {
    let src = fs::read_to_string("res/test.cx").unwrap();

    let tokens = lexer::Lexer::new(&src);

    let ast = match parser::parse(tokens) {
        Ok(ast) => ast,
        Err(err) => {
            println!("{}", err);
            return
        }
    };

    println!("{:#?}", ast);
}

