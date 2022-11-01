mod lexer;
mod parser;

use crate::lexer::Lexer;
use crate::parser::{Parser, ParserError};

pub fn exec() {
    let object = match Lexer::new(
        r#"
            {
                "number": 123,
                "boolean": true,
                "string": "togatoga",
                "object": {
                "number": 2E10
                }
            }
        "#,
    )
    .tokenize()
    {
        Ok(tokens) => Parser::new(tokens).parse(),
        Err(e) => Err(ParserError::new(&e.msg)),
    };
    println!("{:?}", object.unwrap());
}

#[cfg(test)]
mod exec_tests {
    use super::exec;

    #[test]
    fn exec_test() {
        exec();
    }
}
