use crate::lexer::Token;
use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    String(String),
    Number(f64),
    Bool(bool),
    Null,
    Array(Vec<Value>),
    Object(BTreeMap<String, Value>),
}

#[derive(Debug, Clone)]
pub struct ParserError {
    pub msg: String,
}

impl ParserError {
    pub fn new(msg: &str) -> ParserError {
        ParserError {
            msg: msg.to_string(),
        }
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, index: 0 }
    }

    fn parse_array(&mut self) -> Result<Value, ParserError> {
        let token = self.peek_expect()?;
        if *token != Token::LeftBracket {
            return Err(ParserError::new(&format!(
                "error: JSON array must starts [ {:?}",
                token
            )));
        }
        self.next_expect()?;

        let mut array = vec![];

        let token = self.peek_expect()?;

        if *token == Token::RightBracket {
            return Ok(Value::Array(array));
        }

        loop {
            let value = self.parse()?;
            array.push(value);

            let token = self.next_expect()?;
            match token {
                Token::RightBracket => {
                    return Ok(Value::Array(array));
                }
                Token::Comma => {
                    continue;
                }
                _ => {
                    return Err(ParserError::new(&format!(
                        "error: a [ or , token is expected {:?}",
                        token
                    )));
                }
            }
        }
    }

    fn parse_object(&mut self) -> Result<Value, ParserError> {
        let token = self.peek_expect()?;
        if *token != Token::LeftBrace {
            return Err(ParserError::new(&format!(
                "error: JSON object must starts {{ {:?}",
                token
            )));
        }
        self.next_expect()?;

        let mut object = std::collections::BTreeMap::new();

        if *self.peek_expect()? == Token::RightBrace {
            return Ok(Value::Object(object));
        }

        loop {
            let token1 = self.next_expect()?.clone();
            let token2 = self.next_expect()?;

            match (token1, token2) {
                (Token::String(key), Token::Colon) => {
                    object.insert(key, self.parse()?);
                }
                _ => {
                    return Err(ParserError::new(
                        "error: a pair (key(string) and : token) token is expected",
                    ));
                }
            }

            let token3 = self.next_expect()?;
            match token3 {
                Token::RightBrace => {
                    return Ok(Value::Object(object));
                }
                Token::Comma => {
                    continue;
                }
                _ => {
                    return Err(ParserError::new(&format!(
                        "error: a {{ or , token is expected {:?}",
                        token3
                    )));
                }
            }
        }
    }

    pub fn parse(&mut self) -> Result<Value, ParserError> {
        let token = self.peek_expect()?.clone();

        match token {
            Token::LeftBrace => self.parse_object(),
            Token::LeftBracket => self.parse_array(),
            Token::String(string) => {
                self.next_expect()?;
                Ok(Value::String(string))
            }
            Token::Number(number) => {
                self.next_expect()?;
                Ok(Value::Number(number))
            }
            Token::Bool(boolean) => {
                self.next_expect()?;
                Ok(Value::Bool(boolean))
            }
            Token::Null => {
                self.next_expect()?;
                Ok(Value::Null)
            }
            _ => Err(ParserError::new(&format!(
                "error: a token must start {{ or [ or string or number or bool or null {:?}",
                token
            ))),
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    fn peek_expect(&self) -> Result<&Token, ParserError> {
        self.peek()
            .ok_or_else(|| ParserError::new("error: a token isn't peekable"))
    }

    fn next(&mut self) -> Option<&Token> {
        self.index += 1;
        self.tokens.get(self.index - 1)
    }

    fn next_expect(&mut self) -> Result<&Token, ParserError> {
        self.next()
            .ok_or_else(|| ParserError::new("error: a token isn't peekable"))
    }
}
