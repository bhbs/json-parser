#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    String(String),
    Number(f64),
    Bool(bool),
    Null,
    WhiteSpace,
    NewLine,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Colon,
}

pub struct Lexer<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
}

#[derive(Debug)]
pub struct LexerError {
    pub msg: String,
}

impl LexerError {
    fn new(msg: &str) -> LexerError {
        LexerError {
            msg: msg.to_string(),
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        Lexer {
            chars: input.chars().peekable(),
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = vec![];
        while let Some(token) = self.next_token()? {
            match token {
                Token::WhiteSpace => {}
                _ => {
                    tokens.push(token);
                }
            }
        }

        Ok(tokens)
    }

    fn next_return_token(&mut self, token: Token) -> Option<Token> {
        self.chars.next();
        Some(token)
    }

    fn next_token(&mut self) -> Result<Option<Token>, LexerError> {
        match self.chars.peek() {
            Some(char) => match char {
                char if char.is_whitespace() => Ok(self.next_return_token(Token::WhiteSpace)),
                '\n' => Ok(self.next_return_token(Token::NewLine)),
                '{' => Ok(self.next_return_token(Token::LeftBrace)),
                '}' => Ok(self.next_return_token(Token::RightBrace)),
                '[' => Ok(self.next_return_token(Token::LeftBracket)),
                ']' => Ok(self.next_return_token(Token::RightBracket)),
                ',' => Ok(self.next_return_token(Token::Comma)),
                ':' => Ok(self.next_return_token(Token::Colon)),
                '"' => {
                    self.chars.next();
                    self.parse_string_token()
                }
                char if char.is_numeric() || matches!(char, '+' | '-' | '.') => {
                    self.parse_number_token()
                }
                't' => self.parse_token("true"),
                'f' => self.parse_token("false"),
                'n' => self.parse_token("null"),
                _ => Err(LexerError::new(&format!(
                    "error: an unexpected char {}",
                    char
                ))),
            },
            None => Ok(None),
        }
    }

    fn parse_token(&mut self, token: &str) -> Result<Option<Token>, LexerError> {
        let str = (0..token.len())
            .filter_map(|_| self.chars.next())
            .collect::<String>();

        if str != token {
            Err(LexerError::new(&format!(
                "error: a value is expected {}",
                token
            )))
        } else {
            match token {
                "true" => Ok(Some(Token::Bool(true))),
                "false" => Ok(Some(Token::Bool(false))),
                "null" => Ok(Some(Token::Null)),
                _ => Err(LexerError::new(&format!(
                    "error: a token {} is expected",
                    token
                ))),
            }
        }
    }

    // https://www.rfc-editor.org/rfc/rfc8259#section-6
    fn parse_number_token(&mut self) -> Result<Option<Token>, LexerError> {
        let mut number_str = String::new();
        while let Some(&c) = self.chars.peek() {
            if c.is_numeric() | matches!(c, '+' | '-' | 'e' | 'E' | '.') {
                self.chars.next();
                number_str.push(c);
            } else {
                break;
            }
        }

        match number_str.parse::<f64>() {
            Ok(number) => Ok(Some(Token::Number(number))),
            Err(error) => Err(LexerError::new(&format!("error: {}", error))),
        }
    }

    // https://www.rfc-editor.org/rfc/rfc8259#section-7
    fn parse_string_token(&mut self) -> Result<Option<Token>, LexerError> {
        let mut utf16 = vec![];
        let mut result = String::new();

        while let Some(char1) = self.chars.next() {
            match char1 {
                '\\' => {
                    let char2 = self
                        .chars
                        .next()
                        .ok_or_else(|| LexerError::new("error: a next char is expected"))?;
                    if matches!(char2, '"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't') {
                        Self::push_utf16(&mut result, &mut utf16)?;
                        result.push('\\');
                        result.push(char2);
                    } else if char2 == 'u' {
                        let hexs = (0..4)
                            .filter_map(|_| {
                                let char = self.chars.next()?;
                                if char.is_ascii_hexdigit() {
                                    Some(char)
                                } else {
                                    None
                                }
                            })
                            .collect::<Vec<_>>();

                        match u16::from_str_radix(&hexs.iter().collect::<String>(), 16) {
                            Ok(code_point) => utf16.push(code_point),
                            Err(error) => {
                                return Err(LexerError::new(&format!(
                                    "error: a unicode character is expected {}",
                                    error
                                )))
                            }
                        };
                    } else {
                        return Err(LexerError::new(&format!(
                            "error: an unexpected escaped char {}",
                            char2
                        )));
                    }
                }
                '\"' => {
                    Self::push_utf16(&mut result, &mut utf16)?;
                    return Ok(Some(Token::String(result)));
                }
                _ => {
                    Self::push_utf16(&mut result, &mut utf16)?;
                    result.push(char1);
                }
            }
        }
        Ok(None)
    }

    fn push_utf16(result: &mut String, utf16: &mut Vec<u16>) -> Result<(), LexerError> {
        if utf16.is_empty() {
            return Ok(());
        }
        match String::from_utf16(utf16) {
            Ok(utf16_str) => {
                result.push_str(&utf16_str);
                utf16.clear();
            }
            Err(error) => {
                return Err(LexerError::new(&format!("error: {}", error)));
            }
        };
        Ok(())
    }
}

#[test]
fn test_number() {
    //integer
    let num = "1234567890";
    let tokens = Lexer::new(num).tokenize().unwrap();
    assert_eq!(tokens[0], Token::Number(1234567890f64));

    let num = "+123";
    let tokens = Lexer::new(num).tokenize().unwrap();
    assert_eq!(tokens[0], Token::Number(123f64));

    //float
    let num = "-0.001";
    let tokens = Lexer::new(num).tokenize().unwrap();
    assert_eq!(tokens[0], Token::Number(-0.001));

    let num = ".001";
    let tokens = Lexer::new(num).tokenize().unwrap();
    assert_eq!(tokens[0], Token::Number(0.001));

    // exponent
    let num = "1e-10";
    let tokens = Lexer::new(num).tokenize().unwrap();
    assert_eq!(tokens[0], Token::Number(0.0000000001));

    let num = "+2E10";
    let tokens = Lexer::new(num).tokenize().unwrap();
    assert_eq!(tokens[0], Token::Number(20000000000f64));
}

#[test]
fn test_string() {
    let s = "\"togatoga123\"";
    let tokens = Lexer::new(s).tokenize().unwrap();
    assert_eq!(tokens[0], Token::String("togatoga123".to_string()));

    let s = r#" " \b \f \n \r \t \/ \" ""#.to_string();
    let tokens = Lexer::new(&s).tokenize().unwrap();
    assert_eq!(
        tokens[0],
        Token::String(r#" \b \f \n \r \t \/ \" "#.to_string())
    );

    let s = r#""\uD83D\uDE04\uD83D\uDE07\uD83D\uDC7A""#;
    let tokens = Lexer::new(s).tokenize().unwrap();
    assert_eq!(tokens[0], Token::String(r#"😄😇👺"#.to_string()));
}

#[test]
fn test_tokenize() {
    let object = r#"
        {
            "number": 123,
            "boolean": true,
            "string": "togatoga",
            "object": {
               "number": 2E10
            }
         }
         "#;

    let tokens = Lexer::new(object).tokenize().unwrap();
    let result_tokens = [
        Token::LeftBrace,
        Token::String("number".to_string()),
        Token::Colon,
        Token::Number(123f64),
        Token::Comma,
        Token::String("boolean".to_string()),
        Token::Colon,
        Token::Bool(true),
        Token::Comma,
        Token::String("string".to_string()),
        Token::Colon,
        Token::String("togatoga".to_string()),
        Token::Comma,
        Token::String("object".to_string()),
        Token::Colon,
        Token::LeftBrace,
        Token::String("number".to_string()),
        Token::Colon,
        Token::Number(20000000000f64),
        Token::RightBrace,
        Token::RightBrace,
    ];

    tokens
        .iter()
        .zip(result_tokens.iter())
        .enumerate()
        .for_each(|(i, (x, y))| {
            assert_eq!(x, y, "index: {}", i);
        });

    let object = "[true, {\"key\": null}]";
    let tokens = Lexer::new(object).tokenize().unwrap();
    let result_tokens = vec![
        Token::LeftBracket,
        Token::Bool(true),
        Token::Comma,
        Token::LeftBrace,
        Token::String("key".to_string()),
        Token::Colon,
        Token::Null,
        Token::RightBrace,
        Token::RightBracket,
    ];

    tokens
        .iter()
        .zip(result_tokens.iter())
        .for_each(|(x, y)| assert_eq!(x, y));
}
