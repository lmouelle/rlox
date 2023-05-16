use std::{iter::Peekable, str::Chars};

pub struct Scanner<'a> {
    iter: Peekable<Chars<'a>>,
    line: i32,
}

#[derive(Debug, PartialEq)]
enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Semicolon,
    Dot,
    Dash,
    Plus,
    Star,
    Slash,
    Bang,
    BangEqual,
    EqualEqual,
    Equal,
    Greater,
    Lesser,
    GreaterEqual,
    LesserEqual,
    Identifier(String),
    String(String),
    Number(f64),
    And,
    Or,
    If,
    Else,
    False,
    For,
    While,
    Function,
    Nil,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    Error(String),
    EOF,
    Comment(String),
}

#[derive(Debug)]
pub struct Token {
    line: i32,
    typ: TokenType,
}

impl<'a> Scanner<'a> {
    fn advance_and_make_token(&mut self, typ: TokenType) -> Token {
        self.iter.next();
        Token {
            line: self.line,
            typ,
        }
    }

    fn make_error(&self, errormsg: String) -> Token {
        Token {
            line: self.line,
            typ: TokenType::Error(errormsg),
        }
    }

    fn new(buff: &'a String) -> Scanner<'a> {
        let iterator = buff.chars().peekable();
        Scanner {
            line: 1,
            iter: iterator,
        }
    }

    fn advance_and_advance_if_match(&mut self, expected: char) -> bool {
        let _initial_char = self.iter.next().expect("Peeked in caller");
        match self.iter.peek() {
            None => false,
            Some(c) => {
                if *c == expected {
                    self.iter.next();
                    true
                } else {
                    false
                }
            }
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.iter.peek() {
            if c.is_whitespace() {
                self.iter.next();
            } else {
                return;
            }
        }
    }

    fn advance_and_make_string_token(&mut self) -> Token {
        let mut buff = String::new();

        let initialdoublequote = self.iter.next().expect("Initial double quote expected");
        if initialdoublequote != '"' {
            panic!("Initial \" expected");
        }

        while let Some(c) = self.iter.next() {
            if c == '\n' {
                self.line += 1;
            }
            if c == '"' {
                return Token {
                    line: self.line,
                    typ: TokenType::String(buff),
                };
            }
            buff.push(c);
        }

        return self.make_error(String::from("Unterminated string"));
    }

    fn match_number_token(&mut self) -> Token {
        let mut buff = String::new();

        while let Some(c) = self.iter.peek() {
            if c.is_digit(10) || *c == '.' {
                buff.push(self.iter.next().expect("Already peeked"))
            } else {
                break;
            }
        }

        let result = buff.parse::<f64>();
        match result {
            Ok(v) => Token { line : self.line, typ : TokenType::Number(v)},
            Err(e) => self.make_error(format!("Failed to tokenize number {}", e)),
        }
    }

    fn match_identifier_or_keyword(&mut self) -> Token {
        let mut buff = String::new();

        let initial_letter = self.iter.next().expect("Peeked in caller method");
        buff.push(initial_letter);

        while let Some(c) = self.iter.peek() {
            if c.is_alphanumeric() {
                let item = self.iter.next().expect("Peeked already");
                buff.push(item);
            } else {
                break;
            }
        }
        return self.gen_token_type(buff);
    }

    fn gen_token_type(&mut self, buff: String) -> Token {
        match buff.as_str() {
            "and" => Token {
                line: self.line,
                typ: TokenType::And,
            },
            "else" => Token {
                line: self.line,
                typ: TokenType::Else,
            },
            "fun" => Token {
                line: self.line,
                typ: TokenType::Function,
            },
            "for" => Token {
                line: self.line,
                typ: TokenType::For,
            },
            "false" => Token {
                line: self.line,
                typ: TokenType::False,
            },
            "if" => Token {
                line: self.line,
                typ: TokenType::If,
            },
            "nil" => Token {
                line: self.line,
                typ: TokenType::Nil,
            },
            "or" => Token {
                line: self.line,
                typ: TokenType::Or,
            },
            "print" => Token {
                line: self.line,
                typ: TokenType::Print,
            },
            "true" => Token {
                line: self.line,
                typ: TokenType::True,
            },
            "this" => Token {
                line: self.line,
                typ: TokenType::This,
            },
            "return" => Token {
                line: self.line,
                typ: TokenType::Return,
            },
            "var" => Token {
                line: self.line,
                typ: TokenType::Var,
            },
            "while" => Token {
                line: self.line,
                typ: TokenType::While,
            },
            _ => Token {
                line: self.line,
                typ: TokenType::Identifier(buff),
            },
        }
    }

    /// Advances up to, and including, end of line character
    fn advance_thru_eol(&mut self) -> String {
        let mut buff = String::new();
        while let Some(c) = self.iter.next() {
            if c == '\n' {
                break;
            }
            buff.push(c);
        }

        return buff;
    }

    fn advance_and_handle_slash(&mut self) -> Token {
        let initial_slash = self.iter.next().expect("Peeked in caller");
        if initial_slash != '/' {
            panic!("Initial / expected");
        }

        match self.iter.peek() {
            None => self.make_error(String::from("Unexpected EOF on char /")),
            Some('/') => {
                let comment = self.advance_thru_eol();
                self.line += 1;
                Token {
                    line: self.line,
                    typ: TokenType::Comment(comment),
                }
            }
            Some(_) => Token {
                line: self.line,
                typ: TokenType::Slash,
            },
        }
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Token;

    // Equal to ScanToken in the CraftingInterpreters book
    fn next(&mut self) -> Option<Token> {
        self.skip_whitespace();

        match self.iter.peek() {
            None => None,
            Some(digit) if digit.is_digit(10) => Some(self.match_number_token()),
            Some(alpha) if alpha.is_alphabetic() => Some(self.match_identifier_or_keyword()),
            Some('"') => Some(self.advance_and_make_string_token()),
            Some('(') => Some(self.advance_and_make_token(TokenType::LeftParen)),
            Some(')') => Some(self.advance_and_make_token(TokenType::RightParen)),
            Some('{') => Some(self.advance_and_make_token(TokenType::LeftBrace)),
            Some('}') => Some(self.advance_and_make_token(TokenType::RightBrace)),
            Some(';') => Some(self.advance_and_make_token(TokenType::Semicolon)),
            Some(',') => Some(self.advance_and_make_token(TokenType::Comma)),
            Some('.') => Some(self.advance_and_make_token(TokenType::Dot)),
            Some('-') => Some(self.advance_and_make_token(TokenType::Dash)),
            Some('+') => Some(self.advance_and_make_token(TokenType::Plus)),
            Some('*') => Some(self.advance_and_make_token(TokenType::Star)),
            Some('/') => Some(self.advance_and_handle_slash()),
            Some('!') => Some(if self.advance_and_advance_if_match('=') {
                Token {
                    line: self.line,
                    typ: TokenType::BangEqual,
                }
            } else {
                Token {
                    line: self.line,
                    typ: TokenType::Bang,
                }
            }),
            Some('=') => Some(if self.advance_and_advance_if_match('=') {
                Token {
                    line: self.line,
                    typ: TokenType::EqualEqual,
                }
            } else {
                Token {
                    line: self.line,
                    typ: TokenType::Equal,
                }
            }),
            Some('>') => Some(if self.advance_and_advance_if_match('=') {
                Token {
                    line: self.line,
                    typ: (TokenType::GreaterEqual),
                }
            } else {
                Token {
                    line: self.line,
                    typ: (TokenType::Greater),
                }
            }),
            Some('<') => Some(if self.advance_and_advance_if_match('=') {
                Token {
                    line: self.line,
                    typ: TokenType::LesserEqual,
                }
            } else {
                Token {
                    line: self.line,
                    typ: TokenType::Lesser,
                }
            }),
            Some(etc) => Some(Token {
                line: self.line,
                typ: TokenType::Error(format!("Unexpected character {}", etc)),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::scanner::TokenType;

    use super::{Scanner, Token};

    impl Token {
        fn assert_token_type(&self, t: TokenType) {
            assert_eq!(
                self.typ, t,
                "Expected token of type {:?}, got {:?}",
                t, self.typ
            );
        }
    }

    #[test]
    fn keyword_test_1() {
        let buff = String::from("while (false) { x = x + 1}");
        let mut scanner = Scanner::new(&buff);

        scanner
            .next()
            .expect("Expected while")
            .assert_token_type(TokenType::While);
        scanner
            .next()
            .expect("Expected lparen")
            .assert_token_type(TokenType::LeftParen);
        scanner
            .next()
            .expect("Expected false")
            .assert_token_type(TokenType::False);
        scanner
            .next()
            .expect("Expected rparen")
            .assert_token_type(TokenType::RightParen);
        scanner
            .next()
            .expect("Expected Lbrace")
            .assert_token_type(TokenType::LeftBrace);
        scanner
            .next()
            .expect("Expected first x ref")
            .assert_token_type(TokenType::Identifier("x".to_owned()));
        scanner
            .next()
            .expect("Expected eq")
            .assert_token_type(TokenType::Equal);
        scanner
            .next()
            .expect("Expected second x ref")
            .assert_token_type(TokenType::Identifier("x".to_owned()));
        scanner
            .next()
            .expect("Expected plus")
            .assert_token_type(TokenType::Plus);
        scanner
            .next()
            .expect("Expected one")
            .assert_token_type(TokenType::Number(1.0));
        scanner
            .next()
            .expect("Expected rbrace")
            .assert_token_type(TokenType::RightBrace);
        assert!(scanner.next().is_none(), "Expected end of expression");
    }

    #[test]
    fn identifier_token() {
        let buff = String::from("foo - bar + 2");
        let mut scanner = Scanner::new(&buff);

        scanner.next().expect("Expected lhs").assert_token_type(TokenType::Identifier("foo".to_owned()));
        scanner.next().expect("Expected minus").assert_token_type(TokenType::Dash);
        scanner.next().expect("Expected bar").assert_token_type(TokenType::Identifier("bar".to_owned()));
        scanner.next().expect("Expected plus").assert_token_type(TokenType::Plus);
        scanner.next().expect("Expected 2").assert_token_type(TokenType::Number(2.0));
        assert!(scanner.next().is_none(), "Expected end of statement");
    }

    #[test]
    fn number_token() {
        let buff = String::from("11222.654 * \n2");
        let mut scanner = Scanner::new(&buff);

        scanner.next().expect("Expected decimal value").assert_token_type(TokenType::Number(11222.654));
        scanner.next().expect("Expected star value").assert_token_type(TokenType::Star);
        scanner.next().expect("Expected numeric value on lhs").assert_token_type(TokenType::Number(2.0));
        assert!(scanner.next().is_none(), "Expected end of expression");
    }

    #[test]
    fn comment() {
        let buff = String::from("1 // foo bar baz \n + 2");
        let mut scanner = Scanner::new(&buff);

        let one = scanner
            .next()
            .expect("Expected single digit in multi line statement");
        let _ = scanner
            .next()
            .expect("Expected comment token to be generated");
        let plus = scanner
            .next()
            .expect("Expected plus in multi line statement");
        let two = scanner
            .next()
            .expect("Expected operand in multiline statement");
        let end = scanner.next();

        assert!(end.is_none());

        assert_eq!(one.typ, TokenType::Number(1.0));
        assert_eq!(plus.typ, TokenType::Plus);
        assert_eq!(two.typ, TokenType::Number(2.0));
        assert_eq!(two.line, 2);
    }

    #[test]
    fn string_literal() {
        let buff = String::from("\"foo bar\n baz\""); // multiline, whitespace-having string
        let mut scanner = Scanner::new(&buff);

        let string = scanner.next().expect("Expected single string in buffer");
        let end = scanner.next();

        assert!(end.is_none());

        assert_eq!(string.typ, TokenType::String(String::from("foo bar\n baz")));
        assert_eq!(string.line, 2);
    }

    #[test]
    fn basic_addition() {
        let buff = String::from(" 1 +1");
        let mut scanner = Scanner::new(&buff);

        let lhs = scanner.next().expect("Expected lhs of 1+1");
        let plus = scanner.next().expect("Expected plus of 1+1");
        let rhs = scanner.next().expect("Expected rhs of 1+1");
        let end = scanner.next();

        assert_eq!(lhs.line, 1);
        assert_eq!(rhs.line, 1);
        assert_eq!(plus.line, 1);

        assert!(end.is_none());

        assert_eq!(lhs.typ, TokenType::Number(1.0));
        assert_eq!(rhs.typ, TokenType::Number(1.0));
        assert_eq!(plus.typ, TokenType::Plus);
    }
}
