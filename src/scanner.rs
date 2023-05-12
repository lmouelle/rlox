use std::{iter::Peekable, str::Chars};

struct Scanner<'a> {
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

struct Token {
    line: i32,
    typ: TokenType,
}

impl<'a> Scanner<'a> {
    fn make_token(&self, typ: TokenType) -> Token {
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

    fn is_at_end(&mut self) -> bool {
        match self.iter.peek() {
            None => true,
            Some(_) => false,
        }
    }

    fn matches(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if let Some(c) = self.iter.next() {
            return c == expected;
        }
        return false;
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.iter.peek() {
            if c.is_whitespace() {
                self.iter.next();
            }
            else {
                return;
            }
        }
    }

    fn read_till_eol(&mut self) -> String {
        let mut buff = String::new();
        while let Some(c) = self.iter.next() {
            if c == '\n'
                || c == '\r' && *self.iter.peek().expect("Expected newline after \\r") == '\n'
            {
                break;
            }
            buff.push(c);
        }

        return buff;
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Token;

    // Equal to ScanToken in the CraftingInterpreters book
    fn next(&mut self) -> Option<Token> {
        self.skip_whitespace();

        match self.iter.next() {
            None => None,
            Some('(') => Some(self.make_token(TokenType::LeftParen)),
            Some(')') => Some(self.make_token(TokenType::RightParen)),
            Some('{') => Some(self.make_token(TokenType::LeftBrace)),
            Some('}') => Some(self.make_token(TokenType::RightBrace)),
            Some(';') => Some(self.make_token(TokenType::Semicolon)),
            Some(',') => Some(self.make_token(TokenType::Comma)),
            Some('.') => Some(self.make_token(TokenType::Dot)),
            Some('-') => Some(self.make_token(TokenType::Dash)),
            Some('+') => Some(self.make_token(TokenType::Plus)),
            Some('*') => Some(self.make_token(TokenType::Star)),
            Some('/') => Some(match self.iter.peek() {
                None => self.make_error(String::from("Unexpected EOF on char /")),
                Some('/') => {
                    let comment = self.read_till_eol();
                    self.line += 1;
                    self.make_token(TokenType::Comment(comment))
                }
                Some(_) => self.make_token(TokenType::Slash),
            }),
            Some('!') => Some(if self.matches('=') {
                self.make_token(TokenType::BangEqual)
            } else {
                self.make_token(TokenType::Bang)
            }),
            Some('=') => Some(if self.matches('=') {
                self.make_token(TokenType::EqualEqual)
            } else {
                self.make_token(TokenType::Equal)
            }),
            Some('>') => Some(if self.matches('=') {
                self.make_token(TokenType::GreaterEqual)
            } else {
                self.make_token(TokenType::Greater)
            }),
            Some('<') => Some(if self.matches('=') {
                self.make_token(TokenType::LesserEqual)
            } else {
                self.make_token(TokenType::Lesser)
            }),
            Some(etc) => Some(self.make_error(format!("Unexpected character {}", etc))),
        }
    }
}


#[cfg(test)]
mod tests {
    use crate::scanner::TokenType;

    use super::Scanner;

    #[test]
    fn comment () {
        let buff = String::from("1 // foo bar baz \n + 2");
        let mut scanner = Scanner::new(&buff);

        let one = scanner.next().expect("Expected single digit in multi line statement");
        let plus = scanner.next().expect("Expected plus in multi line statement");
        let two = scanner.next().expect("Expected operand in multiline statement");
        let end = scanner.next();

        match end {
            None => (),
            Some(t) => panic!("Expected end of expression, found {:?} on line {}", t.typ, t.line)
        }

        assert_eq!(one.typ, TokenType::Number(1.0));
        assert_eq!(plus.typ, TokenType::Plus);
        assert_eq!(two.typ, TokenType::Number(2.0))
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

        match end {
            None => (),
            Some(tok) => panic!("Expected none for end of statement, found {:?} on line {}", tok.typ, tok.line)
        }

        assert_eq!(lhs.typ, TokenType::Number(1.0));
        assert_eq!(rhs.typ, TokenType::Number(1.0));
        assert_eq!(plus.typ, TokenType::Plus);
    }
}