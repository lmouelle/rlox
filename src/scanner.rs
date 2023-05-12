use std::{iter::Peekable, str::Chars};

struct Scanner<'a> {
    buff: &'a String,
    iter: &'a Peekable<Chars<'a>>,
    line: i32,
}

enum TokenType<'a> {
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
    Identifier(&'a String),
    String(&'a String),
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
    Error(&'a String),
    EOF,
    Comment(&'a String),
}

struct Token<'a> {
    line: i32,
    typ: TokenType<'a>,
}

impl<'a> Scanner<'a> {
    fn make_token(&self, typ: TokenType) -> Token {
        Token {
            line: self.line,
            typ,
        }
    }

    fn make_error(&self, errormsg: &String) -> Token {
        Token {
            line: self.line,
            typ: TokenType::Error(errormsg),
        }
    }

    fn new(buff: &String) -> Scanner {
        Scanner {
            buff,
            line: 1,
            iter: &buff.chars().peekable(),
        }
    }

    fn is_at_end(&self) -> bool {
        match self.iter.peek() {
            None => true,
            Some(_) => false,
        }
    }

    fn matches(&self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if let Some(c) = self.iter.next() {
            return true;
        }
        return false;
    }

    fn skip_whitespace(&self) {
        self.iter
            .skip_while(|x| x == &'\n' || x == &'\r' || x == &' ' || x == &'\t');
    }

    fn read_till_eol(&self) -> String {
        let mut buff = String::new();
        while let Some(c) = self.iter.next() {
            if c == '\n'
                || c == '\r' && self.iter.peek().expect("Expected newline after \\r") == &'\n'
            {
                break;
            }
            buff.push(c);
        }

        return buff;
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Token<'a>;

    // Equal to ScanToken in the CraftingInterpreters book
    fn next(&mut self) -> Option<Token<'a>> {
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
                None => self.make_error(&String::from("Unexpected EOF on char /")),
                Some('/') => {
                    let comment = self.read_till_eol();
                    self.make_token(TokenType::Comment(&comment))
                }
                Some(etc) => self.make_token(TokenType::Slash),
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
            Some(etc) => Some(self.make_error(&format!("Unexpected character {}", etc))),
        }
    }
}
