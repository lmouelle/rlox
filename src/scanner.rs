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

#[derive(Debug)]
struct Token {
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
            Some(c) => if *c == expected {
                self.iter.next();
                true
            } else {
                false
            }
        }
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
                return Token { line: self.line, typ: TokenType::String(buff)};
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
            }
            else {
                break;
            }
        }

        let result = buff.parse::<f64>();
        match result {
            Ok(v) => self.advance_and_make_token(TokenType::Number(v)),
            Err(e) => self.make_error(format!("Failed to tokenize number {}", e))
        }
    }

    fn match_identifier_token(&mut self) -> Token {
        let mut buff = String::new();

        let initial_letter = self.iter.next().expect("Peeked in caller method");
        buff.push(initial_letter);

        while let Some(c) = self.iter.peek() {
            if c.is_alphanumeric() {
                let item = self.iter.next().expect("Peeked already");
                buff.push(item);
            }
            else {
                break;
            }
        }

        Token { line: self.line,  typ: TokenType::Identifier(buff) }
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
                Token { line : self.line, typ : TokenType::Comment(comment) }
            }
            Some(_) => Token { line : self.line, typ : TokenType::Slash },
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
            Some(alpha) if alpha.is_alphabetic() => Some(self.match_identifier_token()),
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
                Token { line : self.line, typ : TokenType::BangEqual }
            } else {
                Token { line : self.line, typ : TokenType::Bang }
            }),
            Some('=') => Some(if self.advance_and_advance_if_match('=') {
                Token { line : self.line, typ : TokenType::EqualEqual }
            } else {
                Token { line : self.line, typ : TokenType::Equal }
            }),
            Some('>') => Some(if self.advance_and_advance_if_match('=') {
                Token {line : self.line , typ : (TokenType::GreaterEqual) }
            } else {
                Token {line : self.line , typ : (TokenType::Greater) }
            }),
            Some('<') => Some(if self.advance_and_advance_if_match('=') {
                Token { line : self.line, typ : TokenType::LesserEqual }
            } else {
                Token { line : self.line, typ : TokenType::Lesser }
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

    use super::Scanner;

    #[test]
    fn identifier_token() {
        let buff = String::from("foo - bar + 2");
        let mut scanner = Scanner::new(&buff);

        let foo = scanner.next().expect("Expected lhs");
        let less = scanner.next().expect("Expected minus");
        let bar = scanner.next().expect("Expected bar");
        let plus = scanner.next().expect("Expected plus");
        let two = scanner.next().expect("Expected 2");
        let end = scanner.next();

        assert!(end.is_none());

        assert_eq!(foo.typ, TokenType::Identifier(String::from("foo")));
        assert_eq!(less.typ, TokenType::Dash);
        assert_eq!(bar.typ, TokenType::Identifier(String::from("bar")));
        assert_eq!(plus.typ, TokenType::Plus);
        assert_eq!(two.typ, TokenType::Number(2.0));
    }

    #[test]
    fn number_token() {
        let buff = String::from("11222.654 * \n2");
        let mut scanner = Scanner::new(&buff);

        let decimal = scanner.next().expect("Expected decimal value");
        let star = scanner.next().expect("Expected star value");
        let integer = scanner.next().expect("Expected numeric value on lhs");
        let end = scanner.next();

        assert!(end.is_none());

        assert_eq!(decimal.typ, TokenType::Number(11222.654));
        assert_eq!(star.typ, TokenType::Star);
        assert_eq!(integer.typ, TokenType::Number(2.0));
    }

    #[test]
    fn comment () {
        let buff = String::from("1 // foo bar baz \n + 2");
        let mut scanner = Scanner::new(&buff);

        let one = scanner.next().expect("Expected single digit in multi line statement");
        let _ = scanner.next().expect("Expected comment token to be generated");
        let plus = scanner.next().expect("Expected plus in multi line statement");
        let two = scanner.next().expect("Expected operand in multiline statement");
        let end = scanner.next();

        assert!(end.is_none());

        assert_eq!(one.typ, TokenType::Number(1.0));
        assert_eq!(plus.typ, TokenType::Plus);
        assert_eq!(two.typ, TokenType::Number(2.0));
        assert_eq!(two.line, 2);
    }

    #[test]
    fn string_literal () {
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