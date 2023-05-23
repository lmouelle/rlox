use std::iter::Peekable;

use crate::scanner::{Token, Scanner, TokenType};

struct Parser<'a> {
    panicking : bool,
    scanner : Peekable<Scanner<'a>>,
}

type LineNum = i32;

enum Value {
    Number(LineNum, f64),
    Nil(LineNum),
    Boolean(LineNum, bool),
    String(LineNum, String),
    Variable(LineNum, String),
    Error(LineNum, String)
}

enum Expression<'a> {
    Value(LineNum, Value),
    If(LineNum, &'a Expression<'a>, &'a Expression<'a>, &'a Expression<'a>),
    Or(LineNum, &'a Expression<'a>,  &'a Expression<'a>),
    And(LineNum,  &'a Expression<'a>,  &'a Expression<'a>),
    // Binary ops
    Add(LineNum,  &'a Expression<'a>,  &'a Expression<'a>),
    Subtract(LineNum,  &'a Expression<'a>,  &'a Expression<'a>),
    Multiply(LineNum,  &'a Expression<'a>,  &'a Expression<'a>),
    Divide(LineNum,  &'a Expression<'a>,  &'a Expression<'a>),
    // Equality
    Equals(LineNum,  &'a Expression<'a>,  &'a Expression<'a>),
    NotEquals(LineNum,  &'a Expression<'a>,  &'a Expression<'a>),
    // Comparison
    Greater(LineNum,  &'a Expression<'a>,  &'a Expression<'a>),
    Lesser(LineNum,  &'a Expression<'a>,  &'a Expression<'a>),
    LesserEqual(LineNum,  &'a Expression<'a>,  &'a Expression<'a>),
    GreaterEqual(LineNum,  &'a Expression<'a>,  &'a Expression<'a>),
    // Misc
    Grouping(LineNum,  &'a Expression<'a>),
    Not(LineNum,  &'a Expression<'a>),
    Negate(LineNum,  &'a Expression<'a>),
    Invocation(LineNum, String, Vec<Expression<'a>>)
}

enum Statement<'a> {
    Assignment(LineNum, String, Expression<'a>),
    Mutation(LineNum, String, Expression<'a>),
    Print(LineNum, Expression<'a>),
    While(LineNum, Expression<'a>, &'a Statement<'a>),
    FunctionDef(LineNum, Vec<String>, &'a Statement<'a>)
}

type Ast<'a> = Vec<Statement<'a>>;

impl  TokenType {
    fn is_bin_op(&self) -> bool {
        match *self {
            TokenType::Plus | TokenType::Star | TokenType::Slash | TokenType::Dash => true,
            _ => false
        }
    }

    fn infix_binding_power(&self) -> (u8, u8) {
        match self {
            TokenType::Plus | TokenType::Dash => (1,2),
            TokenType::Star | TokenType::Slash => (3,4),
            etc => panic!("Bad argument to infix_binding_power: {:?}", etc)
        }
    }
}

impl Parser {
    fn parse_ast(&self) -> Ast {
    }

    fn parse_expr(&self) -> Expression {

    }

    fn parse_statement(&self) -> Statement {

    }

    fn parse_bin_op(&self) -> Expression {
        let lhs = match self.scanner.next() {
            Some(Token {line, typ : TokenType::Number(n) }) => n,
            None => panic!("Unexpected EOF on parsing binary op"),
            Some(etc) => panic!("Unexpected token {:?} on parsing binary op", etc)
        };

        loop {
            let op = match self.scanner.peek() {
                None => break,
                Some(Token {line, typ }) if typ.is_bin_op() => typ,
                Some(etc) => panic!("Unexpected token {:?} when parsing binary operation", etc)
            };

            let (l_bp, r_bp) = op.infix_binding_power();
        }
    }

    fn parse_value(&mut self) -> Value {
        let token = self.scanner.next(); // TODO: Peek or Next?
        match token {
            None => { 
                self.panicking = true;
                Value::Error(0, format!("Unexpected EOF while parsing value")) 
            },
            Some(Token {typ : TokenType::Number(n), line }) => Value::Number(line, n),
            Some(Token {typ : TokenType::Nil, line }) => Value::Nil(line),
            Some(Token {typ : TokenType::True, line }) => Value::Boolean(line, true),
            Some(Token {typ : TokenType::False, line }) => Value::Boolean(line, false),
            Some(Token { line, typ: TokenType::String(s) }) => Value::String(line, s),
            Some(Token { line, typ : TokenType::Identifier(s) }) => Value::Variable(line, s),
            Some(Token { line, typ }) => {
                self.panicking = true;
                Value::Error(line, format!("Unexpected token in value parser: {:?}", typ))
            }
        }
    }

    fn new(scanner: Scanner) -> Parser {
        Parser { panicking: false, scanner : scanner.peekable() }
    }

    
}

#[cfg(test)]
mod tests {

}