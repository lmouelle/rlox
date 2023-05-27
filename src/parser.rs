use std::{fmt::Debug, iter::Peekable};

use crate::scanner::{Scanner, Token, TokenType};

struct Parser<'a> {
    panicking: bool,
    scanner: Peekable<Scanner<'a>>,
}

type LineNum = i32;

#[derive(Debug)]
enum Value {
    Number(f64),
    Nil,
    Boolean(bool),
    String(String),
    Variable(String),
    Error(String),
}

#[derive(Debug)]
enum Expression {
    Value(LineNum, Value),
    If(LineNum, Box<Expression>, Box<Expression>, Box<Expression>),
    Or(LineNum, Box<Expression>, Box<Expression>),
    And(LineNum, Box<Expression>, Box<Expression>),
    // Binary ops
    Add(LineNum, Box<Expression>, Box<Expression>),
    Subtract(LineNum, Box<Expression>, Box<Expression>),
    Multiply(LineNum, Box<Expression>, Box<Expression>),
    Divide(LineNum, Box<Expression>, Box<Expression>),
    // Equality
    Equals(LineNum, Box<Expression>, Box<Expression>),
    NotEquals(LineNum, Box<Expression>, Box<Expression>),
    // Comparison
    Greater(LineNum, Box<Expression>, Box<Expression>),
    Lesser(LineNum, Box<Expression>, Box<Expression>),
    LesserEqual(LineNum, Box<Expression>, Box<Expression>),
    GreaterEqual(LineNum, Box<Expression>, Box<Expression>),
    // Misc
    Grouping(LineNum, Box<Expression>),
    Not(LineNum, Box<Expression>),
    Negate(LineNum, Box<Expression>),
    Invocation(LineNum, String, Vec<Expression>),
}

enum Statement {
    Assignment(LineNum, String, Expression),
    Mutation(LineNum, String, Expression),
    Print(LineNum, Expression),
    While(LineNum, Expression, Box<Statement>),
    FunctionDef(LineNum, Vec<String>, Box<Statement>),
}

type Ast = Vec<Statement>;

impl TokenType {
    fn infix_binding_power(&self) -> Option<(u8, u8)> {
        match self {
            TokenType::Plus | TokenType::Dash => Some((1, 2)),
            TokenType::Star | TokenType::Slash => Some((3, 4)),
            _ => None,
        }
    }

    fn prefix_binding_power(&self) -> u8 {
        match self {
            TokenType::Plus | TokenType::Dash => 5,
            etc => panic!("Bad argument to prefix_binding_power: {:?}", etc)
        }
    }
}

impl<'a> Parser<'a> {
    fn parse_numeric_expr(&mut self, min_bp: u8) -> Expression {
        let mut lhs = match self.scanner.next() {
            Some(Token {
                line,
                typ: TokenType::Number(n),
            }) => Expression::Value(line, Value::Number(n)),
            Some(Token { line, typ}) if typ == TokenType::Dash => {
                let r_bp = typ.prefix_binding_power();
                let rhs = self.parse_numeric_expr(r_bp);
                Expression::Negate(line, Box::new(rhs))
            },
            Some(Token { line, typ: TokenType::LeftParen }) => {
                let expr = self.parse_numeric_expr(0);
                match self.scanner.next() {
                    Some(Token {typ: TokenType::RightParen, .. }) => {
                        Expression::Grouping(line, Box::new(expr))
                    },
                    Some(tok) => Expression::Value(tok.line, Value::Error(format!("Unexpected token {:?} in parsing grouping", tok.typ))),
                    // TODO: Insert proper line num for EOF
                    None => Expression::Value(0, Value::Error("Unexpected EOF".to_owned()))
                }
            }
            Some(Token { line, typ }) => Expression::Value(
                line,
                Value::Error(format!("Unexpected token {:?} on parsing binary op", typ)),
            ),
            None => Expression::Value(
                0,
                Value::Error("Unexpected EOF on parsing binary op".to_owned()),
            ), // TODO: Insert proper line nums for tokens
        };

        loop {
            let op = match self.scanner.peek() {
                None => break,
                Some(Token { typ, .. }) => typ
            };

            if let Some((l_bp, r_bp)) = op.infix_binding_power() {
                if l_bp < min_bp {
                    break;
                }
                let Token { line, typ } = self.scanner.next().expect("Already peeked");
    
                let rhs = self.parse_numeric_expr(r_bp);
                lhs = match typ {
                    TokenType::Plus => Expression::Add(line, Box::new(lhs), Box::new(rhs)),
                    TokenType::Dash => Expression::Subtract(line, Box::new(lhs), Box::new(rhs)),
                    TokenType::Star => Expression::Multiply(line, Box::new(lhs), Box::new(rhs)),
                    TokenType::Slash => Expression::Divide(line, Box::new(lhs), Box::new(rhs)),
                    etc => {
                        return Expression::Value(
                            line,
                            Value::Error(format!(
                                "Unexpected token {:?} when parsing binary operation",
                                etc
                            )),
                        )
                    }
                };

            }

            break;
        }
        lhs
    }

    fn new(scanner: Scanner) -> Parser {
        Parser {
            panicking: false,
            scanner: scanner.peekable(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::scanner::Scanner;

    use super::{Expression, Parser, Value};

    #[test]
    fn binary_op_test_1() {
        let buff = String::from("1 + 2");
        let scanner = Scanner::new(&buff);
        let mut parser = Parser::new(scanner);

        let result = parser.parse_numeric_expr(0);
        match result {
            Expression::Add(_, lhs, rhs) => match (*lhs, *rhs) {
                (
                    Expression::Value(_, Value::Number(n1)),
                    Expression::Value(_, Value::Number(n2)),
                ) => {
                    assert_eq!(n1, 1.0);
                    assert_eq!(n2, 2.0);
                }
                (unexpectedlhs, unexpectedrhs) => {
                    panic!(
                        "Expected 2 numbers, found {:?} and {:?}",
                        unexpectedlhs, unexpectedrhs
                    );
                }
            },
            etc => panic!("Expected Add expr, found {:?}", etc),
        }
    }

    #[test]
    fn binary_op_test_2() {
        let buff = String::from("1 + 2 * 3");
        let scanner = Scanner::new(&buff);
        let mut parser = Parser::new(scanner);

        let result = parser.parse_numeric_expr(0);
        // TODO: This is sooooo ugly, find a better way to automate nested pattern matching
        match result {
            Expression::Add(_, lhs, rhs) => match *rhs {
                Expression::Multiply(_, addlhs, addrhs) => match (*addlhs, *addrhs) {
                    (
                        Expression::Value(_, Value::Number(n1)),
                        Expression::Value(_, Value::Number(n2)),
                    ) => {
                        match *lhs {
                            Expression::Value(_, Value::Number(n3)) => {
                                assert_eq!(n3, 1.0);
                            }
                            etc => panic!("Expected lhs to be numeric literal, found {:?}", etc),
                        }
                        assert_eq!(n1, 2.0);
                        assert_eq!(n2, 3.0);
                    }
                    (unexpectedlhs, unexpectedrhs) => {
                        panic!(
                            "Expected 2 numbers, found {:?} and {:?}",
                            unexpectedlhs, unexpectedrhs
                        );
                    }
                },
                etc => panic!("Expected inner expr to be multiply, found {:?}", etc),
            },
            etc => panic!("Expected add expr, found {:?}", etc),
        }
    }

    #[test]
    fn numeric_literal() {
        // TODO: Not sure if I need parse_binary_op to have this behavior?
        let buff = String::from("1");
        let scanner = Scanner::new(&buff);
        let mut parser = Parser::new(scanner);

        let result = parser.parse_numeric_expr(0);
        match result {
            Expression::Value(_, Value::Number(n)) => assert_eq!(n, 1.0),
            etc => panic!("Expected single value, found {:?}", etc),
        }
    }

    #[test]
    fn negate_expression_1() {
        let buff = String::from("-1");
        let scanner = Scanner::new(&buff);
        let mut parser = Parser::new(scanner);

        let result = parser.parse_numeric_expr(0);
        match result {
            Expression::Negate(_, lhs) => {
                match *lhs {
                    Expression::Value(_, Value::Number(n)) => {
                        assert_eq!(n, 1.0);
                    },
                    etc => panic!("Expected val expr, receied {:?}", etc)
                }
            },
            etc => panic!("Expected negation, receied {:?}", etc)
        }
    }

    #[test]
    fn parenthized_exprs () {
        let buff = String::from("((0 * 1))");
        let scanner = Scanner::new(&buff);
        let mut parser = Parser::new(scanner);

        let result = parser.parse_numeric_expr(0);
        match result {
            Expression::Grouping(_, expr) => {
                match *expr {
                    Expression::Grouping(_, innerexpr) => {
                        match *innerexpr {
                            Expression::Multiply(_, mult_lhs, mult_rhs) => {
                                match (*mult_lhs, *mult_rhs) {
                                    (Expression::Value(_, Value::Number(lhs_n)), Expression::Value(_, Value::Number(rhs_n))) => {
                                        assert_eq!(lhs_n, 0.0);
                                        assert_eq!(rhs_n, 1.0);
                                    }
                                    (etc_lhs, etc_rhs) => panic!("Expected two numbers, found {:?} and {:?}", etc_lhs, etc_rhs)
                                }
                            },
                            etc => panic!("Unexpected expr {:?}, expected multiply", etc)
                        }
                    },
                    etc => panic!("Unexpected expr {:?}, expected grouping", etc)
                }
            },
            etc => panic!("Unexpected expr {:?}, expected grouping", etc)
        }
    }

    #[test]
    fn negate_expression_2() {
        let buff = String::from("2 + -1");
        let scanner = Scanner::new(&buff);
        let mut parser = Parser::new(scanner);

        let result = parser.parse_numeric_expr(0);
        match result {
            Expression::Add(_, lhs, rhs) => {
                match *lhs {
                    Expression::Value(_, Value::Number(lhs_n)) => {
                        match *rhs {
                            Expression::Negate(_, negate_rhs) => {
                                match *negate_rhs {
                                    Expression::Value(_, Value::Number(negate_rhs_n)) => {
                                        assert_eq!(lhs_n, 2.0);
                                        assert_eq!(negate_rhs_n, 1.0);
                                    },
                                    etc => panic!("Expected numeric value right of negation, found {:?}", etc)
                                }
                            },
                            etc => panic!("Expected negation, found {:?}", etc)
                        }
                    },
                    etc => panic!("Expected numeral expr, found {:?}", etc)
                }
            },
            etc => panic!("Expected addition expr, found {:?}" , etc)
        }
    }    
}
