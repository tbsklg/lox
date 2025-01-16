use core::fmt;
use std::iter::Peekable;

use crate::lex::{Lexer, Token, TokenType};

use anyhow::{anyhow, Error};

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(LiteralValue),
    Grouping(Box<Expr>),
    Unary(Operator, Box<Expr>),
    Binary(Box<Expr>, Operator, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Print(Expr),
    Expression(Expr),
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Print(e) => write!(f, "{e}"),
            Stmt::Expression(e) => write!(f, "{e}"),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(v) => write!(f, "{v}"),
            Expr::Grouping(v) => write!(f, "(group {})", v),
            Expr::Unary(o, v) => write!(f, "({o} {v})"),
            Expr::Binary(l, o, r) => write!(f, "({o} {l} {r})"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    String(String),
    Bool(bool),
    Nil,
    Number(f64),
}

impl fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let literal = match self {
            LiteralValue::String(s) => s.trim_matches('"'),
            LiteralValue::Bool(s) => &s.to_string(),
            LiteralValue::Nil => "nil",
            LiteralValue::Number(n) => &format!("{:?}", n),
        };
        write!(f, "{}", literal)
    }
}

#[derive(Debug, Clone)]
pub struct Grouping {
    pub expression: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum Operator {
    Minus,
    Bang,
    Plus,
    Multi,
    Div,
    BangEqual,
    EqualEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Operator::Minus => write!(f, "-"),
            Operator::Bang => write!(f, "!"),
            Operator::Plus => write!(f, "+"),
            Operator::Multi => write!(f, "*"),
            Operator::Div => write!(f, "/"),
            Operator::BangEqual => write!(f, "!="),
            Operator::EqualEqual => write!(f, "=="),
            Operator::Less => write!(f, "<"),
            Operator::LessEqual => write!(f, "<="),
            Operator::Greater => write!(f, ">"),
            Operator::GreaterEqual => write!(f, ">="),
        }
    }
}

pub struct Parser<'e> {
    lexer: Peekable<Lexer<'e>>,
}

impl<'e> Parser<'e> {
    pub fn new(input: &'e str) -> Self {
        Self {
            lexer: Lexer::new(input).peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, Error> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            statements.push(self.statement()?);
        }
        Ok(statements)
    }

    fn is_at_end(&mut self) -> bool {
        matches!(self.peek(), Ok(token) if token.kind == TokenType::EOF)
    }

    fn statement(&mut self) -> Result<Stmt, Error> {
        if matches!(self.peek()?.kind, TokenType::PRINT) {
            self.lexer.next();
            self.print_statement()
        } else {
            self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> Result<Stmt, Error> {
        let expr = self.expression()?;
        match self.peek()?.kind {
            TokenType::SEMICOLON => {
                self.lexer.next();
                Ok(Stmt::Print(expr))
            }
            TokenType::EOF => Ok(Stmt::Expression(expr)),
            _ => Err(anyhow!("Expected semicolon after expression")),
        }
    }

    fn expression_statement(&mut self) -> Result<Stmt, Error> {
        let expr = self.expression()?;
        Ok(Stmt::Expression(expr))
    }

    fn expression(&mut self) -> Result<Expr, Error> {
        self.comparison()
    }

    pub fn comparison(&mut self) -> Result<Expr, Error> {
        let mut expr = self.term()?;

        while matches!(
            self.peek()?.kind,
            TokenType::GREATER
                | TokenType::GREATEREQUAL
                | TokenType::LESS
                | TokenType::LESSEQUAL
                | TokenType::EQUALEQUAL
                | TokenType::BANGEQUAL
        ) {
            let token = self.peek()?;
            let operator = match token.kind {
                TokenType::EQUALEQUAL => Operator::EqualEqual,
                TokenType::BANGEQUAL => Operator::BangEqual,
                TokenType::GREATER => Operator::Greater,
                TokenType::GREATEREQUAL => Operator::GreaterEqual,
                TokenType::LESS => Operator::Less,
                TokenType::LESSEQUAL => Operator::LessEqual,
                _ => return Ok(expr),
            };
            self.lexer.next();

            let right = self.term()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, Error> {
        let mut expr = self.factor()?;

        while matches!(self.peek()?.kind, TokenType::MINUS | TokenType::PLUS) {
            let token = self.peek()?;
            let operator = match token.kind {
                TokenType::MINUS => Operator::Minus,
                TokenType::PLUS => Operator::Plus,
                _ => return Ok(expr),
            };
            self.lexer.next();

            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, Error> {
        let mut expr = self.unary()?;

        while matches!(self.peek()?.kind, TokenType::SLASH | TokenType::STAR) {
            let token = self.peek()?;
            let operator = match token.kind {
                TokenType::SLASH => Operator::Div,
                TokenType::STAR => Operator::Multi,
                _ => return Ok(expr),
            };
            self.lexer.next();

            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, Error> {
        let token = self.peek()?;
        let operator = match token.kind {
            TokenType::BANG => Operator::Bang,
            TokenType::MINUS => Operator::Minus,
            _ => return self.primary(),
        };
        self.lexer.next();

        let right = self.unary()?;
        Ok(Expr::Unary(operator, Box::new(right)))
    }

    fn primary(&mut self) -> Result<Expr, Error> {
        let token = self.peek()?;
        let expr = match token.kind {
            TokenType::FALSE => Expr::Literal(LiteralValue::Bool(false)),
            TokenType::TRUE => Expr::Literal(LiteralValue::Bool(true)),
            TokenType::NIL => Expr::Literal(LiteralValue::Nil),
            TokenType::NUMBER(n) => Expr::Literal(LiteralValue::Number(n)),
            TokenType::STRING => Expr::Literal(LiteralValue::String(
                token.clone().origin.trim_matches('"').to_string(),
            )),
            TokenType::LeftParen => {
                self.lexer.next();
                let expr = self.comparison()?;
                let token = self.peek()?;
                if token.kind == TokenType::RightParen {
                    self.lexer.next();
                    return Ok(Expr::Grouping(Box::new(expr)));
                }

                return Err(anyhow!("[line {}] Expect ')' after expression", token.line));
            }
            _ => return Err(anyhow!("Unexpected token: {:?}", token.kind)),
        };
        self.lexer.next();

        Ok(expr)
    }

    pub fn peek(&mut self) -> Result<Token, Error> {
        match self.lexer.peek() {
            Some(Ok(token)) => Ok(token.clone()),
            Some(Err(e)) => Err(anyhow!("{}", e)),
            None => Ok(Token {
                origin: "".to_string(),
                kind: TokenType::EOF,
                line: u32::MAX,
            }),
        }
    }
}
