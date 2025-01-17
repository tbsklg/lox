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
    Identifier(String),
    Assign(String, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Print(Expr),
    Expression(Expr),
    Var(String, Option<Expr>),
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Print(e) => write!(f, "{e}"),
            Stmt::Expression(e) => write!(f, "{e}"),
            Stmt::Var(n, e) => match e {
                Some(e) => write!(f, "{n} {e}"),
                None => write!(f, "{n}"),
            },
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
            Expr::Identifier(n) => write!(f, "{n}"),
            Expr::Assign(n, v) => write!(f, "{n} {v}"),
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
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.is_at_end() {
            match self.declaration()? {
                Some(s) => statements.push(s),
                None => continue,
            }
        }

        Ok(statements)
    }

    fn is_at_end(&mut self) -> bool {
        matches!(self.peek(), Ok(token) if token.kind == TokenType::EOF)
    }

    fn declaration(&mut self) -> Result<Option<Stmt>, Error> {
        match self.peek()?.kind {
            TokenType::VAR => {
                self.lexer.next();
                self.var_declaration()
            }
            _ => self.statement(),
        }
    }

    fn consume(&mut self, expected_token: TokenType, msg: String) -> Result<Token, Error> {
        let next_token = self.lexer.next().unwrap()?;

        if next_token.kind != expected_token {
            return Err(anyhow!(msg));
        }

        Ok(next_token)
    }

    fn var_declaration(&mut self) -> Result<Option<Stmt>, Error> {
        let identifier_token =
            self.consume(TokenType::IDENTIFIER, "Expected variable name".to_string())?;

        let initializer = match self.peek()?.kind {
            TokenType::EQUAL => {
                self.lexer.next();
                Some(self.expression()?)
            }
            _ => None,
        };

        let _ = self.consume(
            TokenType::SEMICOLON,
            "Expected ';' after variable declaration.".to_string(),
        );

        Ok(Some(Stmt::Var(identifier_token.origin, initializer)))
    }

    fn statement(&mut self) -> Result<Option<Stmt>, Error> {
        match self.peek()?.kind {
            TokenType::PRINT => {
                self.lexer.next();
                self.print_statement()
            }
            _ => self.expression_statement(),
        }
    }

    fn print_statement(&mut self) -> Result<Option<Stmt>, Error> {
        let expr = self.expression()?;
        match self.peek()?.kind {
            TokenType::SEMICOLON => {
                self.lexer.next();
                Ok(Some(Stmt::Print(expr)))
            }
            TokenType::EOF => Ok(Some(Stmt::Expression(expr))),
            _ => Err(anyhow!("Expected semicolon after expression")),
        }
    }

    fn expression_statement(&mut self) -> Result<Option<Stmt>, Error> {
        let expr = self.expression()?;
        match self.peek()?.kind {
            TokenType::SEMICOLON => {
                self.lexer.next();
                Ok(Some(Stmt::Expression(expr)))
            }
            TokenType::EOF => Ok(Some(Stmt::Print(expr))),
            _ => Err(anyhow!("Expected semicolon after expression")),
        }
    }

    fn expression(&mut self) -> Result<Expr, Error> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, Error> {
        let expr = self.comparison()?;
        match self.peek()?.kind {
            TokenType::EQUAL => {
                self.lexer.next();
                let value = self.assignment()?;

                match expr {
                    Expr::Identifier(n) => Ok(Expr::Assign(n, Box::new(value))),
                    _ => Err(anyhow!("Invalid assignment target.")),
                }
            }
            _ => Ok(expr),
        }
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
            TokenType::IDENTIFIER => {
                let name = token.clone().origin.trim_matches('"').to_string();
                Expr::Identifier(name)
            }
            _ => return Err(anyhow!("[primary] Unexpected token: {:?}", token.kind)),
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
