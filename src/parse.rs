use core::fmt;
use std::iter::Peekable;

use anyhow::{anyhow, Error};

use crate::lex::{Lexer, Token, TokenType};

#[derive(Debug, Clone)]
pub enum AstNode {
    Literal(LiteralValue),
    Grouping(Grouping),
    Unary(Operator, Box<AstNode>),
    Binary(Box<AstNode>, Operator, Box<AstNode>),
    Eof,
}

impl fmt::Display for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstNode::Literal(v) => write!(f, "{v}"),
            AstNode::Grouping(v) => write!(f, "{v}"),
            AstNode::Unary(o, v) => write!(f, "({o} {v})"),
            AstNode::Binary(l, o, r) => write!(f, "({o} {l} {r})"),
            AstNode::Eof => write!(f, ""),
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
    pub expression: Box<AstNode>,
}

impl fmt::Display for Grouping {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(group {})", self.expression)
    }
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

    pub fn parse(&mut self) -> Result<AstNode, Error> {
        self.comparison()
    }

    pub fn comparison(&mut self) -> Result<AstNode, Error> {
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
            expr = AstNode::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<AstNode, Error> {
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
            expr = AstNode::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<AstNode, Error> {
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
            expr = AstNode::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<AstNode, Error> {
        let token = self.peek()?;
        let operator = match token.kind {
            TokenType::BANG => Operator::Bang,
            TokenType::MINUS => Operator::Minus,
            _ => return self.primary(),
        };
        self.lexer.next();

        let right = self.unary()?;
        Ok(AstNode::Unary(operator, Box::new(right)))
    }

    fn primary(&mut self) -> Result<AstNode, Error> {
        let token = self.peek()?;
        let expr = match token.kind {
            TokenType::FALSE => AstNode::Literal(LiteralValue::Bool(false)),
            TokenType::TRUE => AstNode::Literal(LiteralValue::Bool(true)),
            TokenType::NIL => AstNode::Literal(LiteralValue::Nil),
            TokenType::NUMBER(n) => AstNode::Literal(LiteralValue::Number(n)),
            TokenType::STRING => AstNode::Literal(LiteralValue::String(token.clone().origin)),
            TokenType::LeftParen => {
                self.lexer.next();
                let expr = self.comparison()?;
                let token = self.peek()?;
                if token.kind == TokenType::RightParen {
                    self.lexer.next();
                    return Ok(AstNode::Grouping(Grouping {
                        expression: Box::new(expr),
                    }));
                }

                return Err(anyhow!("[line {}] Expect ')' after expression", token.line));
            }
            _ => return Err(anyhow!("")),
        };
        self.lexer.next();

        Ok(expr)
    }

    pub fn peek(&mut self) -> Result<Token, Error> {
        match self.lexer.peek() {
            Some(Ok(token)) => Ok(token.clone()),
            Some(Err(e)) => Err(anyhow!("{}", e)),
            _ => Ok(Token {
                kind: TokenType::NIL,
                origin: "".to_string(),
                line: 0,
            }),
        }
    }
}
