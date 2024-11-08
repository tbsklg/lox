use core::fmt;
use std::iter::Peekable;

use anyhow::Error;

use crate::lex::{Lexer, Token, TokenType};

#[derive(Debug)]
pub enum AstNode {
    Literal(LiteralValue),
    Grouping(Grouping),
    Unary(Operator),
}

impl fmt::Display for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstNode::Literal(l) => write!(f, "{}", l),
            AstNode::Grouping(g) => write!(f, "{}", g),
            AstNode::Unary(o) => write!(f, "{}", o),
        }
    }
}

#[derive(Debug)]
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
            LiteralValue::Nil => &"nil".to_string(),
            LiteralValue::Number(n) => &format!("{:?}", n),
        };
        write!(f, "{}", literal)
    }
}

#[derive(Debug)]
pub struct Grouping {
    expression: Box<AstNode>,
}

impl fmt::Display for Grouping {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(group {})", self.expression)
    }
}

#[derive(Debug)]
pub enum Operator {
    Minus(Box<AstNode>),
    Bang(Box<AstNode>),
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Operator::Minus(e) => write!(f, "(- {})", e),
            Operator::Bang(e) => write!(f, "(! {})", e),
        }
    }
}

pub struct Parser<'e> {
    input: &'e str,
    lexer: Peekable<Lexer<'e>>,
}

impl<'e> Parser<'e> {
    pub fn new(input: &'e str) -> Self {
        Self {
            input,
            lexer: Lexer::new(input).peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<AstNode, Error> {
        let token = match self.lexer.next() {
            Some(Ok(tt)) => tt,
            None => return Err(anyhow::anyhow!("Unexpected EOF")),
            Some(Err(e)) => return Err(e),
        };

        loop {
            let t = match token {
                Token {
                    kind: TokenType::STRING,
                    origin,
                } => AstNode::Literal(LiteralValue::String(origin.to_string())),
                Token {
                    kind: TokenType::NUMBER(n),
                    ..
                } => AstNode::Literal(LiteralValue::Number(n)),
                Token {
                    kind: TokenType::TRUE,
                    ..
                } => AstNode::Literal(LiteralValue::Bool(true)),
                Token {
                    kind: TokenType::FALSE,
                    ..
                } => AstNode::Literal(LiteralValue::Bool(false)),
                Token {
                    kind: TokenType::NIL,
                    ..
                } => AstNode::Literal(LiteralValue::Nil),
                Token {
                    kind: TokenType::LeftParen,
                    ..
                } => {
                    let expression = self.parse()?;
                    let t = self.lexer.next();

                    match t {
                        Some(Ok(Token {
                            kind: TokenType::RightParen,
                            ..
                        })) => (),
                        _ => return Err(anyhow::anyhow!("Expected ')'")),
                    }

                    AstNode::Grouping(Grouping {
                        expression: Box::new(expression),
                    })
                }
                Token {
                    kind: TokenType::BANG,
                    ..
                } => {
                    let expression = self.parse()?;

                    AstNode::Unary(Operator::Bang(Box::new(expression)))
                },
                Token {
                    kind: TokenType::MINUS,
                    ..
                } => {
                    let expression = self.parse()?;

                    AstNode::Unary(Operator::Minus(Box::new(expression)))
                    
                },
                _ => return Err(anyhow::anyhow!("Unexpected token {}", token)),
            };

            return Ok(t);
        }
    }
}


