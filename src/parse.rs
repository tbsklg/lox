use core::fmt;
use std::iter::Peekable;

use anyhow::Error;

use crate::lex::{Lexer, TokenType};

#[derive(Debug)]
pub enum AstNode {
    Literal(LiteralValue),
    Grouping(Grouping),
}

impl fmt::Display for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstNode::Literal(l) => write!(f, "{}", l),
            AstNode::Grouping(g) => write!(f, "{}", g),
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
            LiteralValue::String(s) => &s.to_string(),
            LiteralValue::Bool(s) => &s.to_string(),
            LiteralValue::Nil => &"nil".to_string(),
            LiteralValue::Number(n) => &format!("{:?}", n),
        };
        write!(f, "{:?}", literal)
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
        let c = self.lexer.next();

        loop {
            match c {
                Some(t) => match t {
                    Ok(tt) => match tt {
                        TokenType::STRING(s) => {
                            return Ok(AstNode::Literal(LiteralValue::String(s.to_string())))
                        }
                        TokenType::NUMBER(n) => {
                            return Ok(AstNode::Literal(LiteralValue::Number(n)))
                        }
                        TokenType::TRUE => {
                            return Ok(AstNode::Literal(LiteralValue::Bool(true)))
                        }
                        TokenType::FALSE => {
                            return Ok(AstNode::Literal(LiteralValue::Bool(false)))
                        }
                        TokenType::NIL => return Ok(AstNode::Literal(LiteralValue::Nil)),
                        TokenType::LeftParen => {
                            let expression = self.parse()?;
                            let _ = self.lexer.next();
                            return Ok(AstNode::Grouping(Grouping {
                                expression: Box::new(expression),
                            }));
                        }
                        _ => todo!(),
                    },
                    Err(_e) => todo!(),
                },
                None => todo!(),
            };
        }
    }
}
