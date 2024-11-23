use core::fmt;
use std::iter::Peekable;

use anyhow::{anyhow, Error};

use crate::lex::{Lexer, Token, TokenType};

#[derive(Debug)]
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
            AstNode::Unary(o, v) => write!(f, "{o}{v}"),
            AstNode::Binary(l, o, r) => write!(f, "({o} {l} {r})"),
            AstNode::Eof => write!(f, ""),
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
        write!(f, "( {} )", self.expression)
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
            Operator::BangEqual => write!(f, "/"),
            Operator::EqualEqual => write!(f, "/"),
            Operator::Less => write!(f, "/"),
            Operator::LessEqual => write!(f, "/"),
            Operator::Greater => write!(f, "/"),
            Operator::GreaterEqual => write!(f, "/"),
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
        match self.expression() {
            Ok(ast) => Ok(ast),
            Err(_) => Err(anyhow!("Failed to parse")),
        }
    }

    pub fn expression(&mut self) -> Result<AstNode, ()> {
        let mut expr = self.term()?;
        
        while matches!(self.peek().unwrap_or(&Token { kind: TokenType::NIL, origin: "".to_string() }).kind, TokenType::PLUS | TokenType::MINUS) {
            let operator = match self.peek() {
                Some(token) => match token.kind {
                    TokenType::PLUS => Operator::Plus,
                    TokenType::MINUS => Operator::Minus,
                    _ => return Ok(expr),
                },
                None => return Ok(expr),
            };
            self.lexer.next();
            
            let right = self.term()?;
            expr = AstNode::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<AstNode, ()> {
        let mut expr = self.factor()?;
            
        while matches!(self.peek().unwrap_or(&Token { kind: TokenType::NIL, origin: "".to_string() }).kind, TokenType::SLASH | TokenType::STAR) {
            let operator = match self.peek() {
                Some(token) => match token.kind {
                    TokenType::SLASH => Operator::Div,
                    TokenType::STAR => Operator::Multi,
                    _ => return Ok(expr),
                },
                None => return Ok(expr),
            };
            self.lexer.next();
            
            let right = self.factor()?;
            expr = AstNode::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<AstNode, ()> {
        let mut expr = self.unary()?;

        match self.peek() {
            Some(token) => {
                let operator = match token.kind {
                    TokenType::SLASH => Operator::Div,
                    TokenType::STAR=> Operator::Multi,
                    _ => return Ok(expr),
                };
                self.lexer.next();
                
                let right = self.unary()?;
                expr = AstNode::Binary(Box::new(expr), operator, Box::new(right));
            },
            None => return Ok(expr),
        }
        
        Ok(expr)
    }

    fn unary(&mut self) -> Result<AstNode, ()> {
        match self.peek() {
            Some(token) => {
                let operator = match token.kind {
                    TokenType::BANG=> Operator::Bang,
                    TokenType::MINUS=> Operator::Minus,
                    _ => return self.primary(),
                };
                self.lexer.next();
                
                let right = self.unary()?;
                return Ok(AstNode::Unary(operator, Box::new(right)));
            },
            None => return self.primary(),
        }
    }

    fn primary(&mut self) -> Result<AstNode, ()> {
        match self.peek() {
            Some(token) => {
                let expr = match token.kind {
                    TokenType::FALSE => AstNode::Literal(LiteralValue::Bool(false)),
                    TokenType::TRUE => AstNode::Literal(LiteralValue::Bool(true)),
                    TokenType::NIL => AstNode::Literal(LiteralValue::Nil),
                    TokenType::NUMBER(n) => AstNode::Literal(LiteralValue::Number(n)),
                    TokenType::STRING => AstNode::Literal(LiteralValue::String(token.clone().origin)),
                    TokenType::LeftParen => {
                        self.lexer.next();
                        let expr = self.expression()?;
                        match self.peek() {
                            Some(token) => {
                                if token.kind == TokenType::RightParen {
                                    self.lexer.next();
                                    return Ok(AstNode::Grouping(Grouping { expression: Box::new(expr) }));
                                }
                            },
                            None => return Err(()),
                        }
                        return Err(());
                    }
                    _ => AstNode::Eof,
                };
                self.lexer.next();
                
                return Ok(expr)
            },
            None => Ok(AstNode::Eof),
        }
    }

    pub fn peek(&mut self) -> Option<&Token> {
        match self.lexer.peek() {
            Some(Ok(token)) => Some(&token),
            _ => None
        }
    }
}

