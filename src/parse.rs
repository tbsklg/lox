use core::fmt;
use std::iter::Peekable;

use anyhow::Error;

use crate::lex::{Lexer, TokenType};

#[derive(Debug)]
pub struct Expression {
    literal: String,
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.literal)
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

    pub fn parse(&mut self) -> Result<Expression, Error> {
        let c = self.lexer.next();

        let e = match c {
            Some(t) => match t {
                Ok(tt) => match tt {
                    TokenType::TRUE => Expression { literal: true.to_string() },
                    TokenType::FALSE => Expression { literal: false.to_string() },
                    TokenType::NIL => Expression { literal: "nil".to_string() },
                    TokenType::NUMBER(n) => Expression { literal: n.to_string() },
                    _ => todo!(),
                }
                Err(_e) => todo!(),
            }
            None => todo!(),
        };

        Ok(e)
    }
}

