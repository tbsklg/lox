use core::fmt;

use anyhow::Error;

use crate::parse::{AstNode, LiteralValue};

pub struct Evaluator {
    ast: AstNode,
}

pub enum Evaluation {
    Bool(bool),
    String(String),
    Number(f64),
    Nil,
}

impl fmt::Display for Evaluation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Evaluation::Bool(b) => write!(f, "{}", b),
            Evaluation::String(s) => write!(f, "{}", s.trim_matches('"')),
            Evaluation::Number(n) => write!(f, "{}", n),
            Evaluation::Nil => write!(f, "nil"),
        }
    }
}

impl Evaluator {
    pub fn new(ast: AstNode) -> Self {
        Self { ast }
    }

    pub fn evaluate(&self) -> Result<Evaluation, Error> {
        match &self.ast {
            AstNode::Literal(literal) => match &literal {
                LiteralValue::Bool(true) => Ok(Evaluation::Bool(true)),
                LiteralValue::Bool(false) => Ok(Evaluation::Bool(false)),
                LiteralValue::String(s) => Ok(Evaluation::String(s.clone())),
                LiteralValue::Number(n) => Ok(Evaluation::Number(*n)),
                LiteralValue::Nil => Ok(Evaluation::Nil),
            },
            AstNode::Grouping(_) => todo!(),
            AstNode::Unary(_, _) => todo!(),
            AstNode::Binary(_, _, _) => todo!(),
            AstNode::Eof => todo!(),
        }
    }
}
