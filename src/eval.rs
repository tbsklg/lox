use anyhow::Error;

use crate::parse::{AstNode, LiteralValue};

pub struct Evaluator {
    ast: AstNode,
}

impl Evaluator {
    pub fn new(ast: AstNode) -> Self {
        Self { ast }
    }

    pub fn evaluate(&self) -> Result<String, Error> {
        match &self.ast {
            AstNode::Literal(literal) => match &literal {
                LiteralValue::Bool(true) => Ok("true".to_string()),
                LiteralValue::Bool(false) => Ok("false".to_string()),
                LiteralValue::String(s) => Ok(s.clone()),
                LiteralValue::Nil => Ok("nil".to_string()),
                _ => todo!(),
            },
            AstNode::Grouping(_) => todo!(),
            AstNode::Unary(_, _) => todo!(),
            AstNode::Binary(_, _, _) => todo!(),
            AstNode::Eof => todo!(),
        }
    }
}
