use anyhow::Error;

use crate::parse::{AstNode, LiteralValue, Parser};

pub struct Evaluator {
    ast: AstNode,
}

impl Evaluator {
    pub fn new(ast: AstNode) -> Self {
        Self { ast }
    }

    pub fn evaluate(&self) -> Result<bool, Error> {
        match &self.ast {
            AstNode::Literal(literal) => match &literal {
                LiteralValue::Bool(b) => Ok(*b),
                _ => todo!(),
            },
            AstNode::Grouping(_) => todo!(),
            AstNode::Unary(_, _) => todo!(),
            AstNode::Binary(_, _, _) => todo!(),
            AstNode::Eof => todo!(),
        }
    }
}
