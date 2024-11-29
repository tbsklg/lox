use core::fmt;

use anyhow::{anyhow, Error};

use crate::parse::{AstNode, LiteralValue, Operator};

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
            Evaluation::Number(n) => {
                if n.fract() == 0.0 {
                    write!(f, "{}", n.trunc())
                } else {
                    write!(f, "{:?}", n)
                }
            }
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
            AstNode::Grouping(g) => {
                Evaluator::new(*g.expression.clone()).evaluate()
            },
            AstNode::Unary(o, e) => {
               match o {
                    &Operator::Minus => {
                        match Evaluator::new(*e.clone()).evaluate()? {
                            Evaluation::Number(n) => Ok(Evaluation::Number(-n)),
                            _ => Err(anyhow!("Unary minus can only be applied to numbers")),
                        }
                    },
                    &Operator::Bang => {
                        match Evaluator::new(*e.clone()).evaluate()? {
                            Evaluation::Bool(true) => Ok(Evaluation::Bool(false)),
                            Evaluation::Bool(false) => Ok(Evaluation::Bool(true)),
                            Evaluation::Nil => Ok(Evaluation::Bool(true)),
                            _ => Ok(Evaluation::Bool(false)),
                        }
                    },
                    _ => Err(anyhow!("Unknown unary operator")),
                }
            },
            AstNode::Binary(_, _, _) => todo!(),
            AstNode::Eof => todo!(),
        }
    }
}
