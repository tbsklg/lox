use core::fmt;
use std::collections::HashMap;

use crate::parse::{Expr, LiteralValue, Operator, Stmt};
use anyhow::{anyhow, Error};

#[derive(Clone)]
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

pub struct RuntimeError {
    pub error: Error,
}

type Environment = HashMap<String, Evaluation>;

pub struct Evaluator {
    ast: Vec<Stmt>,
}

impl Evaluator {
    pub fn new(ast: Vec<Stmt>) -> Self {
        Self {
            ast,
        }
    }

    pub fn evaluate(&mut self) -> Result<(), RuntimeError> {
        let mut env = HashMap::new();
        for statement in self.ast.clone() {
            match self.evaluate_stmt(&statement, &mut env) {
                Ok(_) => continue,
                Err(e) => return Err(RuntimeError { error: e }),
            }
        }

        Ok(())
    }

    fn evaluate_stmt(&mut self, stmt: &Stmt, env: &mut Environment) -> Result<(), Error> {
        match stmt {
            Stmt::Expression(expr) => {
                self.evaluate_expr(expr, env)?;
                Ok(())
            }
            Stmt::Print(expr) => match self.evaluate_expr(expr, env) {
                Ok(result) => {
                    self.print_evaluation(&result);
                    Ok(())
                }
                Err(e) => Err(e),
            },
            Stmt::Var(name, initializer) => match initializer {
                Some(expr) => {
                    let eval = self.evaluate_expr(expr, env)?;
                    env.insert(name.to_string(), eval);
                    Ok(())
                }
                None => {
                    env.insert(name.to_string(), Evaluation::Nil);
                    Ok(())
                }
            },
            Stmt::Block(stmts) => {
                self.evaluate_block(stmts, env)?;
                Ok(())
            }
        }
    }

    fn evaluate_block(&mut self, stmts: &Vec<Stmt>, env: &mut Environment) -> Result<(), Error> {
        for stmt in stmts {
            let _ = self.evaluate_stmt(stmt, env)?;
        }

        Ok(())
    }

    fn print_evaluation(&self, evaluation: &Evaluation) {
        println!("{}", evaluation);
    }

    fn evaluate_expr(&mut self, expr: &Expr, env: &mut Environment) -> Result<Evaluation, Error> {
        match expr {
            Expr::Literal(literal) => match literal {
                LiteralValue::Bool(b) => Ok(Evaluation::Bool(*b)),
                LiteralValue::String(s) => Ok(Evaluation::String(s.clone())),
                LiteralValue::Number(n) => Ok(Evaluation::Number(*n)),
                LiteralValue::Nil => Ok(Evaluation::Nil),
            },
            Expr::Grouping(expr) => self.evaluate_expr(expr, env),
            Expr::Unary(op, expr) => {
                let right = self.evaluate_expr(expr, env)?;
                match op {
                    Operator::Minus => match right {
                        Evaluation::Number(n) => Ok(Evaluation::Number(-n)),
                        _ => Err(anyhow!("Operand must be a number.")),
                    },
                    Operator::Bang => match right {
                        Evaluation::Bool(b) => Ok(Evaluation::Bool(!b)),
                        Evaluation::Nil => Ok(Evaluation::Bool(true)),
                        _ => Ok(Evaluation::Bool(false)),
                    },
                    _ => Err(anyhow!("Unknown unary operator")),
                }
            }
            Expr::Binary(left, op, right) => self.evaluate_binary(left, op, right, env),
            Expr::Identifier(name) => match env.get(name) {
                Some(value) => Ok(value.clone()),
                None => Err(anyhow!("Undefined variable '{}'.", name)),
            },
            Expr::Assign(name, value_expr) => {
                let value = self.evaluate_expr(value_expr, env)?;
                env.insert(name.clone(), value.clone());
                Ok(value)
            }
        }
    }

    fn evaluate_binary(
        &mut self,
        left: &Expr,
        op: &Operator,
        right: &Expr,
        env: &mut Environment,
    ) -> Result<Evaluation, Error> {
        let left_val = self.evaluate_expr(left, env)?;
        let right_val = self.evaluate_expr(right, env)?;

        match (left_val, right_val) {
            (Evaluation::Number(l), Evaluation::Number(r)) => match op {
                Operator::Minus => Ok(Evaluation::Number(l - r)),
                Operator::Plus => Ok(Evaluation::Number(l + r)),
                Operator::Multi => Ok(Evaluation::Number(l * r)),
                Operator::Div => Ok(Evaluation::Number(l / r)),
                Operator::Greater => Ok(Evaluation::Bool(l > r)),
                Operator::GreaterEqual => Ok(Evaluation::Bool(l >= r)),
                Operator::Less => Ok(Evaluation::Bool(l < r)),
                Operator::LessEqual => Ok(Evaluation::Bool(l <= r)),
                Operator::EqualEqual => Ok(Evaluation::Bool(l == r)),
                Operator::BangEqual => Ok(Evaluation::Bool(l != r)),
                _ => Err(anyhow!("Invalid binary operator for numbers")),
            },
            (Evaluation::String(l), Evaluation::String(r)) => match op {
                Operator::Plus => Ok(Evaluation::String(format!("{}{}", l, r))),
                Operator::EqualEqual => Ok(Evaluation::Bool(l == r)),
                Operator::BangEqual => Ok(Evaluation::Bool(l != r)),
                _ => Err(anyhow!("Only `+`, `==`, and `!=` are valid for strings")),
            },
            (Evaluation::Number(_), Evaluation::String(_)) => match op {
                Operator::EqualEqual => Ok(Evaluation::Bool(false)),
                _ => Err(anyhow!("Only `+`, `==`, and `!=` are valid for strings")),
            },
            (Evaluation::Bool(l), Evaluation::Bool(r)) => match op {
                Operator::EqualEqual => Ok(Evaluation::Bool(l == r)),
                Operator::BangEqual => Ok(Evaluation::Bool(l != r)),
                _ => Err(anyhow!("Unsupported operation for binary bools")),
            },
            _ => Err(anyhow!(
                "Operands must be of the same type for binary operations"
            )),
        }
    }
}
