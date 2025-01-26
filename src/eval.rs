use core::fmt;
use std::{
    collections::HashMap,
    mem,
    sync::Arc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::parse::{Expr, LiteralValue, Operator, Stmt};
use anyhow::{anyhow, Error};

#[derive(Clone, Debug)]
pub enum Evaluation {
    Bool(bool),
    String(String),
    Number(f64),
    Nil,
    Function(Callable),
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
            Evaluation::Function(_) => todo!(),
        }
    }
}

pub struct Callable {
    arity: usize,
    function: Arc<dyn Fn(&Vec<Evaluation>) -> Evaluation + Send + Sync>,
}

impl Callable {
    fn call(&self, arguments: &Vec<Evaluation>) -> Evaluation {
        (self.function)(arguments)
    }
}

impl Clone for Callable {
    fn clone(&self) -> Self {
        Self {
            arity: self.arity,
            function: Arc::clone(&self.function),
        }
    }
}

impl std::fmt::Debug for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Callable(arity={})", self.arity)
    }
}

pub struct RuntimeError {
    pub error: Error,
}

#[derive(Default, Debug, Clone)]
struct Environment {
    enclosing: Option<Box<Environment>>,
    values: HashMap<String, Evaluation>,
}

impl Environment {
    fn new_scope(&mut self) {
        let mut new = Environment {
            enclosing: Some(mem::take(self).into()),
            values: HashMap::new(),
        };

        mem::swap(self, &mut new);
    }

    fn prev_scope(&mut self) {
        let mut old = mem::take(self.enclosing.as_mut().unwrap());
        mem::swap(self, &mut old);
    }

    fn define(&mut self, key: &str, eval: Evaluation) {
        self.values.insert(key.to_string(), eval);
    }

    fn get(&self, key: &str) -> Result<Evaluation, Error> {
        if let Some(value) = self.values.get(key) {
            return Ok(value.clone());
        }

        match &self.enclosing {
            Some(enclosing) => enclosing.get(key),
            None => Err(anyhow!("Undefined variable '{key}'")),
        }
    }

    fn assign(&mut self, key: &str, eval: Evaluation) -> Result<(), Error> {
        if self.values.contains_key(key) {
            self.values.insert(key.to_string(), eval);
            return Ok(());
        }

        if let Some(ref mut enclosing) = &mut self.enclosing {
            return enclosing.assign(key, eval);
        }

        Err(anyhow!("Undefined variable '{key}'"))
    }
}

pub struct Evaluator {
    ast: Vec<Stmt>,
    env: Environment,
}

impl Evaluator {
    pub fn new(ast: Vec<Stmt>) -> Self {
        let mut env: Environment = Default::default();

        env.define(
            "clock",
            Evaluation::Function(Callable {
                arity: 0,
                function: Arc::new(|_args| {
                    Evaluation::Number(
                        SystemTime::now()
                            .duration_since(UNIX_EPOCH)
                            .unwrap()
                            .as_secs_f64(),
                    )
                }),
            }),
        );

        Self { ast, env }
    }

    pub fn evaluate(&mut self) -> Result<(), RuntimeError> {
        for statement in self.ast.clone() {
            match self.evaluate_stmt(&statement) {
                Ok(_) => continue,
                Err(e) => return Err(RuntimeError { error: e }),
            }
        }

        Ok(())
    }

    fn evaluate_stmt(&mut self, stmt: &Stmt) -> Result<(), Error> {
        match stmt {
            Stmt::Expression(expr) => {
                self.evaluate_expr(expr)?;
                Ok(())
            }
            Stmt::Print(expr) => match self.evaluate_expr(expr) {
                Ok(result) => {
                    self.print_evaluation(&result);
                    Ok(())
                }
                Err(e) => Err(e),
            },
            Stmt::Var(name, initializer) => match initializer {
                Some(expr) => {
                    let eval = self.evaluate_expr(expr)?;
                    self.env.define(name, eval);
                    Ok(())
                }
                None => {
                    self.env.define(name, Evaluation::Nil);
                    Ok(())
                }
            },
            Stmt::Block(stmts) => {
                self.evaluate_block(stmts)?;
                Ok(())
            }
            Stmt::If(condition, then_stmt, else_stmt) => {
                if self.is_truthy(condition) {
                    self.evaluate_stmt(then_stmt)
                } else {
                    else_stmt
                        .as_ref()
                        .map_or(Ok(()), |stmt| self.evaluate_stmt(stmt))
                }
            }
            Stmt::While(condition, then_stmt) => {
                while self.is_truthy(condition) {
                    let _ = self.evaluate_stmt(then_stmt);
                }

                Ok(())
            }
        }
    }

    fn is_truthy(&mut self, expr: &Expr) -> bool {
        self.evaluate_expr(expr)
            .map(|eval| {
                matches!(
                    eval,
                    Evaluation::Bool(true) | Evaluation::String(_) | Evaluation::Number(_)
                )
            })
            .unwrap_or(false)
    }

    fn evaluate_block(&mut self, stmts: &Vec<Stmt>) -> Result<(), Error> {
        self.env.new_scope();

        for stmt in stmts {
            self.evaluate_stmt(stmt)?;
        }

        self.env.prev_scope();

        Ok(())
    }

    fn print_evaluation(&self, evaluation: &Evaluation) {
        println!("{}", evaluation);
    }

    fn evaluate_expr(&mut self, expr: &Expr) -> Result<Evaluation, Error> {
        match expr {
            Expr::Literal(literal) => match literal {
                LiteralValue::Bool(b) => Ok(Evaluation::Bool(*b)),
                LiteralValue::String(s) => Ok(Evaluation::String(s.clone())),
                LiteralValue::Number(n) => Ok(Evaluation::Number(*n)),
                LiteralValue::Nil => Ok(Evaluation::Nil),
            },
            Expr::Grouping(expr) => self.evaluate_expr(expr),
            Expr::Unary(op, expr) => {
                let right = self.evaluate_expr(expr)?;
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
            Expr::Binary(left, op, right) => self.evaluate_binary(left, op, right),
            Expr::Identifier(name) => self.env.get(&name.to_string()),
            Expr::Assign(name, value_expr) => {
                let value = self.evaluate_expr(value_expr)?;
                let _ = self.env.assign(name, value.clone());
                Ok(value)
            }
            Expr::Logical(left, op, right) => self.evaluate_logical(left, op, right),
            Expr::Call(callee, _, arguments) => self.evaluate_call(callee.as_ref(), arguments),
        }
    }

    fn evaluate_call(&mut self, expr: &Expr, arguments: &[Expr]) -> Result<Evaluation, Error> {
        let callee = self.evaluate_expr(expr)?;

        let evaluations: Vec<Evaluation> = arguments
            .iter()
            .map(|arg| self.evaluate_expr(arg))
            .collect::<Result<_, _>>()?;

        match callee {
            Evaluation::Function(callable) => {
                if arguments.len() != callable.arity {
                    return Err(anyhow!(
                        "Expected {} arguments but got {}",
                        callable.arity,
                        arguments.len()
                    ));
                }
                Ok(callable.call(&evaluations))
            }
            _ => Err(anyhow!("Can only call functions")),
        }
    }

    fn evaluate_logical(
        &mut self,
        left: &Expr,
        op: &Operator,
        right: &Expr,
    ) -> Result<Evaluation, Error> {
        match op {
            Operator::Or => {
                if self.is_truthy(left) {
                    let left_val = self.evaluate_expr(left)?;
                    Ok(left_val)
                } else {
                    self.evaluate_expr(right)
                }
            }
            Operator::And => {
                if !self.is_truthy(left) {
                    let left_val = self.evaluate_expr(left)?;
                    Ok(left_val)
                } else {
                    self.evaluate_expr(right)
                }
            }
            _ => Err(anyhow!(
                "Operator {} is not allowed for logical evaluation",
                op
            )),
        }
    }

    fn evaluate_binary(
        &mut self,
        left: &Expr,
        op: &Operator,
        right: &Expr,
    ) -> Result<Evaluation, Error> {
        let left_val = self.evaluate_expr(left)?;
        let right_val = self.evaluate_expr(right)?;

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
