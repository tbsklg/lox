use core::fmt;
use std::{
    cell::RefCell,
    collections::HashMap,
    mem,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{
    lex::Token,
    parse::{Expr, LiteralValue, Operator, Stmt},
};
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
            Evaluation::Function(c) => write!(f, "<fn {}>", c.name),
        }
    }
}

pub struct Callable {
    name: String,
    arity: usize,
    params: Vec<Token>,
    body: Vec<Stmt>,
    closure: Rc<RefCell<Environment>>,
}

impl Callable {
    fn call(&mut self, evaluator: &mut Evaluator, arguments: &Vec<Evaluation>) -> Evaluation {
        if self.name == "clock" {
            return Evaluation::Number(
                SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap()
                    .as_secs() as f64,
            );
        }

        // Create a new environment that inherits from the function's closure
        let new_env = Environment::new_scope(self.closure.clone());

        // Define function parameters in the new environment
        for (param, arg) in self.params.iter().zip(arguments) {
            new_env.borrow_mut().define(&param.origin, arg.clone());
        }

        // Swap the evaluator's environment and run the function body
        let prev_env = mem::replace(&mut evaluator.env, new_env);

        let result = (|| {
            for stmt in &self.body {
                match evaluator.evaluate_stmt(stmt) {
                    Ok(_) => continue,
                    Err(RuntimeError::Return(value)) => return Ok(value),
                    Err(e) => return Err(e),
                }
            }
            Ok(Evaluation::Nil)
        })();

        // Restore the previous environment
        evaluator.env = prev_env;

        result.unwrap_or_else(|err| match err {
            RuntimeError::Return(value) => value,
            RuntimeError::Error(e) => {
                panic!("Unexpected error: {:?}", e);
            }
        })
    }
}

impl Clone for Callable {
    fn clone(&self) -> Self {
        Self {
            name: self.name.clone(),
            arity: self.arity,
            params: self.params.clone(),
            body: self.body.clone(),
            closure: self.closure.clone(),
        }
    }
}

impl std::fmt::Debug for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Callable(arity={})", self.arity)
    }
}

pub enum RuntimeError {
    Error(anyhow::Error),
    Return(Evaluation),
}

#[derive(Debug, Clone)]
struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Evaluation>,
}

impl Environment {
    fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Environment {
            enclosing: None,
            values: HashMap::new(),
        }))
    }

    fn new_scope(enclosing: Rc<RefCell<Environment>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Environment {
            enclosing: Some(enclosing),
            values: HashMap::new(),
        }))
    }

    fn define(&mut self, key: &str, eval: Evaluation) {
        self.values.insert(key.to_string(), eval);
    }

    fn get(&self, key: &str) -> Result<Evaluation, Error> {
        if let Some(value) = self.values.get(key) {
            return Ok(value.clone());
        }

        if let Some(ref enclosing) = self.enclosing {
            return enclosing.borrow().get(key);
        }

        Err(anyhow!("Undefined variable '{key}'"))
    }

    fn assign(&mut self, key: &str, eval: Evaluation) -> Result<(), Error> {
        if self.values.contains_key(key) {
            self.values.insert(key.to_string(), eval);
            return Ok(());
        }

        if let Some(ref enclosing) = self.enclosing {
            return enclosing.borrow_mut().assign(key, eval);
        }

        Err(anyhow!("Undefined variable '{key}'"))
    }
}

pub struct Evaluator {
    ast: Vec<Stmt>,
    env: Rc<RefCell<Environment>>,
}

impl Evaluator {
    pub fn new(ast: Vec<Stmt>) -> Self {
        let env = Environment::new();

        let clock_fn = Callable {
            name: "clock".to_string(),
            arity: 0,
            params: vec![],
            body: vec![],
            closure: Rc::clone(&env),
        };

        env.borrow_mut()
            .define("clock", Evaluation::Function(clock_fn));

        Self { ast, env }
    }

    pub fn evaluate(&mut self) -> Result<(), Error> {
        for statement in self.ast.clone() {
            match self.evaluate_stmt(&statement) {
                Ok(_) => continue,
                Err(RuntimeError::Error(e)) => return Err(e),
                Err(RuntimeError::Return(_)) => {
                    return Err(anyhow!("Unexpected return outside of a function."));
                }
            }
        }

        Ok(())
    }

    fn evaluate_stmt(&mut self, stmt: &Stmt) -> Result<Evaluation, RuntimeError> {
        match stmt {
            Stmt::Expression(expr) => self.evaluate_expr(expr),
            Stmt::Print(expr) => {
                let result = self.evaluate_expr(expr)?;
                self.print_evaluation(&result);
                Ok(Evaluation::Nil)
            }
            Stmt::Var(name, initializer) => {
                let value = match initializer {
                    Some(expr) => self.evaluate_expr(expr)?,
                    None => Evaluation::Nil,
                };
                self.env.borrow_mut().define(name, value);
                Ok(Evaluation::Nil)
            }
            Stmt::Block(stmts) => {
                self.evaluate_block(stmts)?;
                Ok(Evaluation::Nil)
            }
            Stmt::If(condition, then_stmt, else_stmt) => {
                if self.is_truthy(condition) {
                    self.evaluate_stmt(then_stmt)
                } else {
                    match else_stmt {
                        Some(stmt) => self.evaluate_stmt(stmt),
                        None => Ok(Evaluation::Nil),
                    }
                }
            }
            Stmt::While(condition, then_stmt) => {
                while self.is_truthy(condition) {
                    self.evaluate_stmt(then_stmt)?;
                }
                Ok(Evaluation::Nil)
            }
            Stmt::Function(name, params, body) => {
                self.evaluate_function(&name.origin, params, body.as_ref())?;
                Ok(Evaluation::Nil)
            }
            Stmt::Return(_, value) => match value {
                Some(expr) => {
                    let result = self.evaluate_expr(expr)?;
                    Err(RuntimeError::Return(result))
                }
                None => Err(RuntimeError::Return(Evaluation::Nil)),
            },
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

    fn evaluate_function(
        &mut self,
        name: &str,
        params: &[Token],
        body: &Stmt,
    ) -> Result<(), RuntimeError> {
        let body = match body {
            Stmt::Block(stmts) => stmts.to_vec(),
            _ => vec![],
        };

        self.env.borrow_mut().define(
            name,
            Evaluation::Function(Callable {
                name: name.to_string(),
                arity: params.len(),
                params: params.to_vec(),
                body,
                closure: Rc::clone(&self.env), // Capture the current environment
            }),
        );

        Ok(())
    }

    fn evaluate_block(&mut self, stmts: &Vec<Stmt>) -> Result<(), RuntimeError> {
        let new_env = Environment::new_scope(Rc::clone(&self.env));
        let prev_env = mem::replace(&mut self.env, new_env);

        for stmt in stmts {
            match self.evaluate_stmt(stmt) {
                Ok(_) => continue,
                Err(RuntimeError::Return(value)) => {
                    self.env = prev_env;
                    return Err(RuntimeError::Return(value));
                }
                Err(RuntimeError::Error(e)) => {
                    self.env = prev_env;
                    return Err(RuntimeError::Error(e));
                }
            }
        }

        self.env = prev_env;
        Ok(())
    }

    fn print_evaluation(&self, evaluation: &Evaluation) {
        println!("{}", evaluation);
    }

    fn evaluate_expr(&mut self, expr: &Expr) -> Result<Evaluation, RuntimeError> {
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
                        _ => Err(RuntimeError::Error(anyhow!("Operand must be a number."))),
                    },
                    Operator::Bang => match right {
                        Evaluation::Bool(b) => Ok(Evaluation::Bool(!b)),
                        Evaluation::Nil => Ok(Evaluation::Bool(true)),
                        _ => Ok(Evaluation::Bool(false)),
                    },
                    _ => Err(RuntimeError::Error(anyhow!("Unknown unary operator"))),
                }
            }
            Expr::Binary(left, op, right) => self.evaluate_binary(left, op, right),
            Expr::Identifier(name) => match self.env.borrow_mut().get(&name.to_string()) {
                Ok(v) => Ok(v),
                Err(_) => Err(RuntimeError::Error(anyhow!("Unknown identifier {name}."))),
            },
            Expr::Assign(name, value_expr) => {
                let value = self.evaluate_expr(value_expr)?;
                let _ = self.env.borrow_mut().assign(name, value.clone());
                Ok(value)
            }
            Expr::Logical(left, op, right) => self.evaluate_logical(left, op, right),
            Expr::Call(callee, _, arguments) => self.evaluate_call(callee.as_ref(), arguments),
        }
    }

    fn evaluate_call(
        &mut self,
        expr: &Expr,
        arguments: &[Expr],
    ) -> Result<Evaluation, RuntimeError> {
        let callee = self.evaluate_expr(expr)?;

        let evaluations: Vec<Evaluation> = arguments
            .iter()
            .map(|arg| self.evaluate_expr(arg))
            .collect::<Result<_, _>>()?;

        match callee {
            Evaluation::Function(mut callable) => {
                if arguments.len() != callable.arity {
                    return Err(RuntimeError::Error(anyhow!(
                        "Expected {} arguments but got {}",
                        callable.arity,
                        arguments.len()
                    )));
                }
                Ok(callable.call(self, &evaluations))
            }
            _ => Err(RuntimeError::Error(anyhow!("Can only call functions"))),
        }
    }

    fn evaluate_logical(
        &mut self,
        left: &Expr,
        op: &Operator,
        right: &Expr,
    ) -> Result<Evaluation, RuntimeError> {
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
            _ => Err(RuntimeError::Error(anyhow!(
                "Operator {} is not allowed for logical evaluation",
                op
            ))),
        }
    }

    fn evaluate_binary(
        &mut self,
        left: &Expr,
        op: &Operator,
        right: &Expr,
    ) -> Result<Evaluation, RuntimeError> {
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
                _ => Err(RuntimeError::Error(anyhow!(
                    "Invalid binary operator for numbers"
                ))),
            },
            (Evaluation::String(l), Evaluation::String(r)) => match op {
                Operator::Plus => Ok(Evaluation::String(format!("{}{}", l, r))),
                Operator::EqualEqual => Ok(Evaluation::Bool(l == r)),
                Operator::BangEqual => Ok(Evaluation::Bool(l != r)),
                _ => Err(RuntimeError::Error(anyhow!(
                    "Only `+`, `==`, and `!=` are valid for strings"
                ))),
            },
            (Evaluation::Number(_), Evaluation::String(_)) => match op {
                Operator::EqualEqual => Ok(Evaluation::Bool(false)),
                _ => Err(RuntimeError::Error(anyhow!(
                    "Only `+`, `==`, and `!=` are valid for strings"
                ))),
            },
            (Evaluation::Bool(l), Evaluation::Bool(r)) => match op {
                Operator::EqualEqual => Ok(Evaluation::Bool(l == r)),
                Operator::BangEqual => Ok(Evaluation::Bool(l != r)),
                _ => Err(RuntimeError::Error(anyhow!(
                    "Unsupported operation for binary bools"
                ))),
            },
            _ => Err(RuntimeError::Error(anyhow!(
                "Operands must be of the same type for binary operations"
            ))),
        }
    }
}
