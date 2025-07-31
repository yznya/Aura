use crate::lex::{ScanError, Token, TokenType, scan_tokens};
use crate::parser::{
    BinaryOperator, Expr, ExprType, ParseError, Parser, Stmt, UnaryOperator, Value,
};
use anyhow::{Context, Result};
use std::{
    fmt::{Display, Formatter},
    fs,
    io::{self},
    path::Path,
    slice::Iter,
};
use thiserror::Error;

mod lex;
mod parser;

pub struct Interpreter {
    pub had_error: bool,
    environment: Environments,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            had_error: false,
            environment: Environments::new(),
        }
    }

    pub fn run_file(&mut self, path: impl AsRef<Path>) {
        let content = fs::read_to_string(path)
            .context("Failed to read file")
            .unwrap();

        self.run(&content);
    }

    pub fn prompt(&mut self) {
        let mut buf = String::new();

        while io::stdin().read_line(&mut buf).unwrap() > 0 {
            self.run(&buf);
            buf.clear();
        }
    }

    fn run(&mut self, script: &str) {
        let tokens = match scan_tokens(script) {
            Ok(t) => t,
            Err(e @ ScanError::UnexpectedChar(line)) => {
                self.report_error(line, "", &e.to_string());
                return;
            }
            Err(e @ ScanError::UnterminatedString(line)) => {
                self.report_error(line, "", &e.to_string());
                return;
            }
        };

        let ast = match Parser::parse(tokens.iter().peekable()) {
            Ok(t) => t,
            Err(e @ ParseError::SyntaxError(line)) => {
                self.report_error(line, "", &e.to_string());
                return;
            }
        };

        for statement in &ast {
            match self.evaluate(statement) {
                Ok(_) => {}
                Err(e @ RuntimeError::TypeMismatch(line)) => {
                    self.report_error(line, "", &e.to_string());
                    return;
                }
                Err(e @ RuntimeError::UndefinedVariable(line)) => {
                    self.report_error(line, "", &e.to_string());
                    return;
                }
            };
        }
    }

    fn evaluate(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Expression(expr) => {
                let _ = self.evaluate_expr(expr);
            }
            Stmt::Print(expr) => {
                let value = self.evaluate_expr(expr)?;
                println!("{value}");
            }
            Stmt::Var { initializer, token } => {
                let value = if let Some(init_expr) = initializer {
                    self.evaluate_expr(init_expr)?
                } else {
                    Value::Nil
                };

                self.environment.define(token.lexeme.clone(), value);
            }
            Stmt::Block { statements } => {
                self.environment.new_scope();
                for statement in statements {
                    self.evaluate(statement)?;
                }
                self.environment.end_scope();
            }
        };

        Ok(())
    }

    fn evaluate_expr(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match &expr.expr_type {
            ExprType::Binary {
                left,
                operator,
                right,
            } => {
                let left = self.evaluate_expr(left.as_ref())?;
                let right = self.evaluate_expr(right.as_ref())?;

                match (left, right) {
                    (Value::Number(l), Value::Number(r)) => match operator {
                        BinaryOperator::Plus => Ok(Value::Number(l + r)),
                        BinaryOperator::Minus => Ok(Value::Number(l - r)),
                        BinaryOperator::Star => Ok(Value::Number(l * r)),
                        BinaryOperator::Slash => Ok(Value::Number(l / r)),
                        BinaryOperator::Greater => Ok(Value::Boolean(l > r)),
                        BinaryOperator::GreaterEqual => Ok(Value::Boolean(l >= r)),
                        BinaryOperator::Less => Ok(Value::Boolean(l < r)),
                        BinaryOperator::LessEqual => Ok(Value::Boolean(l <= r)),
                        _ => unreachable!(),
                    },
                    _ => Err(RuntimeError::TypeMismatch(expr.token.line)),
                }
            }
            ExprType::Grouping { expression } => self.evaluate_expr(expression.as_ref()),
            ExprType::Unary { operator, right } => {
                let right = self.evaluate_expr(right.as_ref())?;

                match operator {
                    UnaryOperator::Bang => match right {
                        Value::Boolean(b) => Ok(Value::Boolean(!b)),
                        _ => Err(RuntimeError::TypeMismatch(expr.token.line)),
                    },
                    UnaryOperator::Minus => match right {
                        Value::Number(v) => Ok(Value::Number(-v)),
                        _ => Err(RuntimeError::TypeMismatch(expr.token.line)),
                    },
                }
            }
            ExprType::Literal { value } => match expr.token.token_type {
                TokenType::String => Ok(value.clone().unwrap()),
                TokenType::Number => Ok(value.clone().unwrap()),
                TokenType::True => Ok(Value::Boolean(true)),
                TokenType::False => Ok(Value::Boolean(false)),
                _ => panic!("Invalid state"),
            },
            ExprType::Variable => {
                if expr.token.token_type == TokenType::Identifier {
                    Ok(self.environment.get(&expr.token)?.clone())
                } else {
                    panic!("Varible ExprType without an Identifier token")
                }
            }
            ExprType::Assign { name, expression } => {
                let value = self.evaluate_expr(expression.as_ref())?;
                self.environment.assign(name, value.clone())?;
                Ok(value)
            }
        }
    }

    fn report_error(&mut self, line: u32, location: &str, message: &str) {
        self.had_error = true;
        println!("[{location} line {line}] Error: {message}");
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Error, Debug)]
enum RuntimeError {
    #[error("Undefined Variable at line {0}")]
    UndefinedVariable(u32),

    #[error("Type mismatch at line {0}")]
    TypeMismatch(u32),
}

struct Environments {
    scopes: Vec<Environment>,
}

impl Environments {
    fn new() -> Self {
        Self {
            scopes: vec![Environment {
                values: HashMap::new(),
                enclosing_env: -1,
            }],
        }
    }

    fn new_scope(&mut self) {
        self.scopes.push(Environment {
            values: HashMap::new(),
            enclosing_env: (self.scopes.len() - 1) as i32,
        })
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn define(&mut self, key: String, value: Value) {
        let index = self.scopes.len() - 1;
        self.scopes[index].values.insert(key, value);
    }

    fn assign(&mut self, token: &Token, value: Value) -> Result<(), RuntimeError> {
        let mut index = (self.scopes.len() - 1) as i32;

        while index != -1 {
            match self.scopes[index as usize].values.get_mut(&token.lexeme) {
                Some(var) => {
                    *var = value;
                    return Ok(());
                }
                None => {
                    index = self.scopes[index as usize].enclosing_env;
                }
            }
        }
        Err(RuntimeError::UndefinedVariable(token.line))
    }

    fn get(&self, token: &Token) -> Result<&Value, RuntimeError> {
        let mut index = (self.scopes.len() - 1) as i32;
        while index != -1 {
            if let Some(v) = self.scopes[index as usize].values.get(&token.lexeme) {
                return Ok(v);
            } else {
                index = self.scopes[index as usize].enclosing_env;
            }
        }

        Err(RuntimeError::UndefinedVariable(token.line))
    }
}

struct Environment {
    values: HashMap<String, Value>,
    enclosing_env: i32,
}
