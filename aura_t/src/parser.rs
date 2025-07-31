use crate::lex::{Literal, Token, TokenType};
use anyhow::Result;
use std::{
    fmt::{Display, Formatter},
    iter::Peekable,
    slice::Iter,
};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Star,
    Slash,
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    And,
    Or,
}

impl From<TokenType> for BinaryOperator {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::Plus => BinaryOperator::Plus,
            TokenType::Minus => BinaryOperator::Minus,
            TokenType::Star => BinaryOperator::Star,
            TokenType::Slash => BinaryOperator::Slash,
            TokenType::BangEqual => BinaryOperator::BangEqual,
            TokenType::EqualEqual => BinaryOperator::EqualEqual,
            TokenType::Greater => BinaryOperator::Greater,
            TokenType::GreaterEqual => BinaryOperator::GreaterEqual,
            TokenType::Less => BinaryOperator::Less,
            TokenType::LessEqual => BinaryOperator::LessEqual,
            TokenType::And => BinaryOperator::And,
            TokenType::Or => BinaryOperator::Or,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Minus,
    Bang,
}

impl From<TokenType> for UnaryOperator {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::Minus => UnaryOperator::Minus,
            TokenType::Bang => UnaryOperator::Bang,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var {
        initializer: Option<Expr>,
        token: Token,
    },
    Block {
        statements: Vec<Stmt>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprType {
    Assign {
        name: Token,
        expression: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: BinaryOperator,
        right: Box<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Unary {
        operator: UnaryOperator,
        right: Box<Expr>,
    },
    Literal {
        value: Option<Value>,
    },
    Variable,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub expr_type: ExprType,
    pub token: Token,
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Syntax error at line {0}")]
    SyntaxError(u32),
}

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
}

impl<'a> Parser<'a> {
    pub fn parse(tokens: Peekable<Iter<'a, Token>>) -> Result<Vec<Stmt>, ParseError> {
        let mut parser = Parser { tokens };

        parser.program()
    }

    fn program(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::new();
        while let Some(token) = self.tokens.peek()
            && token.token_type != TokenType::Eof
        {
            statements.push(self.decleration()?);
        }

        Ok(statements)
    }

    fn decleration(&mut self) -> Result<Stmt, ParseError> {
        let token = self.tokens.peek().unwrap();
        let decl = match token.token_type {
            TokenType::Let => self.variable(),
            _ => self.stmt(),
        };

        if decl.is_err() {
            while let Some(token) = self.tokens.peek() {
                let token_type = token.token_type;
                let Some(next_token) = self.tokens.next() else {
                    break;
                };

                if token_type == TokenType::Semicolon {
                    break;
                }

                match next_token.token_type {
                    TokenType::Class
                    | TokenType::Fun
                    | TokenType::For
                    | TokenType::If
                    | TokenType::Print
                    | TokenType::Return
                    | TokenType::Let
                    | TokenType::While => {
                        break;
                    }
                    _ => {
                        continue;
                    }
                }
            }
        }

        decl
    }

    fn variable(&mut self) -> Result<Stmt, ParseError> {
        let var_token = self.tokens.next().unwrap();
        let Some(var_name_token) = self
            .tokens
            .next_if(|v| v.token_type == TokenType::Identifier)
        else {
            return Err(ParseError::SyntaxError(var_token.line));
        };
        let mut initializer = None;
        if self
            .tokens
            .next_if(|t| t.token_type == TokenType::Equal)
            .is_some()
        {
            initializer = Some(self.expr()?);
        }

        if self
            .tokens
            .next_if(|v| v.token_type == TokenType::Semicolon)
            .is_none()
        {
            return Err(ParseError::SyntaxError(var_token.line));
        }

        Ok(Stmt::Var {
            initializer,
            token: var_name_token.clone(),
        })
    }

    fn stmt(&mut self) -> Result<Stmt, ParseError> {
        let token = self.tokens.peek().unwrap();
        match token.token_type {
            TokenType::Print => self.print_stmt(),
            TokenType::LeftBrace => self.block_stmt(),
            _ => self.expression_stmt(),
        }
    }

    fn block_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.tokens.next().unwrap();
        let mut statements = Vec::new();
        while let Some(token) = self.tokens.peek()
            && token.token_type != TokenType::RightBrace
        {
            statements.push(self.decleration()?);
        }
        self.tokens.next();

        Ok(Stmt::Block { statements })
    }

    fn expression_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expr()?;
        let line = expr.token.line;
        let stmt = Stmt::Expression(expr);

        match self.tokens.next() {
            Some(t) if t.token_type == TokenType::Semicolon => Ok(stmt),
            _ => Err(ParseError::SyntaxError(line)),
        }
    }

    fn print_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.tokens.next();
        let expr = self.expr()?;
        let line = expr.token.line;
        let stmt = Stmt::Print(expr);

        match self.tokens.next() {
            Some(t) if t.token_type == TokenType::Semicolon => Ok(stmt),
            _ => Err(ParseError::SyntaxError(line)),
        }
    }

    fn expr(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let token = self.tokens.peek().map(|v| (*v).clone()).clone();
        let expr = self.equality()?;

        if self
            .tokens
            .next_if(|v| v.token_type == TokenType::Equal)
            .is_some()
        {
            let token = token.unwrap(); // next_if() retuned Some, peek() couldn't have been None
            let value = self.assignment()?;

            if expr.expr_type == ExprType::Variable {
                return Ok(Expr {
                    expr_type: ExprType::Assign {
                        name: expr.token.clone(),
                        expression: Box::new(value),
                    },
                    token,
                });
            } else {
                return Err(ParseError::SyntaxError(token.line));
            }
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;

        while let Some(token) = self.tokens.next_if(|t| {
            t.token_type != TokenType::Eof
                && matches!(t.token_type, TokenType::BangEqual | TokenType::EqualEqual)
        }) {
            let right = self.comparison()?;

            expr = Expr {
                token: token.clone(),
                expr_type: ExprType::Binary {
                    left: Box::new(expr),
                    operator: token.token_type.into(),
                    right: Box::new(right),
                },
            };
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.term()?;

        while let Some(token) = self.tokens.next_if(|t| {
            t.token_type != TokenType::Eof
                && matches!(
                    t.token_type,
                    TokenType::Greater
                        | TokenType::GreaterEqual
                        | TokenType::Less
                        | TokenType::LessEqual
                )
        }) {
            let right = self.term()?;

            expr = Expr {
                token: token.clone(),
                expr_type: ExprType::Binary {
                    left: Box::new(expr),
                    operator: token.token_type.into(),
                    right: Box::new(right),
                },
            };
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;

        while let Some(token) = self.tokens.next_if(|t| {
            t.token_type != TokenType::Eof
                && matches!(t.token_type, TokenType::Plus | TokenType::Minus)
        }) {
            let right = self.factor()?;

            expr = Expr {
                token: token.clone(),
                expr_type: ExprType::Binary {
                    left: Box::new(expr),
                    operator: token.token_type.into(),
                    right: Box::new(right),
                },
            };
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;

        while let Some(token) = self.tokens.next_if(|t| {
            t.token_type != TokenType::Eof
                && matches!(t.token_type, TokenType::Star | TokenType::Slash)
        }) {
            let right = self.unary()?;

            expr = Expr {
                token: token.clone(),
                expr_type: ExprType::Binary {
                    left: Box::new(expr),
                    operator: token.token_type.into(),
                    right: Box::new(right),
                },
            };
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(token) = self.tokens.next_if(|t| {
            t.token_type != TokenType::Eof
                && matches!(t.token_type, TokenType::Bang | TokenType::Minus)
        }) {
            let right = self.unary()?;

            return Ok(Expr {
                token: token.clone(),
                expr_type: ExprType::Unary {
                    operator: token.token_type.into(),
                    right: Box::new(right),
                },
            });
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        if let Some(&token) = self.tokens.peek()
            && token.token_type != TokenType::Eof
        {
            let token_type = token.token_type;
            if matches!(
                token_type,
                TokenType::False
                    | TokenType::True
                    | TokenType::Number
                    | TokenType::String
                    | TokenType::Nil
            ) {
                self.tokens.next();

                return Ok(Expr {
                    token: token.clone(),
                    expr_type: ExprType::Literal {
                        value: token.literal.as_ref().map(|literal| literal.into()),
                    },
                });
            }

            if token_type == TokenType::Identifier {
                self.tokens.next();
                return Ok(Expr {
                    token: token.clone(),
                    expr_type: ExprType::Variable,
                });
            }

            if token_type == TokenType::LeftParen {
                self.tokens.next();
                let expr = self.expr()?;

                if let Some(&token) = self.tokens.peek() {
                    if token.token_type == TokenType::RightParen {
                        self.tokens.next();
                        return Ok(Expr {
                            token: token.clone(),
                            expr_type: ExprType::Grouping {
                                expression: Box::new(expr),
                            },
                        });
                    }
                }
            }
        }

        Err(ParseError::SyntaxError(
            self.tokens.peek().map(|t| t.line).unwrap_or(0),
        ))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl From<Literal> for Value {
    fn from(literal: Literal) -> Self {
        match literal {
            Literal::NumberLiteral(v) => Value::Number(v),
            Literal::StringLiteral(v) => Value::String(v),
        }
    }
}

impl From<&Literal> for Value {
    fn from(literal: &Literal) -> Self {
        match literal {
            Literal::NumberLiteral(v) => Value::Number(*v),
            Literal::StringLiteral(v) => Value::String(v.clone()),
        }
    }
}
impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(v) => write!(f, "{v}"),
            Value::String(v) => write!(f, "{v}"),
            Value::Boolean(v) => write!(f, "{v}"),
            Value::Nil => write!(f, "nil"),
        }
    }
}
