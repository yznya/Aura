use anyhow::{Context, Result};
use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    fs,
    io::{self},
    iter::Peekable,
    path::Path,
    slice::Iter,
    sync::LazyLock,
};
use thiserror::Error;

static KEYWORDS: LazyLock<HashMap<String, TokenType>> = LazyLock::new(|| {
    let mut keywords = HashMap::new();
    keywords.insert("and".to_owned(), TokenType::And);
    keywords.insert("class".to_owned(), TokenType::Class);
    keywords.insert("else".to_owned(), TokenType::Else);
    keywords.insert("false".to_owned(), TokenType::False);
    keywords.insert("for".to_owned(), TokenType::For);
    keywords.insert("fun".to_owned(), TokenType::Fun);
    keywords.insert("if".to_owned(), TokenType::If);
    keywords.insert("nil".to_owned(), TokenType::Nil);
    keywords.insert("or".to_owned(), TokenType::Or);
    keywords.insert("print".to_owned(), TokenType::Print);
    keywords.insert("return".to_owned(), TokenType::Return);
    keywords.insert("super".to_owned(), TokenType::Super);
    keywords.insert("this".to_owned(), TokenType::This);
    keywords.insert("true".to_owned(), TokenType::True);
    keywords.insert("let".to_owned(), TokenType::Let);
    keywords.insert("while".to_owned(), TokenType::While);
    keywords
});
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

#[derive(Debug, Clone, PartialEq)]
enum Value {
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

#[derive(Debug, Clone, PartialEq)]
enum BinaryOperator {
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
enum UnaryOperator {
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

// Parser
#[derive(Debug, Clone)]
enum Stmt {
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
enum ExprType {
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
struct Expr {
    expr_type: ExprType,
    token: Token,
}

#[derive(Error, Debug)]
enum ParseError {
    #[error("Syntax error at line {0}")]
    SyntaxError(u32),
}

struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
}

impl<'a> Parser<'a> {
    fn parse(tokens: Peekable<Iter<'a, Token>>) -> Result<Vec<Stmt>, ParseError> {
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
        if let Some(token) = self.tokens.peek()
            && token.token_type == TokenType::Equal
        {
            self.tokens.next();
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

        while let Some(&token) = self.tokens.peek()
            && token.token_type != TokenType::Eof
            && (token.token_type == TokenType::BangEqual
                || token.token_type == TokenType::EqualEqual)
        {
            self.tokens.next();
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

        while let Some(&token) = self.tokens.peek()
            && token.token_type != TokenType::Eof
            && (token.token_type == TokenType::Greater
                || token.token_type == TokenType::GreaterEqual
                || token.token_type == TokenType::Less
                || token.token_type == TokenType::LessEqual)
        {
            self.tokens.next();
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

        while let Some(&token) = self.tokens.peek()
            && token.token_type != TokenType::Eof
            && (token.token_type == TokenType::Plus || token.token_type == TokenType::Minus)
        {
            self.tokens.next();
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

        while let Some(&token) = self.tokens.peek()
            && token.token_type != TokenType::Eof
            && (token.token_type == TokenType::Star || token.token_type == TokenType::Slash)
        {
            self.tokens.next();
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
        if let Some(&token) = self.tokens.peek()
            && token.token_type != TokenType::Eof
            && (token.token_type == TokenType::Bang || token.token_type == TokenType::Minus)
        {
            self.tokens.next();
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
            if token_type == TokenType::False
                || token_type == TokenType::True
                || token_type == TokenType::Number
                || token_type == TokenType::String
                || token_type == TokenType::Nil
            {
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

// Scanner
#[derive(Debug, PartialEq, Clone, Copy)]
enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier,
    String,
    Number,
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Let,
    While,
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
enum Literal {
    StringLiteral(String),
    NumberLiteral(f64),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::NumberLiteral(n) => {
                write!(f, "{n}")
            }
            Literal::StringLiteral(s) => {
                write!(f, "\"{s}\"")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Token {
    token_type: TokenType,
    lexeme: String,
    literal: Option<Literal>,
    line: u32,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}, {:?}], ({:?}, {})",
            self.line, self.literal, self.token_type, self.lexeme
        )
    }
}

#[derive(Error, Debug)]
enum ScanError {
    #[error("Unexpected character found at line {0}")]
    UnexpectedChar(u32),
    #[error("Unterminated string found at line {0}")]
    UnterminatedString(u32),
}

fn scan_tokens(script: &str) -> Result<Vec<Token>, ScanError> {
    let mut tokens = Vec::new();
    let mut line = 0;

    let mut chars = script.chars().peekable();
    while let Some(c) = chars.next() {
        let mut lexeme = String::new();
        let mut literal = String::new();
        lexeme.push(c);

        let token_type = match c {
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            '{' => TokenType::LeftBrace,
            '}' => TokenType::RightBrace,
            ',' => TokenType::Comma,
            '.' => TokenType::Dot,
            '-' => TokenType::Minus,
            '+' => TokenType::Plus,
            ';' => TokenType::Semicolon,
            '*' => TokenType::Star,
            '!' => {
                if let Some(next_char) = chars.peek()
                    && *next_char == '='
                {
                    lexeme.push(chars.next().unwrap());
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                }
            }
            '=' => {
                if let Some(next_char) = chars.peek()
                    && *next_char == '='
                {
                    lexeme.push(chars.next().unwrap());
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                }
            }
            '<' => {
                if let Some(next_char) = chars.peek()
                    && *next_char == '='
                {
                    lexeme.push(chars.next().unwrap());
                    TokenType::Less
                } else {
                    TokenType::LessEqual
                }
            }
            '>' => {
                if let Some(next_char) = chars.peek()
                    && *next_char == '='
                {
                    lexeme.push(chars.next().unwrap());
                    TokenType::Greater
                } else {
                    TokenType::GreaterEqual
                }
            }
            '/' => {
                if let Some(next_char) = chars.peek()
                    && *next_char != '/'
                {
                    while let Some(next_char) = chars.peek()
                        && *next_char != '\n'
                    {
                        chars.next();
                    }
                    continue;
                } else {
                    TokenType::Slash
                }
            }
            ' ' | '\t' | '\r' => {
                continue;
            }
            '\n' => {
                line += 1;
                continue;
            }
            '"' => loop {
                let Some(&next_char) = chars.peek() else {
                    return Err(ScanError::UnterminatedString(line));
                };

                chars.next();

                if next_char == '"' {
                    lexeme.push(next_char);
                    break TokenType::String;
                } else if next_char == '\n' {
                    line += 1;
                } else {
                    lexeme.push(next_char);
                    literal.push(next_char);
                }
            },

            _ => {
                if c.is_numeric() {
                    literal.push(c);
                    while let Some(&next_char) = chars.peek()
                        && next_char.is_numeric()
                    {
                        chars.next();
                        lexeme.push(next_char);
                        literal.push(next_char);
                    }

                    // TODO: clean up
                    let mut chars_iter_clone = chars.clone();
                    chars_iter_clone.next();

                    if let Some(&next_char) = chars.peek()
                        && next_char == '.'
                        && let Some(&next_next_char) = chars_iter_clone.peek()
                        && next_next_char.is_numeric()
                    {
                        chars.next();
                        lexeme.push(next_char);
                        literal.push(next_char);

                        while let Some(&next_char) = chars.peek()
                            && next_char.is_numeric()
                        {
                            chars.next();
                            lexeme.push(next_char);
                            literal.push(next_char);
                        }
                    }

                    TokenType::Number
                } else if c.is_alphabetic() {
                    while let Some(&next_char) = chars.peek()
                        && next_char.is_alphanumeric()
                    {
                        chars.next();
                        lexeme.push(next_char);
                    }
                    if let Some(&token_type) = KEYWORDS.get(&lexeme) {
                        token_type
                    } else {
                        TokenType::Identifier
                    }
                } else {
                    return Err(ScanError::UnexpectedChar(line));
                }
            }
        };

        tokens.push(Token {
            token_type,
            lexeme,
            literal: if token_type == TokenType::String {
                Some(Literal::StringLiteral(literal.clone()))
            } else if token_type == TokenType::Number {
                Some(Literal::NumberLiteral(literal.parse().unwrap()))
            } else {
                None
            },
            line,
        })
    }

    tokens.push(Token {
        token_type: TokenType::Eof,
        lexeme: String::new(),
        literal: None,
        line,
    });

    Ok(tokens)
}
