use anyhow::{Context, Result};
use std::{
    collections::HashMap,
    fmt::Display,
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
}

impl Interpreter {
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
        println!("{ast:#?}");
    }

    fn report_error(&self, line: u32, location: &str, message: &str) {
        println!("[{location} line {line}] Error: {message}");
    }
}

// Parser
#[derive(Debug, Clone)]
enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Literal {
        value: Token,
    },
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
    fn parse(tokens: Peekable<Iter<'a, Token>>) -> Result<Expr, ParseError> {
        let mut parser = Parser { tokens };

        parser.expr()
    }

    fn expr(&mut self) -> Result<Expr, ParseError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;

        while let Some(&token) = self.tokens.peek()
            && token.token_type != TokenType::Eof
            && (token.token_type == TokenType::BangEqual
                || token.token_type == TokenType::EqualEqual)
        {
            let operator = token.clone();
            self.tokens.next();
            let right = self.comparison()?;

            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
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
            let operator = token.clone();
            self.tokens.next();
            let right = self.term()?;

            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
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
            let operator = token.clone();
            self.tokens.next();
            let right = self.factor()?;

            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
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
            let operator = token.clone();
            self.tokens.next();
            let right = self.unary()?;

            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        while let Some(&token) = self.tokens.peek()
            && token.token_type != TokenType::Eof
            && (token.token_type == TokenType::Bang || token.token_type == TokenType::Minus)
        {
            let operator = token.clone();
            self.tokens.next();
            let right = self.primary()?;

            return Ok(Expr::Unary {
                operator,
                right: Box::new(right),
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

                return Ok(Expr::Literal {
                    value: token.clone(),
                });
            }

            if token_type == TokenType::LeftParen {
                self.tokens.next();
                let expr = self.expr()?;

                if let Some(&token) = self.tokens.peek() {
                    if token.token_type == TokenType::RightParen {
                        self.tokens.next();
                        return Ok(Expr::Grouping {
                            expression: Box::new(expr),
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

#[derive(Debug, Clone)]
enum Literal {
    StringLiteral(String),
    NumberLiteral(f64),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::NumberLiteral(n) => {
                write!(f, "{}", n)
            }
            Literal::StringLiteral(s) => {
                write!(f, "\"{}\"", s)
            }
        }
    }
}

#[derive(Debug, Clone)]
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
        line: line,
    });

    Ok(tokens)
}
