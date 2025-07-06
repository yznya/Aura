use anyhow::{Context, Result};
use std::{
    collections::HashMap,
    fmt::Display,
    fs,
    io::{self},
    path::Path,
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

        for token in tokens {
            println!("{}", token);
        }
    }

    fn report_error(&self, line: u32, location: &str, message: &str) {
        println!("[{location} line {line}] Error: {message}");
    }
}

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

#[derive(Debug)]
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

#[derive(Debug)]
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
