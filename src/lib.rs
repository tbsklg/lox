use core::fmt;
use std::iter::Peekable;

use anyhow::Error;

#[derive(Debug)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    STAR,
    DOT,
    COMMA,
    PLUS,
    MINUS,
    SEMICOLON,
    EOF,
    EQUAL,
    EQUALEQUAL,
    BANG,
    BANGEQUAL,
    LESSEQUAL,
    LESS,
    GREATEREQUAL,
    GREATER,
    STRING,
    SLASH,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::EOF => write!(f, "{}", "EOF"),
            TokenType::LeftParen => write!(f, "{}", "LEFT_PAREN"),
            TokenType::RightParen => write!(f, "{}", "RIGHT_PAREN"),
            TokenType::LeftBrace => write!(f, "{}", "LEFT_BRACE"),
            TokenType::RightBrace => write!(f, "{}", "RIGHT_BRACE"),
            TokenType::DOT => write!(f, "{}", "DOT"),
            TokenType::COMMA => write!(f, "{}", "COMMA"),
            TokenType::PLUS => write!(f, "{}", "PLUS"),
            TokenType::STAR => write!(f, "{}", "STAR"),
            TokenType::MINUS => write!(f, "{}", "MINUS"),
            TokenType::SEMICOLON => write!(f, "{}", "SEMICOLON"),
            TokenType::EQUAL => write!(f, "{}", "EQUAL"),
            TokenType::EQUALEQUAL => write!(f, "{}", "EQUAL_EQUAL"),
            TokenType::BANG => write!(f, "{}", "BANG"),
            TokenType::BANGEQUAL => write!(f, "{}", "BANG_EQUAL"),
            TokenType::LESS => write!(f, "{}", "LESS"),
            TokenType::LESSEQUAL => write!(f, "{}", "LESS_EQUAL"),
            TokenType::GREATER => write!(f, "{}", "GREATER"),
            TokenType::GREATEREQUAL => write!(f, "{}", "GREATER_EQUAL"),
            TokenType::SLASH => write!(f, "{}", "SLASH"),
            TokenType::STRING => write!(f, "{}", "STRING"),
        }
    }
}

pub struct Token {
    token_type: TokenType,
    lexeme: String,
    literal: String,
}

impl Token {
    pub fn from(token_type: TokenType, lexeme: &str) -> Self {
        Token {
            token_type,
            lexeme: lexeme.to_string(),
            literal: "null".to_string(),
        }
    }

    pub fn with_literal(token_type: TokenType, lexeme: &str, literal: &str) -> Self {
        Token {
            token_type,
            lexeme: lexeme.to_string(),
            literal: literal.to_string(),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.token_type, self.lexeme, self.literal)
    }
}

pub struct Lexer<I>
where
    I: Iterator,
{
    iterator: Peekable<I>,
    line: Line,
}

#[derive(Debug)]
struct Line {
    line: u32,
}

impl Line {
    pub fn start_from(line: u32) -> Self {
        Self { line }
    }

    pub fn increment(&mut self) {
        self.line += 1
    }
}

impl fmt::Display for Line {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}", self.line)
    }
}

impl<I> Lexer<I>
where
    I: Iterator<Item = char>,
{
    pub fn new(input: impl IntoIterator<Item = char, IntoIter = I>) -> Self {
        Self {
            iterator: input.into_iter().peekable(),
            line: Line::start_from(1),
        }
    }
}

enum TokenKind {
    Single(Token),
    Double(Token, Token),
    Error(String),
    Comment(Token),
    NewLine,
    String,
    Skip,
}

impl<I> Iterator for Lexer<I>
where
    I: Iterator<Item = char>,
{
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let c = self.iterator.next()?;
            let p = self.iterator.peek();

            let kind = match c {
                '(' => TokenKind::Single(Token::from(TokenType::LeftParen, "(")),
                ')' => TokenKind::Single(Token::from(TokenType::RightParen, ")")),
                '}' => TokenKind::Single(Token::from(TokenType::RightBrace, "}")),
                '{' => TokenKind::Single(Token::from(TokenType::LeftBrace, "{")),
                ',' => TokenKind::Single(Token::from(TokenType::COMMA, ",")),
                '.' => TokenKind::Single(Token::from(TokenType::DOT, ".")),
                '-' => TokenKind::Single(Token::from(TokenType::MINUS, "-")),
                '+' => TokenKind::Single(Token::from(TokenType::PLUS, "+")),
                ';' => TokenKind::Single(Token::from(TokenType::SEMICOLON, ";")),
                '*' => TokenKind::Single(Token::from(TokenType::STAR, "*")),
                '=' => TokenKind::Double(
                    Token::from(TokenType::EQUAL, "="),
                    Token::from(TokenType::EQUALEQUAL, "=="),
                ),
                '!' => TokenKind::Double(
                    Token::from(TokenType::BANG, "!"),
                    Token::from(TokenType::BANGEQUAL, "!="),
                ),
                '<' => TokenKind::Double(
                    Token::from(TokenType::LESS, "<"),
                    Token::from(TokenType::LESSEQUAL, "<="),
                ),
                '>' => TokenKind::Double(
                    Token::from(TokenType::GREATER, ">"),
                    Token::from(TokenType::GREATEREQUAL, ">="),
                ),
                '/' => TokenKind::Comment(Token::from(TokenType::SLASH, "/")),
                '\n' => TokenKind::NewLine,
                '"' => TokenKind::String,
                '\t' | ' ' => TokenKind::Skip,
                c => TokenKind::Error(format!(
                    "[{}] Error: Unexpected character: {}",
                    self.line, c
                )),
            };

            match kind {
                TokenKind::Single(token) => return Some(Ok(token)),
                TokenKind::Double(token1, token2) => {
                    if p == Some(&'=') {
                        self.iterator.next();
                        return Some(Ok(token2));
                    } else {
                        return Some(Ok(token1));
                    }
                }
                TokenKind::Comment(token) => {
                    if p == Some(&'/') {
                        while self.iterator.peek() != None && self.iterator.peek() != Some(&'\n') {
                            self.iterator.next();
                        }
                        continue;
                    }
                    return Some(Ok(token));
                }
                TokenKind::NewLine => {
                    self.line.increment();
                    continue;
                }
                TokenKind::String => {
                    let mut literal = "".to_string();
                    while self.iterator.peek() != Some(&'"') {
                        let s = self.iterator.next()?;
                        literal.push_str(&s.to_string());
                    }

                    return Some(Ok(Token::with_literal(
                        TokenType::STRING,
                        format!("\"{}\"", literal.as_str()).as_str(),
                        literal.as_str(),
                    )));
                }
                TokenKind::Skip => continue,
                TokenKind::Error(e) => return Some(Err(Error::msg(e))),
            }
        }
    }
}
