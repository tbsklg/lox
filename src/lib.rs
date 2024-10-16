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
    STRING(String),
    SLASH,
    NUMBER(f64),
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::EOF => write!(f, "{}", "EOF null"),
            TokenType::LeftParen => write!(f, "{}", "LEFT_PAREN ( null"),
            TokenType::RightParen => write!(f, "{}", "RIGHT_PAREN ) null"),
            TokenType::LeftBrace => write!(f, "{}", "LEFT_BRACE { null"),
            TokenType::RightBrace => write!(f, "{}", "RIGHT_BRACE } null"),
            TokenType::DOT => write!(f, "{}", "DOT . null"),
            TokenType::COMMA => write!(f, "{}", "COMMA , null"),
            TokenType::PLUS => write!(f, "{}", "PLUS + null"),
            TokenType::STAR => write!(f, "{}", "STAR * null"),
            TokenType::MINUS => write!(f, "{}", "MINUS - null"),
            TokenType::SEMICOLON => write!(f, "{}", "SEMICOLON ; null"),
            TokenType::EQUAL => write!(f, "{}", "EQUAL = null"),
            TokenType::EQUALEQUAL => write!(f, "{}", "EQUAL_EQUAL == null"),
            TokenType::BANG => write!(f, "{}", "BANG ! null"),
            TokenType::BANGEQUAL => write!(f, "{}", "BANG_EQUAL != null"),
            TokenType::LESS => write!(f, "{}", "LESS < null"),
            TokenType::LESSEQUAL => write!(f, "{}", "LESS_EQUAL <= null"),
            TokenType::GREATER => write!(f, "{}", "GREATER > null"),
            TokenType::GREATEREQUAL => write!(f, "{}", "GREATER_EQUAL >= null"),
            TokenType::SLASH => write!(f, "{}", "SLASH / null"),
            TokenType::STRING(s) => write!(f, "{} \"{}\" {}", "STRING", s, s),
            TokenType::NUMBER(n) => write!(f, "{} {} {:?}", "NUMBER", n, n),
        }
    }
}

pub struct Token {
    token_type: TokenType,
}

impl Token {
    pub fn from(token_type: TokenType) -> Self {
        Token { token_type }
    }

    pub fn with_literal(token_type: TokenType) -> Self {
        Token { token_type }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_type)
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
    Number,
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
                '(' => TokenKind::Single(Token::from(TokenType::LeftParen)),
                ')' => TokenKind::Single(Token::from(TokenType::RightParen)),
                '}' => TokenKind::Single(Token::from(TokenType::RightBrace)),
                '{' => TokenKind::Single(Token::from(TokenType::LeftBrace)),
                ',' => TokenKind::Single(Token::from(TokenType::COMMA)),
                '.' => TokenKind::Single(Token::from(TokenType::DOT)),
                '-' => TokenKind::Single(Token::from(TokenType::MINUS)),
                '+' => TokenKind::Single(Token::from(TokenType::PLUS)),
                ';' => TokenKind::Single(Token::from(TokenType::SEMICOLON)),
                '*' => TokenKind::Single(Token::from(TokenType::STAR)),
                '=' => TokenKind::Double(
                    Token::from(TokenType::EQUAL),
                    Token::from(TokenType::EQUALEQUAL),
                ),
                '!' => TokenKind::Double(
                    Token::from(TokenType::BANG),
                    Token::from(TokenType::BANGEQUAL),
                ),
                '<' => TokenKind::Double(
                    Token::from(TokenType::LESS),
                    Token::from(TokenType::LESSEQUAL),
                ),
                '>' => TokenKind::Double(
                    Token::from(TokenType::GREATER),
                    Token::from(TokenType::GREATEREQUAL),
                ),
                '/' => TokenKind::Comment(Token::from(TokenType::SLASH)),
                '\n' => TokenKind::NewLine,
                '"' => TokenKind::String,
                '\t' | ' ' => TokenKind::Skip,
                c => {
                    if c.is_digit(10) {
                        TokenKind::Number
                    } else {
                        TokenKind::Error(format!(
                            "[{}] Error: Unexpected character: {}",
                            self.line, c
                        ))
                    }
                }
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
                    let mut capture = "".to_string();
                    while self.iterator.peek() != Some(&'"') {
                        if self.iterator.peek() == None {
                            return Some(Err(Error::msg(format!(
                                "[{}] Error: Unterminated string.",
                                self.line
                            ))));
                        }

                        capture.push_str(&self.iterator.next()?.to_string());
                    }
                    self.iterator.next();

                    return Some(Ok(Token::with_literal(TokenType::STRING(capture.clone()))));
                }
                TokenKind::Skip => continue,
                TokenKind::Number => {
                    let mut capture = c.to_string();
                    while self.iterator.peek() != None
                        && self.iterator.peek() != Some(&'\n')
                        && self.iterator.peek() != Some(&' ')
                    {
                        capture.push_str(&self.iterator.next()?.to_string());
                    }
                    self.iterator.next();
                    let f = capture.parse::<f64>().unwrap();
                    return Some(Ok(Token::with_literal(TokenType::NUMBER(f))));
                }
                TokenKind::Error(e) => return Some(Err(Error::msg(e))),
            }
        }
    }
}
