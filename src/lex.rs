use core::fmt;
use std::{collections::HashMap, iter::Peekable, str::Chars};

use anyhow::Error;

#[derive(Debug, Clone, PartialEq)]
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
    NUMBER(f64),
    IDENTIFIER,
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
}

pub struct Token {
    pub origin: String,
    pub kind: TokenType,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let origin = self.origin.clone();
        match self.kind {
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
            TokenType::STRING => write!(f, "STRING {origin} {}", origin.trim_matches('"')),
            TokenType::NUMBER(n) => {
                if n == n.trunc() {
                    // tests require that integers are printed as N.0
                    write!(f, "NUMBER {origin} {n}.0")
                } else {
                    write!(f, "NUMBER {n} {n}")
                }
            }
            TokenType::IDENTIFIER => write!(f, "{} {} null", "IDENTIFIER", origin),
            TokenType::AND => write!(f, "{}", "AND and null"),
            TokenType::CLASS => write!(f, "{}", "CLASS class null"),
            TokenType::ELSE => write!(f, "{}", "ELSE else null"),
            TokenType::FALSE => write!(f, "{}", "FALSE false null"),
            TokenType::FOR => write!(f, "{}", "FOR for null"),
            TokenType::FUN => write!(f, "{}", "FUN fun null"),
            TokenType::IF => write!(f, "{}", "IF if null"),
            TokenType::NIL => write!(f, "{}", "NIL nil null"),
            TokenType::OR => write!(f, "{}", "OR or null"),
            TokenType::PRINT => write!(f, "{}", "PRINT print null"),
            TokenType::RETURN => write!(f, "{}", "RETURN return null"),
            TokenType::SUPER => write!(f, "{}", "SUPER super null"),
            TokenType::THIS => write!(f, "{}", "THIS this null"),
            TokenType::TRUE => write!(f, "{}", "TRUE true null"),
            TokenType::VAR => write!(f, "{}", "VAR var null"),
            TokenType::WHILE => write!(f, "{}", "WHILE while null"),
        }
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Lexer<'de> {
    iterator: Peekable<Chars<'de>>,
    line: Line,
    reserved_words: HashMap<String, TokenType>,
}

impl<'de> Lexer<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            iterator: input.chars().into_iter().peekable(),
            line: Line::start_from(1),
            reserved_words: HashMap::from([
                (String::from("and"), TokenType::AND),
                (String::from("class"), TokenType::CLASS),
                (String::from("else"), TokenType::ELSE),
                (String::from("false"), TokenType::FALSE),
                (String::from("for"), TokenType::FOR),
                (String::from("fun"), TokenType::FUN),
                (String::from("if"), TokenType::IF),
                (String::from("nil"), TokenType::NIL),
                (String::from("or"), TokenType::OR),
                (String::from("print"), TokenType::PRINT),
                (String::from("return"), TokenType::RETURN),
                (String::from("super"), TokenType::SUPER),
                (String::from("this"), TokenType::THIS),
                (String::from("true"), TokenType::TRUE),
                (String::from("var"), TokenType::VAR),
                (String::from("while"), TokenType::WHILE),
            ]),
        }
    }
}

enum TokenKind {
    Single(Token),
    Double(TokenType, TokenType),
    Error(String),
    Comment(TokenType),
    NewLine,
    String,
    Skip,
    Number,
    Identifier,
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let c = self.iterator.next()?;

            let kind = match c {
                '(' => TokenKind::Single(Token {origin: c.to_string(), kind: TokenType::LeftParen}),
                ')' => TokenKind::Single(Token {origin: c.to_string(), kind: TokenType::RightParen}),
                '}' => TokenKind::Single(Token {origin: c.to_string(), kind: TokenType::RightBrace}),
                '{' => TokenKind::Single(Token {origin: c.to_string(), kind: TokenType::LeftBrace}),
                ',' => TokenKind::Single(Token {origin: c.to_string(), kind: TokenType::COMMA}),
                '.' => TokenKind::Single(Token {origin: c.to_string(), kind: TokenType::DOT}),
                '-' => TokenKind::Single(Token {origin: c.to_string(), kind: TokenType::MINUS}),
                '+' => TokenKind::Single(Token {origin: c.to_string(), kind: TokenType::PLUS}),
                ';' => TokenKind::Single(Token {origin: c.to_string(), kind: TokenType::SEMICOLON}),
                '*' => TokenKind::Single(Token {origin: c.to_string(), kind: TokenType::STAR}),
                '=' => TokenKind::Double(TokenType::EQUAL, TokenType::EQUALEQUAL),
                '!' => TokenKind::Double(TokenType::BANG, TokenType::BANGEQUAL),
                '<' => TokenKind::Double(TokenType::LESS, TokenType::LESSEQUAL),
                '>' => TokenKind::Double(TokenType::GREATER, TokenType::GREATEREQUAL),
                '/' => TokenKind::Comment(TokenType::SLASH),
                '\n' => TokenKind::NewLine,
                '"' => TokenKind::String,
                '\t' | ' ' => TokenKind::Skip,
                c => {
                    if c.is_digit(10) {
                        TokenKind::Number
                    } else if c.is_alphabetic() || c == '_' {
                        TokenKind::Identifier
                    } else {
                        TokenKind::Error(format!(
                            "[{}] Error: Unexpected character: {}",
                            self.line, c
                        ))
                    }
                }
            };

            let p = self.iterator.peek().cloned();
            match kind {
                TokenKind::Single(token) => return Some(Ok(token)),
                TokenKind::Double(token1, token2) => {
                    if p == Some('=') {
                        self.iterator.next();
                        return Some(Ok(Token{origin: format!("{}{}", c.to_string(), p.unwrap()), kind: token2}));
                    } else {
                        return Some(Ok(Token{origin: c.to_string(), kind: token1}));
                    }
                }
                TokenKind::Comment(token) => {
                    if p == Some('/') {
                        while self.iterator.peek() != None && self.iterator.peek() != Some(&'\n') {
                            self.iterator.next();
                        }
                        continue;
                    }
                    return Some(Ok(Token{origin: c.to_string(), kind: token}));
                }
                TokenKind::NewLine => {
                    self.line.increment();
                    continue;
                }
                TokenKind::String => {
                    let mut capture = c.to_string();
                    while self.iterator.peek() != Some(&'"') {
                        if self.iterator.peek() == None {
                            return Some(Err(Error::msg(format!(
                                "[{}] Error: Unterminated string.",
                                self.line
                            ))));
                        }

                        capture.push_str(&self.iterator.next()?.to_string());
                    }
                    capture.push_str(&self.iterator.next()?.to_string());

                    return Some(Ok(Token{origin: capture.clone(), kind: TokenType::STRING}));
                }
                TokenKind::Skip => continue,
                TokenKind::Number => {
                    let mut capture = c.to_string();
                    while self.iterator.peek() != None
                        && (self.iterator.peek().unwrap().is_digit(10)
                            || self.iterator.peek() == Some(&'.'))
                    {
                        capture.push_str(&self.iterator.next()?.to_string());
                    }
                    let n = capture.parse::<f64>().unwrap();
                    return Some(Ok(Token{origin: capture, kind: TokenType::NUMBER(n)}));
                }
                TokenKind::Identifier => {
                    let mut capture = c.to_string();
                    while self.iterator.peek() != None
                        && (self.iterator.peek().unwrap().is_alphanumeric()
                            || self.iterator.peek() == Some(&'_'))
                    {
                        capture.push_str(&self.iterator.next()?.to_string());
                    }

                    let token_type = self
                        .reserved_words
                        .get(&capture)
                        .cloned()
                        .unwrap_or_else(|| TokenType::IDENTIFIER);

                    return Some(Ok(Token{origin: capture, kind: token_type}));
                }
                TokenKind::Error(e) => return Some(Err(Error::msg(e))),
            }
        }
    }
}
