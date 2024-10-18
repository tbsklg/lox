use core::fmt;
use std::{collections::HashMap, iter::Peekable};

use anyhow::Error;

#[derive(Debug, Clone)]
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
    NUMBER(String),
    IDENTIFIER(String),
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
            TokenType::NUMBER(s) => write!(f, "{} {} {:?}", "NUMBER", s, s.parse::<f64>().unwrap()),
            TokenType::IDENTIFIER(s) => write!(f, "{} {} null", "IDENTIFIER", s),
            TokenType::AND => write!(f, "{}  null", "AND"),
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

pub struct Lexer<I>
where
    I: Iterator,
{
    iterator: Peekable<I>,
    line: Line,
    reserved_words: HashMap<String, TokenType>,
}

impl<I> Lexer<I>
where
    I: Iterator<Item = char>,
{
    pub fn new(input: impl IntoIterator<Item = char, IntoIter = I>) -> Self {
        Self {
            iterator: input.into_iter().peekable(),
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
    Double(Token, Token),
    Error(String),
    Comment(Token),
    NewLine,
    String,
    Skip,
    Number,
    Identifier,
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
                        && (self.iterator.peek().unwrap().is_digit(10)
                            || self.iterator.peek() == Some(&'.'))
                    {
                        capture.push_str(&self.iterator.next()?.to_string());
                    }
                    return Some(Ok(Token::with_literal(TokenType::NUMBER(capture.clone()))));
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
                        .unwrap_or_else(|| TokenType::IDENTIFIER(capture.clone()));

                    return Some(Ok(Token::with_literal(token_type)));
                }
                TokenKind::Error(e) => return Some(Err(Error::msg(e))),
            }
        }
    }
}
