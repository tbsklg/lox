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
}

impl<I> Lexer<I>
where
    I: Iterator<Item = char>,
{
    pub fn new(input: impl IntoIterator<Item = char, IntoIter = I>) -> Self {
        Self {
            iterator: input.into_iter().peekable(),
        }
    }
}

enum TokenKind {
    Single(Token),
    Double(Token, Token),
    Error(String),
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
                '/' => {
                    if p == Some(&'/') {
                        while self.iterator.peek() != Some(&'\n') {
                            self.iterator.next();
                        }
                        continue;
                    }

                    TokenKind::Single(Token::from(TokenType::SLASH, "/"))
                }
                '\n' | '\t' | ' ' => TokenKind::Skip,
                c => TokenKind::Error(format!("Unexpected character: {}", c)),
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
                TokenKind::Skip => continue,
                TokenKind::Error(e) => return Some(Err(Error::msg(e))),
            }
        }
    }
}
