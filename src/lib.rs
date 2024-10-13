use core::fmt;

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
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
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
            TokenType::EOF => write!(f, "{}", "EOF"),
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

pub struct Lexer<I> {
    iterator: I,
}

impl<I> Lexer<I> {
    pub fn new(input: impl IntoIterator<Item = char, IntoIter = I>) -> Self {
        Self {
            iterator: input.into_iter(),
        }
    }
}

impl<I> Iterator for Lexer<I>
where
    I: Iterator<Item = char>,
{
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.iterator.next()?;
        match c {
            '(' => return Some(Ok(Token::from(TokenType::LeftParen, "("))),
            ')' => return Some(Ok(Token::from(TokenType::RightParen, ")"))),
            '}' => return Some(Ok(Token::from(TokenType::RightBrace, "}"))),
            '{' => return Some(Ok(Token::from(TokenType::LeftBrace, "{"))),
            ',' => return Some(Ok(Token::from(TokenType::COMMA, ","))),
            '.' => return Some(Ok(Token::from(TokenType::DOT, "."))),
            '-' => return Some(Ok(Token::from(TokenType::MINUS, "-"))),
            '+' => return Some(Ok(Token::from(TokenType::PLUS, "+"))),
            ';' => return Some(Ok(Token::from(TokenType::SEMICOLON, ";"))),
            '*' => return Some(Ok(Token::from(TokenType::STAR, "*"))),
            c => return Some(Err(anyhow::anyhow!("[line 1] Error: Unexpected character: {}", c))),
        };
    }
}
