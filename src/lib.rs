use core::fmt;

use anyhow::Error;

#[derive(Debug)]
pub enum TokenType {
    LeftParen,
    RightParen,
    EOF,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::LeftParen => write!(f, "{}", "Left_Paren"),
            TokenType::RightParen => write!(f, "{}", "Right_Paren"),
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
        while let Some(c) = self.iterator.next() {
            let token = match c {
                '(' => Token::from(TokenType::LeftParen, "("),
                ')' => Token::from(TokenType::RightParen, ")"),
                _ => Token::from(TokenType::EOF, ""),
            };

            return Some(Ok(token));
        }

        None
    }
}
