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

impl<I> Iterator for Lexer<I>
where
    I: Iterator<Item = char>,
{
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {

        fn when_equal(c: &char) -> impl Fn(Token, Token) -> Token {
            match c {
                '=' => move |_, e| e,
                _ => move |e, _| e,
            }
        }

        loop {
            let c = self.iterator.next()?;
            let p = self.iterator.peek();
            let single_or = when_equal(p?);

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
                '=' => {
                    return Some(Ok(single_or(
                        Token::from(TokenType::EQUAL, "="),
                        Token::from(TokenType::EQUALEQUAL, "=="),
                    )))
                }
                '!' => {
                    return Some(Ok(single_or(
                        Token::from(TokenType::BANG, "!"),
                        Token::from(TokenType::BANGEQUAL, "!="),
                    )))
                }
                '<' => {
                    return Some(Ok(single_or(
                        Token::from(TokenType::LESS, "<"),
                        Token::from(TokenType::LESSEQUAL, "<="),
                    )))
                }
                '>' => {
                    return Some(Ok(single_or(
                        Token::from(TokenType::GREATER, ">"),
                        Token::from(TokenType::GREATEREQUAL, ">="),
                    )))
                }
                '/' => {
                    if p == Some(&'/') {
                        while self.iterator.peek() != Some(&'\n') {
                            self.iterator.next();
                        }
                        continue;
                    }

                    return Some(Ok(Token::from(TokenType::SLASH, "/")));
                }
                '\n' => {
                    self.iterator.next();
                    continue;
                }
                '\t' => {
                    self.iterator.next();
                    continue;
                }
                ' ' => {
                    self.iterator.next();
                    continue;
                }
                c => return Some(Err(anyhow::anyhow!("Unexpected character: {}", c))),
            };
        }
    }
}
