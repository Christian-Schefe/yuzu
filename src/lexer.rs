use std::fmt;

use logos::Logos;

#[derive(Logos, Clone, PartialEq, Debug)]
#[logos(skip r"[ \t\r\n\f]+", skip r"//[^\n]*", skip r"/\*([^*]|\*[^/])*\*/")]
pub enum Token<'a> {
    #[token("false", |_| false)]
    #[token("true", |_| true)]
    Bool(bool),

    #[token(":")]
    Colon,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,

    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEqual,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEqual,

    #[token("=")]
    Assign,

    #[token("null")]
    Null,

    #[token("let")]
    Let,
    #[token("if")]
    If,
    #[token("else")]
    Else,

    #[token("or")]
    Or,
    #[token("and")]
    And,
    #[token("not")]
    Not,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
    Ident(&'a str),

    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", |lex| lex.slice().parse::<f64>().unwrap())]
    Number(f64),

    #[regex(r"-?(?:0|[1-9]\d*)", |lex| lex.slice().parse::<i64>().unwrap(), priority = 4)]
    Integer(i64),

    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let slice = lex.slice();
        let unquoted = &slice[1..slice.len() - 1];
        let unescaped = unquoted.replace(r#"\""#, r#"""#).replace(r#"\n"#, "\n").replace(r#"\t"#, "\t");
        unescaped
    })]
    String(String),
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "Bool({})", b),
            Self::Colon => write!(f, ":"),
            Self::Dot => write!(f, "."),
            Self::Comma => write!(f, ","),
            Self::Semicolon => write!(f, ";"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LBracket => write!(f, "["),
            Self::RBracket => write!(f, "]"),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
            Self::Less => write!(f, "<"),
            Self::LessEqual => write!(f, "<="),
            Self::Greater => write!(f, ">"),
            Self::GreaterEqual => write!(f, ">="),
            Self::Assign => write!(f, "="),
            Self::Null => write!(f, "null"),
            Self::Let => write!(f, "let"),
            Self::Or => write!(f, "or"),
            Self::And => write!(f, "and"),
            Self::Not => write!(f, "not"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::Ident(s) => write!(f, "Ident({})", s),
            Self::Number(n) => write!(f, "Number({})", n),
            Self::Integer(i) => write!(f, "Integer({})", i),
            Self::String(s) => write!(f, "String(\"{}\")", s),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LocatedToken<'a> {
    pub token: Token<'a>,
    pub span: core::ops::Range<usize>,
}

pub fn lex(input: &str) -> Result<Vec<LocatedToken>, ()> {
    let mut lexer = Token::lexer(input);
    let mut tokens = Vec::new();
    while let Some(token) = lexer.next() {
        let span = lexer.span();
        let token = LocatedToken {
            token: token?,
            span,
        };
        tokens.push(token);
    }
    Ok(tokens)
}
