use std::fmt;

use logos::Logos;

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexingError {
    InvalidChar(String),
    #[default]
    Other,
}

impl fmt::Display for LexingError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::InvalidChar(c) => write!(f, "Invalid character: {}", c),
            Self::Other => write!(f, "Unknown lexing error"),
        }
    }
}

impl LexingError {
    fn at(&self, span: core::ops::Range<usize>) -> LocatedLexingError {
        LocatedLexingError {
            error: self.clone(),
            span: span,
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct LocatedLexingError {
    pub error: LexingError,
    pub span: core::ops::Range<usize>,
}

fn from_lexing_error<'a>(lexer: &logos::Lexer<'a, Token<'a>>) -> LocatedLexingError {
    LocatedLexingError {
        error: LexingError::InvalidChar(lexer.slice().to_string()),
        span: lexer.span(),
    }
}

#[derive(Logos, Clone, PartialEq, Debug)]
#[logos(skip r"[ \t\r\n\f]+", skip r"//[^\n]*", skip r"/\*([^*]|\*[^/])*\*/", error(LocatedLexingError, from_lexing_error))]
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
    #[token("%")]
    Percent,

    #[token("~")]
    Tilde,

    #[token("+=")]
    PlusAssign,
    #[token("-=")]
    MinusAssign,
    #[token("*=")]
    StarAssign,
    #[token("/=")]
    SlashAssign,

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

    #[token("??")]
    NullCoalesce,

    #[token("=")]
    Assign,

    #[token("=>")]
    DoubleArrow,

    #[token("null")]
    Null,

    #[token("let")]
    Let,
    #[token("fn")]
    Fn,
    #[token("class")]
    Class,
    #[token("constructor")]
    Constructor,

    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("for")]
    For,
    #[token("while")]
    While,
    #[token("in")]
    In,
    #[token("return")]
    Return,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("raise")]
    Raise,
    #[token("static")]
    Static,

    #[token("new")]
    New,

    #[token("try")]
    Try,
    #[token("catch")]
    Catch,
    #[token("as")]
    As,

    #[token("||")]
    Or,
    #[token("&&")]
    And,
    #[token("!")]
    Not,

    #[token("|")]
    Pipe,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice())]
    Ident(&'a str),

    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", |lex| lex.slice().parse::<f64>().map_err(|_| LexingError::InvalidChar(lex.slice().to_string()).at(lex.span())), priority = 3)]
    Number(f64),

    #[regex(r"-?(?:0|[1-9]\d*)", |lex| lex.slice().parse::<i64>().map_err(|_| LexingError::InvalidChar(lex.slice().to_string()).at(lex.span())), priority = 4)]
    Integer(i64),

    #[regex(r"'([^'\\]|\\.)*'", |lex| {
        let slice = lex.slice();
        let unquoted = &slice[1..slice.len() - 1];
        match unquoted {
            r#"\'"# => Ok('\''),
            r#"\n"# => Ok('\n'),
            r#"\t"# => Ok('\t'),
            r#"\\"# => Ok('\\'),
            _ => {
                let chars: Vec<char> = unquoted.chars().collect();
                if chars.len() == 1 {
                    Ok(chars[0])
                } else {
                    Err(LexingError::InvalidChar(lex.slice().to_string()).at(lex.span()))
                }
            }
        }
    })]
    Char(char),

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
            Self::Percent => write!(f, "%"),
            Self::Tilde => write!(f, "~"),
            Self::PlusAssign => write!(f, "+="),
            Self::MinusAssign => write!(f, "-="),
            Self::StarAssign => write!(f, "*="),
            Self::SlashAssign => write!(f, "/="),
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
            Self::Less => write!(f, "<"),
            Self::LessEqual => write!(f, "<="),
            Self::Greater => write!(f, ">"),
            Self::GreaterEqual => write!(f, ">="),
            Self::NullCoalesce => write!(f, "??"),
            Self::Assign => write!(f, "="),
            Self::DoubleArrow => write!(f, "=>"),
            Self::Null => write!(f, "null"),
            Self::Let => write!(f, "let"),
            Self::Fn => write!(f, "fn"),
            Self::Class => write!(f, "class"),
            Self::Or => write!(f, "||"),
            Self::And => write!(f, "&&"),
            Self::Not => write!(f, "!"),
            Self::Pipe => write!(f, "|"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::For => write!(f, "for"),
            Self::While => write!(f, "while"),
            Self::Return => write!(f, "return"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),
            Self::Raise => write!(f, "raise"),
            Self::Static => write!(f, "static"),
            Self::New => write!(f, "new"),
            Self::Try => write!(f, "try"),
            Self::Catch => write!(f, "catch"),
            Self::As => write!(f, "as"),
            Self::In => write!(f, "in"),
            Self::Constructor => write!(f, "constructor"),
            Self::Ident(s) => write!(f, "Ident({})", s),
            Self::Number(n) => write!(f, "Number({})", n),
            Self::Integer(i) => write!(f, "Integer({})", i),
            Self::String(s) => write!(f, "String(\"{}\")", s),
            Self::Char(c) => write!(f, "Char('{}')", c),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LocatedToken<'a> {
    pub token: Token<'a>,
    pub span: core::ops::Range<usize>,
}

pub fn lex(input: &str) -> Result<Vec<LocatedToken>, LocatedLexingError> {
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
