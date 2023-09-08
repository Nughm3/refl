#![allow(clippy::all)]
use std::fmt;
use logos::Logos;
use cstree::Syntax;
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Logos, Syntax)]
pub enum SyntaxKind {
    #[token("struct")]
    Struct,
    #[regex("[a-zA-Z_][a-zA-Z_0-9]*")]
    Ident,
    #[token("{")]
    LeftBrace,
    #[token(",")]
    Comma,
    #[token("}")]
    RightBrace,
    #[token("enum")]
    Enum,
    #[token("fn")]
    Fn,
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("->")]
    Arrow,
    #[token(":")]
    Colon,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
    #[token("&")]
    Ampersand,
    #[token(";")]
    Semicolon,
    #[token("let")]
    Let,
    #[token("=")]
    Equals,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("while")]
    While,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("return")]
    Return,
    #[regex("\\d+")]
    Int,
    #[regex("\\d+\\.\\d+")]
    Float,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[regex("b?'([^']|\\\\[\\\\nrt0'])'")]
    Char,
    #[regex("r?b?\"([^\"]|\\\\[\\\\nrt0\"])*\"")]
    String,
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
    #[token("==")]
    Eq,
    #[token("!=")]
    Neq,
    #[token("<")]
    Lt,
    #[token("<=")]
    Le,
    #[token(">")]
    Gt,
    #[token(">=")]
    Ge,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("+=")]
    PlusEquals,
    #[token("-=")]
    MinusEquals,
    #[token("*=")]
    StarEquals,
    #[token("/=")]
    SlashEquals,
    #[token("%=")]
    PercentEquals,
    #[token("!")]
    Not,
    Program,
    StructDef,
    EnumDef,
    FunctionDef,
    Record,
    Param,
    Block,
    FunctionType,
    ArrayType,
    PtrType,
    CustomType,
    ExprStmt,
    LetStmt,
    IfStmt,
    ForStmt,
    WhileStmt,
    BreakStmt,
    ContinueStmt,
    ReturnStmt,
    InfixExpr,
    PrefixExpr,
    ParenExpr,
    CallExpr,
    ArrayExpr,
    IndexExpr,
    StructExpr,
    #[regex(r"[ \n\r\t]+")]
    Whitespace,
    #[regex("//.*")]
    Comment,
    Error,
    #[doc(hidden)]
    Eof,
}
impl fmt::Display for SyntaxKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SyntaxKind::Struct => write!(f, "struct"),
            SyntaxKind::Ident => write!(f, "<ident>"),
            SyntaxKind::LeftBrace => write!(f, "{{"),
            SyntaxKind::Comma => write!(f, ","),
            SyntaxKind::RightBrace => write!(f, "}}"),
            SyntaxKind::Enum => write!(f, "enum"),
            SyntaxKind::Fn => write!(f, "fn"),
            SyntaxKind::LeftParen => write!(f, "("),
            SyntaxKind::RightParen => write!(f, ")"),
            SyntaxKind::Arrow => write!(f, "->"),
            SyntaxKind::Colon => write!(f, ":"),
            SyntaxKind::LeftBracket => write!(f, "["),
            SyntaxKind::RightBracket => write!(f, "]"),
            SyntaxKind::Ampersand => write!(f, "&"),
            SyntaxKind::Semicolon => write!(f, ";"),
            SyntaxKind::Let => write!(f, "let"),
            SyntaxKind::Equals => write!(f, "="),
            SyntaxKind::If => write!(f, "if"),
            SyntaxKind::Else => write!(f, "else"),
            SyntaxKind::For => write!(f, "for"),
            SyntaxKind::In => write!(f, "in"),
            SyntaxKind::While => write!(f, "while"),
            SyntaxKind::Break => write!(f, "break"),
            SyntaxKind::Continue => write!(f, "continue"),
            SyntaxKind::Return => write!(f, "return"),
            SyntaxKind::Int => write!(f, "<number>"),
            SyntaxKind::Float => write!(f, "<float>"),
            SyntaxKind::True => write!(f, "true"),
            SyntaxKind::False => write!(f, "false"),
            SyntaxKind::Char => write!(f, "<char>"),
            SyntaxKind::String => write!(f, "<string>"),
            SyntaxKind::Plus => write!(f, "+"),
            SyntaxKind::Minus => write!(f, "-"),
            SyntaxKind::Star => write!(f, "*"),
            SyntaxKind::Slash => write!(f, "/"),
            SyntaxKind::Percent => write!(f, "%"),
            SyntaxKind::Eq => write!(f, "=="),
            SyntaxKind::Neq => write!(f, "!="),
            SyntaxKind::Lt => write!(f, "<"),
            SyntaxKind::Le => write!(f, "<="),
            SyntaxKind::Gt => write!(f, ">"),
            SyntaxKind::Ge => write!(f, ">="),
            SyntaxKind::And => write!(f, "&&"),
            SyntaxKind::Or => write!(f, "||"),
            SyntaxKind::PlusEquals => write!(f, "+="),
            SyntaxKind::MinusEquals => write!(f, "-="),
            SyntaxKind::StarEquals => write!(f, "*="),
            SyntaxKind::SlashEquals => write!(f, "/="),
            SyntaxKind::PercentEquals => write!(f, "%="),
            SyntaxKind::Not => write!(f, "!"),
            SyntaxKind::Program => write!(f, "{}", stringify!(Program)),
            SyntaxKind::StructDef => write!(f, "{}", stringify!(StructDef)),
            SyntaxKind::EnumDef => write!(f, "{}", stringify!(EnumDef)),
            SyntaxKind::FunctionDef => write!(f, "{}", stringify!(FunctionDef)),
            SyntaxKind::Record => write!(f, "{}", stringify!(Record)),
            SyntaxKind::Param => write!(f, "{}", stringify!(Param)),
            SyntaxKind::Block => write!(f, "{}", stringify!(Block)),
            SyntaxKind::FunctionType => write!(f, "{}", stringify!(FunctionType)),
            SyntaxKind::ArrayType => write!(f, "{}", stringify!(ArrayType)),
            SyntaxKind::PtrType => write!(f, "{}", stringify!(PtrType)),
            SyntaxKind::CustomType => write!(f, "{}", stringify!(CustomType)),
            SyntaxKind::ExprStmt => write!(f, "{}", stringify!(ExprStmt)),
            SyntaxKind::LetStmt => write!(f, "{}", stringify!(LetStmt)),
            SyntaxKind::IfStmt => write!(f, "{}", stringify!(IfStmt)),
            SyntaxKind::ForStmt => write!(f, "{}", stringify!(ForStmt)),
            SyntaxKind::WhileStmt => write!(f, "{}", stringify!(WhileStmt)),
            SyntaxKind::BreakStmt => write!(f, "{}", stringify!(BreakStmt)),
            SyntaxKind::ContinueStmt => write!(f, "{}", stringify!(ContinueStmt)),
            SyntaxKind::ReturnStmt => write!(f, "{}", stringify!(ReturnStmt)),
            SyntaxKind::InfixExpr => write!(f, "{}", stringify!(InfixExpr)),
            SyntaxKind::PrefixExpr => write!(f, "{}", stringify!(PrefixExpr)),
            SyntaxKind::ParenExpr => write!(f, "{}", stringify!(ParenExpr)),
            SyntaxKind::CallExpr => write!(f, "{}", stringify!(CallExpr)),
            SyntaxKind::ArrayExpr => write!(f, "{}", stringify!(ArrayExpr)),
            SyntaxKind::IndexExpr => write!(f, "{}", stringify!(IndexExpr)),
            SyntaxKind::StructExpr => write!(f, "{}", stringify!(StructExpr)),
            SyntaxKind::Whitespace => write!(f, "<ws>"),
            SyntaxKind::Comment => write!(f, "<comment>"),
            SyntaxKind::Error => write!(f, "<error>"),
            SyntaxKind::Eof => write!(f, "<EOF>"),
        }
    }
}
