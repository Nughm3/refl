#![allow(clippy::all)]
use std::fmt;
use crate::syntax::*;
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Struct(SyntaxToken);
impl fmt::Debug for Struct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Struct {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Struct
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Ident(SyntaxToken);
impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstToken for Ident {
    fn text_key(&self) -> TokenKey {
        self.0.text_key().unwrap()
    }
}
impl AstElement for Ident {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Ident
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LeftBrace(SyntaxToken);
impl fmt::Debug for LeftBrace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for LeftBrace {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::LeftBrace
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Comma(SyntaxToken);
impl fmt::Debug for Comma {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Comma {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Comma
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct RightBrace(SyntaxToken);
impl fmt::Debug for RightBrace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for RightBrace {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::RightBrace
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Enum(SyntaxToken);
impl fmt::Debug for Enum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Enum {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Enum
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Fn(SyntaxToken);
impl fmt::Debug for Fn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Fn {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Fn
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LeftParen(SyntaxToken);
impl fmt::Debug for LeftParen {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for LeftParen {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::LeftParen
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct RightParen(SyntaxToken);
impl fmt::Debug for RightParen {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for RightParen {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::RightParen
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Arrow(SyntaxToken);
impl fmt::Debug for Arrow {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Arrow {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Arrow
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Colon(SyntaxToken);
impl fmt::Debug for Colon {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Colon {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Colon
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LeftBracket(SyntaxToken);
impl fmt::Debug for LeftBracket {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for LeftBracket {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::LeftBracket
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct RightBracket(SyntaxToken);
impl fmt::Debug for RightBracket {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for RightBracket {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::RightBracket
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Ampersand(SyntaxToken);
impl fmt::Debug for Ampersand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Ampersand {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Ampersand
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Semicolon(SyntaxToken);
impl fmt::Debug for Semicolon {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Semicolon {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Semicolon
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Let(SyntaxToken);
impl fmt::Debug for Let {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Let {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Let
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Equals(SyntaxToken);
impl fmt::Debug for Equals {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Equals {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Equals
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct If(SyntaxToken);
impl fmt::Debug for If {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for If {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::If
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Else(SyntaxToken);
impl fmt::Debug for Else {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Else {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Else
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct For(SyntaxToken);
impl fmt::Debug for For {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for For {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::For
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct In(SyntaxToken);
impl fmt::Debug for In {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for In {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::In
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct While(SyntaxToken);
impl fmt::Debug for While {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for While {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::While
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Break(SyntaxToken);
impl fmt::Debug for Break {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Break {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Break
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Continue(SyntaxToken);
impl fmt::Debug for Continue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Continue {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Continue
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Return(SyntaxToken);
impl fmt::Debug for Return {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Return {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Return
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Int(SyntaxToken);
impl fmt::Debug for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstToken for Int {
    fn text_key(&self) -> TokenKey {
        self.0.text_key().unwrap()
    }
}
impl AstElement for Int {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Int
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Float(SyntaxToken);
impl fmt::Debug for Float {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstToken for Float {
    fn text_key(&self) -> TokenKey {
        self.0.text_key().unwrap()
    }
}
impl AstElement for Float {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Float
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct True(SyntaxToken);
impl fmt::Debug for True {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for True {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::True
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct False(SyntaxToken);
impl fmt::Debug for False {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for False {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::False
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Char(SyntaxToken);
impl fmt::Debug for Char {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstToken for Char {
    fn text_key(&self) -> TokenKey {
        self.0.text_key().unwrap()
    }
}
impl AstElement for Char {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Char
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct String(SyntaxToken);
impl fmt::Debug for String {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstToken for String {
    fn text_key(&self) -> TokenKey {
        self.0.text_key().unwrap()
    }
}
impl AstElement for String {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::String
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Plus(SyntaxToken);
impl fmt::Debug for Plus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Plus {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Plus
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Minus(SyntaxToken);
impl fmt::Debug for Minus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Minus {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Minus
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Star(SyntaxToken);
impl fmt::Debug for Star {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Star {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Star
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Slash(SyntaxToken);
impl fmt::Debug for Slash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Slash {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Slash
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Percent(SyntaxToken);
impl fmt::Debug for Percent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Percent {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Percent
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Eq(SyntaxToken);
impl fmt::Debug for Eq {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Eq {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Eq
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Neq(SyntaxToken);
impl fmt::Debug for Neq {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Neq {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Neq
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Lt(SyntaxToken);
impl fmt::Debug for Lt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Lt {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Lt
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Le(SyntaxToken);
impl fmt::Debug for Le {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Le {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Le
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Gt(SyntaxToken);
impl fmt::Debug for Gt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Gt {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Gt
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Ge(SyntaxToken);
impl fmt::Debug for Ge {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Ge {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Ge
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct And(SyntaxToken);
impl fmt::Debug for And {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for And {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::And
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Or(SyntaxToken);
impl fmt::Debug for Or {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Or {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Or
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct PlusEquals(SyntaxToken);
impl fmt::Debug for PlusEquals {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for PlusEquals {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::PlusEquals
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct MinusEquals(SyntaxToken);
impl fmt::Debug for MinusEquals {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for MinusEquals {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::MinusEquals
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct StarEquals(SyntaxToken);
impl fmt::Debug for StarEquals {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for StarEquals {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::StarEquals
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct SlashEquals(SyntaxToken);
impl fmt::Debug for SlashEquals {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for SlashEquals {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::SlashEquals
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct PercentEquals(SyntaxToken);
impl fmt::Debug for PercentEquals {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for PercentEquals {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::PercentEquals
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Not(SyntaxToken);
impl fmt::Debug for Not {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstElement for Not {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Not
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let tok = elem.into_token()?;
        Self::can_cast(tok.kind()).then(|| Self(tok))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
