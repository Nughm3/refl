#![allow(clippy::all)]
use std::fmt;
use crate::syntax::*;
fn children<'a, T: 'a + AstElement>(
    node: &'a SyntaxNode,
) -> impl Iterator<Item = T> + 'a {
    node.children_with_tokens()
        .map(|elem| match elem {
            SyntaxElementRef::Node(node) => SyntaxElement::Node(node.clone()),
            SyntaxElementRef::Token(token) => SyntaxElement::Token(token.clone()),
        })
        .filter_map(T::cast)
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Program(SyntaxNode);
impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for Program {}
impl AstElement for Program {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Program
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl Program {
    pub fn items(&self) -> impl Iterator<Item = Item> + '_ {
        children(&self.0)
    }
}
#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Item {
    StructDef(StructDef),
    EnumDef(EnumDef),
    FunctionDef(FunctionDef),
}
impl fmt::Debug for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::StructDef(x) => fmt::Debug::fmt(x, f),
            Self::EnumDef(x) => fmt::Debug::fmt(x, f),
            Self::FunctionDef(x) => fmt::Debug::fmt(x, f),
        }
    }
}
impl AstElement for Item {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind, | SyntaxKind::StructDef | SyntaxKind::EnumDef | SyntaxKind::FunctionDef
        )
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        match elem.kind() {
            SyntaxKind::StructDef => AstElement::cast(elem.clone()).map(Self::StructDef),
            SyntaxKind::EnumDef => AstElement::cast(elem.clone()).map(Self::EnumDef),
            SyntaxKind::FunctionDef => {
                AstElement::cast(elem.clone()).map(Self::FunctionDef)
            }
            _ => None,
        }
    }
    fn span(&self) -> Span {
        match self {
            Self::StructDef(x) => x.span(),
            Self::EnumDef(x) => x.span(),
            Self::FunctionDef(x) => x.span(),
        }
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct StructDef(SyntaxNode);
impl fmt::Debug for StructDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for StructDef {}
impl AstElement for StructDef {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::StructDef
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl StructDef {
    pub fn name(&self) -> Option<Ident> {
        children(&self.0).next()
    }
    pub fn body(&self) -> Option<Record> {
        children(&self.0).next()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct EnumDef(SyntaxNode);
impl fmt::Debug for EnumDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for EnumDef {}
impl AstElement for EnumDef {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::EnumDef
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl EnumDef {
    pub fn name(&self) -> Option<Ident> {
        children(&self.0).next()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FunctionDef(SyntaxNode);
impl fmt::Debug for FunctionDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for FunctionDef {}
impl AstElement for FunctionDef {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::FunctionDef
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl FunctionDef {
    pub fn name(&self) -> Option<Ident> {
        children(&self.0).next()
    }
    pub fn return_ty(&self) -> Option<Type> {
        children(&self.0).next()
    }
    pub fn body(&self) -> Option<Block> {
        children(&self.0).next()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Record(SyntaxNode);
impl fmt::Debug for Record {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for Record {}
impl AstElement for Record {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Record
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl Record {}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Param(SyntaxNode);
impl fmt::Debug for Param {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for Param {}
impl AstElement for Param {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Param
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl Param {
    pub fn name(&self) -> Option<Ident> {
        children(&self.0).next()
    }
    pub fn ty(&self) -> Option<Type> {
        children(&self.0).next()
    }
}
#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Type {
    FunctionType(FunctionType),
    ArrayType(ArrayType),
    PtrType(PtrType),
    CustomType(CustomType),
}
impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FunctionType(x) => fmt::Debug::fmt(x, f),
            Self::ArrayType(x) => fmt::Debug::fmt(x, f),
            Self::PtrType(x) => fmt::Debug::fmt(x, f),
            Self::CustomType(x) => fmt::Debug::fmt(x, f),
        }
    }
}
impl AstElement for Type {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind, | SyntaxKind::FunctionType | SyntaxKind::ArrayType |
            SyntaxKind::PtrType | SyntaxKind::CustomType
        )
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        match elem.kind() {
            SyntaxKind::FunctionType => {
                AstElement::cast(elem.clone()).map(Self::FunctionType)
            }
            SyntaxKind::ArrayType => AstElement::cast(elem.clone()).map(Self::ArrayType),
            SyntaxKind::PtrType => AstElement::cast(elem.clone()).map(Self::PtrType),
            SyntaxKind::CustomType => {
                AstElement::cast(elem.clone()).map(Self::CustomType)
            }
            _ => None,
        }
    }
    fn span(&self) -> Span {
        match self {
            Self::FunctionType(x) => x.span(),
            Self::ArrayType(x) => x.span(),
            Self::PtrType(x) => x.span(),
            Self::CustomType(x) => x.span(),
        }
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Block(SyntaxNode);
impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for Block {}
impl AstElement for Block {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Block
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl Block {
    pub fn stmts(&self) -> impl Iterator<Item = Stmt> + '_ {
        children(&self.0)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FunctionType(SyntaxNode);
impl fmt::Debug for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for FunctionType {}
impl AstElement for FunctionType {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::FunctionType
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl FunctionType {
    pub fn return_ty(&self) -> Option<Type> {
        children(&self.0).next()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ArrayType(SyntaxNode);
impl fmt::Debug for ArrayType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for ArrayType {}
impl AstElement for ArrayType {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::ArrayType
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl ArrayType {
    pub fn ty(&self) -> Option<Type> {
        children(&self.0).next()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct PtrType(SyntaxNode);
impl fmt::Debug for PtrType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for PtrType {}
impl AstElement for PtrType {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::PtrType
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl PtrType {
    pub fn ty(&self) -> Option<Type> {
        children(&self.0).next()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CustomType(SyntaxNode);
impl fmt::Debug for CustomType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for CustomType {}
impl AstElement for CustomType {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::CustomType
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl CustomType {
    pub fn name(&self) -> Option<Ident> {
        children(&self.0).next()
    }
}
#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Stmt {
    Semicolon(Semicolon),
    ExprStmt(ExprStmt),
    LetStmt(LetStmt),
    IfStmt(IfStmt),
    ForStmt(ForStmt),
    WhileStmt(WhileStmt),
    BreakStmt(BreakStmt),
    ContinueStmt(ContinueStmt),
    ReturnStmt(ReturnStmt),
    Item(Item),
}
impl fmt::Debug for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Semicolon(x) => fmt::Debug::fmt(x, f),
            Self::ExprStmt(x) => fmt::Debug::fmt(x, f),
            Self::LetStmt(x) => fmt::Debug::fmt(x, f),
            Self::IfStmt(x) => fmt::Debug::fmt(x, f),
            Self::ForStmt(x) => fmt::Debug::fmt(x, f),
            Self::WhileStmt(x) => fmt::Debug::fmt(x, f),
            Self::BreakStmt(x) => fmt::Debug::fmt(x, f),
            Self::ContinueStmt(x) => fmt::Debug::fmt(x, f),
            Self::ReturnStmt(x) => fmt::Debug::fmt(x, f),
            Self::Item(x) => fmt::Debug::fmt(x, f),
        }
    }
}
impl AstElement for Stmt {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind, | SyntaxKind::Semicolon | SyntaxKind::ExprStmt | SyntaxKind::LetStmt |
            SyntaxKind::IfStmt | SyntaxKind::ForStmt | SyntaxKind::WhileStmt |
            SyntaxKind::BreakStmt | SyntaxKind::ContinueStmt | SyntaxKind::ReturnStmt
        ) || Item::can_cast(kind)
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        match elem.kind() {
            SyntaxKind::ExprStmt => AstElement::cast(elem.clone()).map(Self::ExprStmt),
            SyntaxKind::LetStmt => AstElement::cast(elem.clone()).map(Self::LetStmt),
            SyntaxKind::IfStmt => AstElement::cast(elem.clone()).map(Self::IfStmt),
            SyntaxKind::ForStmt => AstElement::cast(elem.clone()).map(Self::ForStmt),
            SyntaxKind::WhileStmt => AstElement::cast(elem.clone()).map(Self::WhileStmt),
            SyntaxKind::BreakStmt => AstElement::cast(elem.clone()).map(Self::BreakStmt),
            SyntaxKind::ContinueStmt => {
                AstElement::cast(elem.clone()).map(Self::ContinueStmt)
            }
            SyntaxKind::ReturnStmt => {
                AstElement::cast(elem.clone()).map(Self::ReturnStmt)
            }
            SyntaxKind::Semicolon => AstElement::cast(elem.clone()).map(Self::Semicolon),
            _ => None,
        }
            .or_else(|| AstElement::cast(elem.clone()).map(Self::Item))
    }
    fn span(&self) -> Span {
        match self {
            Self::Semicolon(x) => x.span(),
            Self::ExprStmt(x) => x.span(),
            Self::LetStmt(x) => x.span(),
            Self::IfStmt(x) => x.span(),
            Self::ForStmt(x) => x.span(),
            Self::WhileStmt(x) => x.span(),
            Self::BreakStmt(x) => x.span(),
            Self::ContinueStmt(x) => x.span(),
            Self::ReturnStmt(x) => x.span(),
            Self::Item(x) => x.span(),
        }
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ExprStmt(SyntaxNode);
impl fmt::Debug for ExprStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for ExprStmt {}
impl AstElement for ExprStmt {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::ExprStmt
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl ExprStmt {
    pub fn expr(&self) -> Option<Expr> {
        children(&self.0).next()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LetStmt(SyntaxNode);
impl fmt::Debug for LetStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for LetStmt {}
impl AstElement for LetStmt {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::LetStmt
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl LetStmt {
    pub fn ty(&self) -> Option<Type> {
        children(&self.0).next()
    }
    pub fn value(&self) -> Option<Expr> {
        children(&self.0).next()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct IfStmt(SyntaxNode);
impl fmt::Debug for IfStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for IfStmt {}
impl AstElement for IfStmt {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::IfStmt
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl IfStmt {
    pub fn condition(&self) -> Option<Expr> {
        children(&self.0).next()
    }
    pub fn then_branch(&self) -> Option<Block> {
        children(&self.0).next()
    }
    pub fn else_branch(&self) -> Option<ElseStmt> {
        children(&self.0).next()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ForStmt(SyntaxNode);
impl fmt::Debug for ForStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for ForStmt {}
impl AstElement for ForStmt {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::ForStmt
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl ForStmt {
    pub fn binding(&self) -> Option<Ident> {
        children(&self.0).next()
    }
    pub fn iterator(&self) -> Option<Expr> {
        children(&self.0).next()
    }
    pub fn body(&self) -> Option<Block> {
        children(&self.0).next()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct WhileStmt(SyntaxNode);
impl fmt::Debug for WhileStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for WhileStmt {}
impl AstElement for WhileStmt {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::WhileStmt
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl WhileStmt {
    pub fn condition(&self) -> Option<Expr> {
        children(&self.0).next()
    }
    pub fn body(&self) -> Option<Block> {
        children(&self.0).next()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct BreakStmt(SyntaxNode);
impl fmt::Debug for BreakStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for BreakStmt {}
impl AstElement for BreakStmt {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::BreakStmt
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl BreakStmt {}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ContinueStmt(SyntaxNode);
impl fmt::Debug for ContinueStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for ContinueStmt {}
impl AstElement for ContinueStmt {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::ContinueStmt
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl ContinueStmt {}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ReturnStmt(SyntaxNode);
impl fmt::Debug for ReturnStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for ReturnStmt {}
impl AstElement for ReturnStmt {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::ReturnStmt
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl ReturnStmt {
    pub fn value(&self) -> Option<Expr> {
        children(&self.0).next()
    }
}
#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Expr {
    LiteralExpr(LiteralExpr),
    InfixExpr(InfixExpr),
    PrefixExpr(PrefixExpr),
    ParenExpr(ParenExpr),
    CallExpr(CallExpr),
    ArrayExpr(ArrayExpr),
    IndexExpr(IndexExpr),
    StructExpr(StructExpr),
}
impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LiteralExpr(x) => fmt::Debug::fmt(x, f),
            Self::InfixExpr(x) => fmt::Debug::fmt(x, f),
            Self::PrefixExpr(x) => fmt::Debug::fmt(x, f),
            Self::ParenExpr(x) => fmt::Debug::fmt(x, f),
            Self::CallExpr(x) => fmt::Debug::fmt(x, f),
            Self::ArrayExpr(x) => fmt::Debug::fmt(x, f),
            Self::IndexExpr(x) => fmt::Debug::fmt(x, f),
            Self::StructExpr(x) => fmt::Debug::fmt(x, f),
        }
    }
}
impl AstElement for Expr {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind, | SyntaxKind::InfixExpr | SyntaxKind::PrefixExpr |
            SyntaxKind::ParenExpr | SyntaxKind::CallExpr | SyntaxKind::ArrayExpr |
            SyntaxKind::IndexExpr | SyntaxKind::StructExpr
        ) || LiteralExpr::can_cast(kind)
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        match elem.kind() {
            SyntaxKind::InfixExpr => AstElement::cast(elem.clone()).map(Self::InfixExpr),
            SyntaxKind::PrefixExpr => {
                AstElement::cast(elem.clone()).map(Self::PrefixExpr)
            }
            SyntaxKind::ParenExpr => AstElement::cast(elem.clone()).map(Self::ParenExpr),
            SyntaxKind::CallExpr => AstElement::cast(elem.clone()).map(Self::CallExpr),
            SyntaxKind::ArrayExpr => AstElement::cast(elem.clone()).map(Self::ArrayExpr),
            SyntaxKind::IndexExpr => AstElement::cast(elem.clone()).map(Self::IndexExpr),
            SyntaxKind::StructExpr => {
                AstElement::cast(elem.clone()).map(Self::StructExpr)
            }
            _ => None,
        }
            .or_else(|| AstElement::cast(elem.clone()).map(Self::LiteralExpr))
    }
    fn span(&self) -> Span {
        match self {
            Self::LiteralExpr(x) => x.span(),
            Self::InfixExpr(x) => x.span(),
            Self::PrefixExpr(x) => x.span(),
            Self::ParenExpr(x) => x.span(),
            Self::CallExpr(x) => x.span(),
            Self::ArrayExpr(x) => x.span(),
            Self::IndexExpr(x) => x.span(),
            Self::StructExpr(x) => x.span(),
        }
    }
}
#[derive(Clone, Hash, PartialEq, Eq)]
pub enum ElseStmt {
    IfStmt(IfStmt),
    Block(Block),
}
impl fmt::Debug for ElseStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::IfStmt(x) => fmt::Debug::fmt(x, f),
            Self::Block(x) => fmt::Debug::fmt(x, f),
        }
    }
}
impl AstElement for ElseStmt {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(kind, | SyntaxKind::IfStmt | SyntaxKind::Block)
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        match elem.kind() {
            SyntaxKind::IfStmt => AstElement::cast(elem.clone()).map(Self::IfStmt),
            SyntaxKind::Block => AstElement::cast(elem.clone()).map(Self::Block),
            _ => None,
        }
    }
    fn span(&self) -> Span {
        match self {
            Self::IfStmt(x) => x.span(),
            Self::Block(x) => x.span(),
        }
    }
}
#[derive(Clone, Hash, PartialEq, Eq)]
pub enum LiteralExpr {
    Int(Int),
    Float(Float),
    True(True),
    False(False),
    Char(Char),
    String(String),
    Ident(Ident),
}
impl fmt::Debug for LiteralExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(x) => fmt::Debug::fmt(x, f),
            Self::Float(x) => fmt::Debug::fmt(x, f),
            Self::True(x) => fmt::Debug::fmt(x, f),
            Self::False(x) => fmt::Debug::fmt(x, f),
            Self::Char(x) => fmt::Debug::fmt(x, f),
            Self::String(x) => fmt::Debug::fmt(x, f),
            Self::Ident(x) => fmt::Debug::fmt(x, f),
        }
    }
}
impl AstElement for LiteralExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind, | SyntaxKind::Int | SyntaxKind::Float | SyntaxKind::True |
            SyntaxKind::False | SyntaxKind::Char | SyntaxKind::String | SyntaxKind::Ident
        )
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        match elem.kind() {
            SyntaxKind::Int => AstElement::cast(elem.clone()).map(Self::Int),
            SyntaxKind::Float => AstElement::cast(elem.clone()).map(Self::Float),
            SyntaxKind::True => AstElement::cast(elem.clone()).map(Self::True),
            SyntaxKind::False => AstElement::cast(elem.clone()).map(Self::False),
            SyntaxKind::Char => AstElement::cast(elem.clone()).map(Self::Char),
            SyntaxKind::String => AstElement::cast(elem.clone()).map(Self::String),
            SyntaxKind::Ident => AstElement::cast(elem.clone()).map(Self::Ident),
            _ => None,
        }
    }
    fn span(&self) -> Span {
        match self {
            Self::Int(x) => x.span(),
            Self::Float(x) => x.span(),
            Self::True(x) => x.span(),
            Self::False(x) => x.span(),
            Self::Char(x) => x.span(),
            Self::String(x) => x.span(),
            Self::Ident(x) => x.span(),
        }
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct InfixExpr(SyntaxNode);
impl fmt::Debug for InfixExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for InfixExpr {}
impl AstElement for InfixExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::InfixExpr
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl InfixExpr {
    pub fn lhs(&self) -> Option<Expr> {
        children(&self.0).next()
    }
    pub fn op(&self) -> Option<InfixOp> {
        children(&self.0).next()
    }
    pub fn rhs(&self) -> Option<Expr> {
        children(&self.0).nth(1usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct PrefixExpr(SyntaxNode);
impl fmt::Debug for PrefixExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for PrefixExpr {}
impl AstElement for PrefixExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::PrefixExpr
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl PrefixExpr {
    pub fn op(&self) -> Option<PrefixOp> {
        children(&self.0).next()
    }
    pub fn child(&self) -> Option<Expr> {
        children(&self.0).next()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ParenExpr(SyntaxNode);
impl fmt::Debug for ParenExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for ParenExpr {}
impl AstElement for ParenExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::ParenExpr
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl ParenExpr {
    pub fn child(&self) -> Option<Expr> {
        children(&self.0).next()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CallExpr(SyntaxNode);
impl fmt::Debug for CallExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for CallExpr {}
impl AstElement for CallExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::CallExpr
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl CallExpr {
    pub fn child(&self) -> Option<Expr> {
        children(&self.0).next()
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ArrayExpr(SyntaxNode);
impl fmt::Debug for ArrayExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for ArrayExpr {}
impl AstElement for ArrayExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::ArrayExpr
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl ArrayExpr {}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct IndexExpr(SyntaxNode);
impl fmt::Debug for IndexExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for IndexExpr {}
impl AstElement for IndexExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::IndexExpr
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl IndexExpr {
    pub fn array(&self) -> Option<Expr> {
        children(&self.0).next()
    }
    pub fn index(&self) -> Option<Expr> {
        children(&self.0).nth(1usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct StructExpr(SyntaxNode);
impl fmt::Debug for StructExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for StructExpr {}
impl AstElement for StructExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::StructExpr
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> Span {
        self.0.text_range().into()
    }
}
impl StructExpr {
    pub fn name(&self) -> Option<Ident> {
        children(&self.0).next()
    }
}
#[derive(Clone, Hash, PartialEq, Eq)]
pub enum InfixOp {
    Plus(Plus),
    Minus(Minus),
    Star(Star),
    Slash(Slash),
    Percent(Percent),
    Eq(Eq),
    Neq(Neq),
    Lt(Lt),
    Le(Le),
    Gt(Gt),
    Ge(Ge),
    And(And),
    Or(Or),
    Equals(Equals),
    PlusEquals(PlusEquals),
    MinusEquals(MinusEquals),
    StarEquals(StarEquals),
    SlashEquals(SlashEquals),
    PercentEquals(PercentEquals),
}
impl fmt::Debug for InfixOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Plus(x) => fmt::Debug::fmt(x, f),
            Self::Minus(x) => fmt::Debug::fmt(x, f),
            Self::Star(x) => fmt::Debug::fmt(x, f),
            Self::Slash(x) => fmt::Debug::fmt(x, f),
            Self::Percent(x) => fmt::Debug::fmt(x, f),
            Self::Eq(x) => fmt::Debug::fmt(x, f),
            Self::Neq(x) => fmt::Debug::fmt(x, f),
            Self::Lt(x) => fmt::Debug::fmt(x, f),
            Self::Le(x) => fmt::Debug::fmt(x, f),
            Self::Gt(x) => fmt::Debug::fmt(x, f),
            Self::Ge(x) => fmt::Debug::fmt(x, f),
            Self::And(x) => fmt::Debug::fmt(x, f),
            Self::Or(x) => fmt::Debug::fmt(x, f),
            Self::Equals(x) => fmt::Debug::fmt(x, f),
            Self::PlusEquals(x) => fmt::Debug::fmt(x, f),
            Self::MinusEquals(x) => fmt::Debug::fmt(x, f),
            Self::StarEquals(x) => fmt::Debug::fmt(x, f),
            Self::SlashEquals(x) => fmt::Debug::fmt(x, f),
            Self::PercentEquals(x) => fmt::Debug::fmt(x, f),
        }
    }
}
impl AstElement for InfixOp {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind, | SyntaxKind::Plus | SyntaxKind::Minus | SyntaxKind::Star |
            SyntaxKind::Slash | SyntaxKind::Percent | SyntaxKind::Eq | SyntaxKind::Neq |
            SyntaxKind::Lt | SyntaxKind::Le | SyntaxKind::Gt | SyntaxKind::Ge |
            SyntaxKind::And | SyntaxKind::Or | SyntaxKind::Equals |
            SyntaxKind::PlusEquals | SyntaxKind::MinusEquals | SyntaxKind::StarEquals |
            SyntaxKind::SlashEquals | SyntaxKind::PercentEquals
        )
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        match elem.kind() {
            SyntaxKind::Plus => AstElement::cast(elem.clone()).map(Self::Plus),
            SyntaxKind::Minus => AstElement::cast(elem.clone()).map(Self::Minus),
            SyntaxKind::Star => AstElement::cast(elem.clone()).map(Self::Star),
            SyntaxKind::Slash => AstElement::cast(elem.clone()).map(Self::Slash),
            SyntaxKind::Percent => AstElement::cast(elem.clone()).map(Self::Percent),
            SyntaxKind::Eq => AstElement::cast(elem.clone()).map(Self::Eq),
            SyntaxKind::Neq => AstElement::cast(elem.clone()).map(Self::Neq),
            SyntaxKind::Lt => AstElement::cast(elem.clone()).map(Self::Lt),
            SyntaxKind::Le => AstElement::cast(elem.clone()).map(Self::Le),
            SyntaxKind::Gt => AstElement::cast(elem.clone()).map(Self::Gt),
            SyntaxKind::Ge => AstElement::cast(elem.clone()).map(Self::Ge),
            SyntaxKind::And => AstElement::cast(elem.clone()).map(Self::And),
            SyntaxKind::Or => AstElement::cast(elem.clone()).map(Self::Or),
            SyntaxKind::Equals => AstElement::cast(elem.clone()).map(Self::Equals),
            SyntaxKind::PlusEquals => {
                AstElement::cast(elem.clone()).map(Self::PlusEquals)
            }
            SyntaxKind::MinusEquals => {
                AstElement::cast(elem.clone()).map(Self::MinusEquals)
            }
            SyntaxKind::StarEquals => {
                AstElement::cast(elem.clone()).map(Self::StarEquals)
            }
            SyntaxKind::SlashEquals => {
                AstElement::cast(elem.clone()).map(Self::SlashEquals)
            }
            SyntaxKind::PercentEquals => {
                AstElement::cast(elem.clone()).map(Self::PercentEquals)
            }
            _ => None,
        }
    }
    fn span(&self) -> Span {
        match self {
            Self::Plus(x) => x.span(),
            Self::Minus(x) => x.span(),
            Self::Star(x) => x.span(),
            Self::Slash(x) => x.span(),
            Self::Percent(x) => x.span(),
            Self::Eq(x) => x.span(),
            Self::Neq(x) => x.span(),
            Self::Lt(x) => x.span(),
            Self::Le(x) => x.span(),
            Self::Gt(x) => x.span(),
            Self::Ge(x) => x.span(),
            Self::And(x) => x.span(),
            Self::Or(x) => x.span(),
            Self::Equals(x) => x.span(),
            Self::PlusEquals(x) => x.span(),
            Self::MinusEquals(x) => x.span(),
            Self::StarEquals(x) => x.span(),
            Self::SlashEquals(x) => x.span(),
            Self::PercentEquals(x) => x.span(),
        }
    }
}
#[derive(Clone, Hash, PartialEq, Eq)]
pub enum PrefixOp {
    Plus(Plus),
    Minus(Minus),
    Not(Not),
    Ampersand(Ampersand),
    Star(Star),
}
impl fmt::Debug for PrefixOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Plus(x) => fmt::Debug::fmt(x, f),
            Self::Minus(x) => fmt::Debug::fmt(x, f),
            Self::Not(x) => fmt::Debug::fmt(x, f),
            Self::Ampersand(x) => fmt::Debug::fmt(x, f),
            Self::Star(x) => fmt::Debug::fmt(x, f),
        }
    }
}
impl AstElement for PrefixOp {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind, | SyntaxKind::Plus | SyntaxKind::Minus | SyntaxKind::Not |
            SyntaxKind::Ampersand | SyntaxKind::Star
        )
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        match elem.kind() {
            SyntaxKind::Plus => AstElement::cast(elem.clone()).map(Self::Plus),
            SyntaxKind::Minus => AstElement::cast(elem.clone()).map(Self::Minus),
            SyntaxKind::Not => AstElement::cast(elem.clone()).map(Self::Not),
            SyntaxKind::Ampersand => AstElement::cast(elem.clone()).map(Self::Ampersand),
            SyntaxKind::Star => AstElement::cast(elem.clone()).map(Self::Star),
            _ => None,
        }
    }
    fn span(&self) -> Span {
        match self {
            Self::Plus(x) => x.span(),
            Self::Minus(x) => x.span(),
            Self::Not(x) => x.span(),
            Self::Ampersand(x) => x.span(),
            Self::Star(x) => x.span(),
        }
    }
}
