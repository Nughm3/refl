#![allow(clippy::all)]
use crate::syntax::*;
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Program(SyntaxNode);
impl std::fmt::Debug for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl Program {
    pub fn items(&self) -> impl Iterator<Item = Item> + '_ {
        children(&self.0)
    }
}
pub enum Item {
    StructDef(StructDef),
    EnumDef(EnumDef),
    Function(Function),
}
impl std::fmt::Debug for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::StructDef(x) => std::fmt::Debug::fmt(x, f),
            Self::EnumDef(x) => std::fmt::Debug::fmt(x, f),
            Self::Function(x) => std::fmt::Debug::fmt(x, f),
        }
    }
}
impl AstNode for Item {}
impl AstElement for Item {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind, | SyntaxKind::StructDef | SyntaxKind::EnumDef | SyntaxKind::Function
        )
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        match elem.kind() {
            SyntaxKind::StructDef => AstElement::cast(elem.clone()).map(Self::StructDef),
            SyntaxKind::EnumDef => AstElement::cast(elem.clone()).map(Self::EnumDef),
            SyntaxKind::Function => AstElement::cast(elem.clone()).map(Self::Function),
            _ => None,
        }
    }
    fn span(&self) -> TextRange {
        match self {
            Self::StructDef(x) => x.span(),
            Self::EnumDef(x) => x.span(),
            Self::Function(x) => x.span(),
        }
    }
    fn inner(self) -> SyntaxElement {
        match self {
            Self::StructDef(x) => x.inner(),
            Self::EnumDef(x) => x.inner(),
            Self::Function(x) => x.inner(),
        }
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct StructDef(SyntaxNode);
impl std::fmt::Debug for StructDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl StructDef {
    pub fn struct_(&self) -> Option<Struct> {
        children(&self.0).nth(0usize)
    }
    pub fn name(&self) -> Option<Ident> {
        children(&self.0).nth(0usize)
    }
    pub fn body(&self) -> Option<StructBody> {
        children(&self.0).nth(0usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct EnumDef(SyntaxNode);
impl std::fmt::Debug for EnumDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl EnumDef {
    pub fn enum_(&self) -> Option<Enum> {
        children(&self.0).nth(0usize)
    }
    pub fn name(&self) -> Option<Ident> {
        children(&self.0).nth(0usize)
    }
    pub fn left_brace(&self) -> Option<LeftBrace> {
        children(&self.0).nth(0usize)
    }
    pub fn right_brace(&self) -> Option<RightBrace> {
        children(&self.0).nth(0usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Function(SyntaxNode);
impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for Function {}
impl AstElement for Function {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::Function
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl Function {
    pub fn fn_(&self) -> Option<Fn> {
        children(&self.0).nth(0usize)
    }
    pub fn name(&self) -> Option<Ident> {
        children(&self.0).nth(0usize)
    }
    pub fn left_paren(&self) -> Option<LeftParen> {
        children(&self.0).nth(0usize)
    }
    pub fn right_paren(&self) -> Option<RightParen> {
        children(&self.0).nth(0usize)
    }
    pub fn colon(&self) -> Option<Colon> {
        children(&self.0).nth(0usize)
    }
    pub fn return_ty(&self) -> Option<Type> {
        children(&self.0).nth(0usize)
    }
    pub fn body(&self) -> Option<Block> {
        children(&self.0).nth(0usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct StructBody(SyntaxNode);
impl std::fmt::Debug for StructBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
    }
}
impl AstNode for StructBody {}
impl AstElement for StructBody {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::StructBody
    }
    fn cast(elem: SyntaxElement) -> Option<Self> {
        let node = elem.into_node()?;
        Self::can_cast(node.kind()).then(|| Self(node))
    }
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl StructBody {
    pub fn left_brace(&self) -> Option<LeftBrace> {
        children(&self.0).nth(0usize)
    }
    pub fn right_brace(&self) -> Option<RightBrace> {
        children(&self.0).nth(0usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Param(SyntaxNode);
impl std::fmt::Debug for Param {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl Param {
    pub fn name(&self) -> Option<Ident> {
        children(&self.0).nth(0usize)
    }
    pub fn colon(&self) -> Option<Colon> {
        children(&self.0).nth(0usize)
    }
    pub fn ty(&self) -> Option<Type> {
        children(&self.0).nth(0usize)
    }
}
pub enum Type {
    IntType(IntType),
    FloatType(FloatType),
    BoolType(BoolType),
    CharType(CharType),
    StringType(StringType),
    FunctionType(FunctionType),
    ArrayType(ArrayType),
    PtrType(PtrType),
    CustomType(CustomType),
}
impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IntType(x) => std::fmt::Debug::fmt(x, f),
            Self::FloatType(x) => std::fmt::Debug::fmt(x, f),
            Self::BoolType(x) => std::fmt::Debug::fmt(x, f),
            Self::CharType(x) => std::fmt::Debug::fmt(x, f),
            Self::StringType(x) => std::fmt::Debug::fmt(x, f),
            Self::FunctionType(x) => std::fmt::Debug::fmt(x, f),
            Self::ArrayType(x) => std::fmt::Debug::fmt(x, f),
            Self::PtrType(x) => std::fmt::Debug::fmt(x, f),
            Self::CustomType(x) => std::fmt::Debug::fmt(x, f),
        }
    }
}
impl AstNode for Type {}
impl AstElement for Type {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind, | SyntaxKind::IntType | SyntaxKind::FloatType | SyntaxKind::BoolType |
            SyntaxKind::CharType | SyntaxKind::StringType | SyntaxKind::FunctionType |
            SyntaxKind::ArrayType | SyntaxKind::PtrType | SyntaxKind::CustomType
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
            SyntaxKind::IntType => AstElement::cast(elem.clone()).map(Self::IntType),
            SyntaxKind::FloatType => AstElement::cast(elem.clone()).map(Self::FloatType),
            SyntaxKind::BoolType => AstElement::cast(elem.clone()).map(Self::BoolType),
            SyntaxKind::CharType => AstElement::cast(elem.clone()).map(Self::CharType),
            SyntaxKind::StringType => {
                AstElement::cast(elem.clone()).map(Self::StringType)
            }
            _ => None,
        }
    }
    fn span(&self) -> TextRange {
        match self {
            Self::IntType(x) => x.span(),
            Self::FloatType(x) => x.span(),
            Self::BoolType(x) => x.span(),
            Self::CharType(x) => x.span(),
            Self::StringType(x) => x.span(),
            Self::FunctionType(x) => x.span(),
            Self::ArrayType(x) => x.span(),
            Self::PtrType(x) => x.span(),
            Self::CustomType(x) => x.span(),
        }
    }
    fn inner(self) -> SyntaxElement {
        match self {
            Self::IntType(x) => x.inner(),
            Self::FloatType(x) => x.inner(),
            Self::BoolType(x) => x.inner(),
            Self::CharType(x) => x.inner(),
            Self::StringType(x) => x.inner(),
            Self::FunctionType(x) => x.inner(),
            Self::ArrayType(x) => x.inner(),
            Self::PtrType(x) => x.inner(),
            Self::CustomType(x) => x.inner(),
        }
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Block(SyntaxNode);
impl std::fmt::Debug for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl Block {
    pub fn left_brace(&self) -> Option<LeftBrace> {
        children(&self.0).nth(0usize)
    }
    pub fn stmts(&self) -> impl Iterator<Item = Stmt> + '_ {
        children(&self.0)
    }
    pub fn right_brace(&self) -> Option<RightBrace> {
        children(&self.0).nth(0usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FunctionType(SyntaxNode);
impl std::fmt::Debug for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl FunctionType {
    pub fn fn_(&self) -> Option<Fn> {
        children(&self.0).nth(0usize)
    }
    pub fn left_paren(&self) -> Option<LeftParen> {
        children(&self.0).nth(0usize)
    }
    pub fn right_paren(&self) -> Option<RightParen> {
        children(&self.0).nth(0usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ArrayType(SyntaxNode);
impl std::fmt::Debug for ArrayType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl ArrayType {
    pub fn left_bracket(&self) -> Option<LeftBracket> {
        children(&self.0).nth(0usize)
    }
    pub fn ty(&self) -> Option<Type> {
        children(&self.0).nth(0usize)
    }
    pub fn right_bracket(&self) -> Option<RightBracket> {
        children(&self.0).nth(0usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct PtrType(SyntaxNode);
impl std::fmt::Debug for PtrType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl PtrType {
    pub fn ampersand(&self) -> Option<Ampersand> {
        children(&self.0).nth(0usize)
    }
    pub fn ty(&self) -> Option<Type> {
        children(&self.0).nth(0usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CustomType(SyntaxNode);
impl std::fmt::Debug for CustomType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl CustomType {
    pub fn name(&self) -> Option<Ident> {
        children(&self.0).nth(0usize)
    }
}
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
impl std::fmt::Debug for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Semicolon(x) => std::fmt::Debug::fmt(x, f),
            Self::ExprStmt(x) => std::fmt::Debug::fmt(x, f),
            Self::LetStmt(x) => std::fmt::Debug::fmt(x, f),
            Self::IfStmt(x) => std::fmt::Debug::fmt(x, f),
            Self::ForStmt(x) => std::fmt::Debug::fmt(x, f),
            Self::WhileStmt(x) => std::fmt::Debug::fmt(x, f),
            Self::BreakStmt(x) => std::fmt::Debug::fmt(x, f),
            Self::ContinueStmt(x) => std::fmt::Debug::fmt(x, f),
            Self::ReturnStmt(x) => std::fmt::Debug::fmt(x, f),
            Self::Item(x) => std::fmt::Debug::fmt(x, f),
        }
    }
}
impl AstNode for Stmt {}
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
    fn span(&self) -> TextRange {
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
    fn inner(self) -> SyntaxElement {
        match self {
            Self::Semicolon(x) => x.inner(),
            Self::ExprStmt(x) => x.inner(),
            Self::LetStmt(x) => x.inner(),
            Self::IfStmt(x) => x.inner(),
            Self::ForStmt(x) => x.inner(),
            Self::WhileStmt(x) => x.inner(),
            Self::BreakStmt(x) => x.inner(),
            Self::ContinueStmt(x) => x.inner(),
            Self::ReturnStmt(x) => x.inner(),
            Self::Item(x) => x.inner(),
        }
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ExprStmt(SyntaxNode);
impl std::fmt::Debug for ExprStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl ExprStmt {
    pub fn expr(&self) -> Option<Expr> {
        children(&self.0).nth(0usize)
    }
    pub fn semicolon(&self) -> Option<Semicolon> {
        children(&self.0).nth(0usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LetStmt(SyntaxNode);
impl std::fmt::Debug for LetStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl LetStmt {
    pub fn let_(&self) -> Option<Let> {
        children(&self.0).nth(0usize)
    }
    pub fn colon(&self) -> Option<Colon> {
        children(&self.0).nth(0usize)
    }
    pub fn ty(&self) -> Option<Type> {
        children(&self.0).nth(0usize)
    }
    pub fn equals(&self) -> Option<Equals> {
        children(&self.0).nth(0usize)
    }
    pub fn value(&self) -> Option<Expr> {
        children(&self.0).nth(0usize)
    }
    pub fn semicolon(&self) -> Option<Semicolon> {
        children(&self.0).nth(0usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct IfStmt(SyntaxNode);
impl std::fmt::Debug for IfStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl IfStmt {
    pub fn if_(&self) -> Option<If> {
        children(&self.0).nth(0usize)
    }
    pub fn condition(&self) -> Option<Expr> {
        children(&self.0).nth(0usize)
    }
    pub fn then_branch(&self) -> Option<Block> {
        children(&self.0).nth(0usize)
    }
    pub fn else_(&self) -> Option<Else> {
        children(&self.0).nth(0usize)
    }
    pub fn else_branch(&self) -> Option<ElseStmt> {
        children(&self.0).nth(0usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ForStmt(SyntaxNode);
impl std::fmt::Debug for ForStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl ForStmt {
    pub fn for_(&self) -> Option<For> {
        children(&self.0).nth(0usize)
    }
    pub fn binding(&self) -> Option<Ident> {
        children(&self.0).nth(0usize)
    }
    pub fn in_(&self) -> Option<In> {
        children(&self.0).nth(0usize)
    }
    pub fn iterator(&self) -> Option<Expr> {
        children(&self.0).nth(0usize)
    }
    pub fn body(&self) -> Option<Block> {
        children(&self.0).nth(0usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct WhileStmt(SyntaxNode);
impl std::fmt::Debug for WhileStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl WhileStmt {
    pub fn while_(&self) -> Option<While> {
        children(&self.0).nth(0usize)
    }
    pub fn condition(&self) -> Option<Expr> {
        children(&self.0).nth(0usize)
    }
    pub fn body(&self) -> Option<Block> {
        children(&self.0).nth(0usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct BreakStmt(SyntaxNode);
impl std::fmt::Debug for BreakStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl BreakStmt {
    pub fn break_(&self) -> Option<Break> {
        children(&self.0).nth(0usize)
    }
    pub fn semicolon(&self) -> Option<Semicolon> {
        children(&self.0).nth(0usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ContinueStmt(SyntaxNode);
impl std::fmt::Debug for ContinueStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl ContinueStmt {
    pub fn continue_(&self) -> Option<Continue> {
        children(&self.0).nth(0usize)
    }
    pub fn semicolon(&self) -> Option<Semicolon> {
        children(&self.0).nth(0usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ReturnStmt(SyntaxNode);
impl std::fmt::Debug for ReturnStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl ReturnStmt {
    pub fn return_(&self) -> Option<Return> {
        children(&self.0).nth(0usize)
    }
    pub fn value(&self) -> Option<Expr> {
        children(&self.0).nth(0usize)
    }
    pub fn semicolon(&self) -> Option<Semicolon> {
        children(&self.0).nth(0usize)
    }
}
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
impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LiteralExpr(x) => std::fmt::Debug::fmt(x, f),
            Self::InfixExpr(x) => std::fmt::Debug::fmt(x, f),
            Self::PrefixExpr(x) => std::fmt::Debug::fmt(x, f),
            Self::ParenExpr(x) => std::fmt::Debug::fmt(x, f),
            Self::CallExpr(x) => std::fmt::Debug::fmt(x, f),
            Self::ArrayExpr(x) => std::fmt::Debug::fmt(x, f),
            Self::IndexExpr(x) => std::fmt::Debug::fmt(x, f),
            Self::StructExpr(x) => std::fmt::Debug::fmt(x, f),
        }
    }
}
impl AstNode for Expr {}
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
    fn span(&self) -> TextRange {
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
    fn inner(self) -> SyntaxElement {
        match self {
            Self::LiteralExpr(x) => x.inner(),
            Self::InfixExpr(x) => x.inner(),
            Self::PrefixExpr(x) => x.inner(),
            Self::ParenExpr(x) => x.inner(),
            Self::CallExpr(x) => x.inner(),
            Self::ArrayExpr(x) => x.inner(),
            Self::IndexExpr(x) => x.inner(),
            Self::StructExpr(x) => x.inner(),
        }
    }
}
pub enum ElseStmt {
    IfStmt(IfStmt),
    Block(Block),
}
impl std::fmt::Debug for ElseStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IfStmt(x) => std::fmt::Debug::fmt(x, f),
            Self::Block(x) => std::fmt::Debug::fmt(x, f),
        }
    }
}
impl AstNode for ElseStmt {}
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
    fn span(&self) -> TextRange {
        match self {
            Self::IfStmt(x) => x.span(),
            Self::Block(x) => x.span(),
        }
    }
    fn inner(self) -> SyntaxElement {
        match self {
            Self::IfStmt(x) => x.inner(),
            Self::Block(x) => x.inner(),
        }
    }
}
pub enum LiteralExpr {
    Int(Int),
    Float(Float),
    True(True),
    False(False),
    Char(Char),
    String(String),
    Ident(Ident),
}
impl std::fmt::Debug for LiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(x) => std::fmt::Debug::fmt(x, f),
            Self::Float(x) => std::fmt::Debug::fmt(x, f),
            Self::True(x) => std::fmt::Debug::fmt(x, f),
            Self::False(x) => std::fmt::Debug::fmt(x, f),
            Self::Char(x) => std::fmt::Debug::fmt(x, f),
            Self::String(x) => std::fmt::Debug::fmt(x, f),
            Self::Ident(x) => std::fmt::Debug::fmt(x, f),
        }
    }
}
impl AstNode for LiteralExpr {}
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
    fn span(&self) -> TextRange {
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
    fn inner(self) -> SyntaxElement {
        match self {
            Self::Int(x) => x.inner(),
            Self::Float(x) => x.inner(),
            Self::True(x) => x.inner(),
            Self::False(x) => x.inner(),
            Self::Char(x) => x.inner(),
            Self::String(x) => x.inner(),
            Self::Ident(x) => x.inner(),
        }
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct InfixExpr(SyntaxNode);
impl std::fmt::Debug for InfixExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl InfixExpr {
    pub fn lhs(&self) -> Option<Expr> {
        children(&self.0).nth(0usize)
    }
    pub fn op(&self) -> Option<InfixOp> {
        children(&self.0).nth(0usize)
    }
    pub fn rhs(&self) -> Option<Expr> {
        children(&self.0).nth(1usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct PrefixExpr(SyntaxNode);
impl std::fmt::Debug for PrefixExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl PrefixExpr {
    pub fn op(&self) -> Option<PrefixOp> {
        children(&self.0).nth(0usize)
    }
    pub fn child(&self) -> Option<Expr> {
        children(&self.0).nth(0usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ParenExpr(SyntaxNode);
impl std::fmt::Debug for ParenExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl ParenExpr {
    pub fn left_paren(&self) -> Option<LeftParen> {
        children(&self.0).nth(0usize)
    }
    pub fn child(&self) -> Option<Expr> {
        children(&self.0).nth(0usize)
    }
    pub fn right_paren(&self) -> Option<RightParen> {
        children(&self.0).nth(0usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CallExpr(SyntaxNode);
impl std::fmt::Debug for CallExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl CallExpr {
    pub fn child(&self) -> Option<Expr> {
        children(&self.0).nth(0usize)
    }
    pub fn left_paren(&self) -> Option<LeftParen> {
        children(&self.0).nth(0usize)
    }
    pub fn right_paren(&self) -> Option<RightParen> {
        children(&self.0).nth(0usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ArrayExpr(SyntaxNode);
impl std::fmt::Debug for ArrayExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl ArrayExpr {
    pub fn left_bracket(&self) -> Option<LeftBracket> {
        children(&self.0).nth(0usize)
    }
    pub fn right_bracket(&self) -> Option<RightBracket> {
        children(&self.0).nth(0usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct IndexExpr(SyntaxNode);
impl std::fmt::Debug for IndexExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl IndexExpr {
    pub fn array(&self) -> Option<Expr> {
        children(&self.0).nth(0usize)
    }
    pub fn left_bracket(&self) -> Option<LeftBracket> {
        children(&self.0).nth(0usize)
    }
    pub fn index(&self) -> Option<Expr> {
        children(&self.0).nth(1usize)
    }
    pub fn right_bracket(&self) -> Option<RightBracket> {
        children(&self.0).nth(0usize)
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct StructExpr(SyntaxNode);
impl std::fmt::Debug for StructExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
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
    fn span(&self) -> TextRange {
        self.0.text_range()
    }
    fn inner(self) -> SyntaxElement {
        self.0.into()
    }
}
impl StructExpr {
    pub fn name(&self) -> Option<Ident> {
        children(&self.0).nth(0usize)
    }
    pub fn left_brace(&self) -> Option<LeftBrace> {
        children(&self.0).nth(0usize)
    }
    pub fn right_brace(&self) -> Option<RightBrace> {
        children(&self.0).nth(0usize)
    }
}
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
impl std::fmt::Debug for InfixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Plus(x) => std::fmt::Debug::fmt(x, f),
            Self::Minus(x) => std::fmt::Debug::fmt(x, f),
            Self::Star(x) => std::fmt::Debug::fmt(x, f),
            Self::Slash(x) => std::fmt::Debug::fmt(x, f),
            Self::Percent(x) => std::fmt::Debug::fmt(x, f),
            Self::Eq(x) => std::fmt::Debug::fmt(x, f),
            Self::Neq(x) => std::fmt::Debug::fmt(x, f),
            Self::Lt(x) => std::fmt::Debug::fmt(x, f),
            Self::Le(x) => std::fmt::Debug::fmt(x, f),
            Self::Gt(x) => std::fmt::Debug::fmt(x, f),
            Self::Ge(x) => std::fmt::Debug::fmt(x, f),
            Self::And(x) => std::fmt::Debug::fmt(x, f),
            Self::Or(x) => std::fmt::Debug::fmt(x, f),
            Self::Equals(x) => std::fmt::Debug::fmt(x, f),
            Self::PlusEquals(x) => std::fmt::Debug::fmt(x, f),
            Self::MinusEquals(x) => std::fmt::Debug::fmt(x, f),
            Self::StarEquals(x) => std::fmt::Debug::fmt(x, f),
            Self::SlashEquals(x) => std::fmt::Debug::fmt(x, f),
            Self::PercentEquals(x) => std::fmt::Debug::fmt(x, f),
        }
    }
}
impl AstNode for InfixOp {}
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
    fn span(&self) -> TextRange {
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
    fn inner(self) -> SyntaxElement {
        match self {
            Self::Plus(x) => x.inner(),
            Self::Minus(x) => x.inner(),
            Self::Star(x) => x.inner(),
            Self::Slash(x) => x.inner(),
            Self::Percent(x) => x.inner(),
            Self::Eq(x) => x.inner(),
            Self::Neq(x) => x.inner(),
            Self::Lt(x) => x.inner(),
            Self::Le(x) => x.inner(),
            Self::Gt(x) => x.inner(),
            Self::Ge(x) => x.inner(),
            Self::And(x) => x.inner(),
            Self::Or(x) => x.inner(),
            Self::Equals(x) => x.inner(),
            Self::PlusEquals(x) => x.inner(),
            Self::MinusEquals(x) => x.inner(),
            Self::StarEquals(x) => x.inner(),
            Self::SlashEquals(x) => x.inner(),
            Self::PercentEquals(x) => x.inner(),
        }
    }
}
pub enum PrefixOp {
    Plus(Plus),
    Minus(Minus),
    Not(Not),
    Ampersand(Ampersand),
    Star(Star),
}
impl std::fmt::Debug for PrefixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Plus(x) => std::fmt::Debug::fmt(x, f),
            Self::Minus(x) => std::fmt::Debug::fmt(x, f),
            Self::Not(x) => std::fmt::Debug::fmt(x, f),
            Self::Ampersand(x) => std::fmt::Debug::fmt(x, f),
            Self::Star(x) => std::fmt::Debug::fmt(x, f),
        }
    }
}
impl AstNode for PrefixOp {}
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
    fn span(&self) -> TextRange {
        match self {
            Self::Plus(x) => x.span(),
            Self::Minus(x) => x.span(),
            Self::Not(x) => x.span(),
            Self::Ampersand(x) => x.span(),
            Self::Star(x) => x.span(),
        }
    }
    fn inner(self) -> SyntaxElement {
        match self {
            Self::Plus(x) => x.inner(),
            Self::Minus(x) => x.inner(),
            Self::Not(x) => x.inner(),
            Self::Ampersand(x) => x.inner(),
            Self::Star(x) => x.inner(),
        }
    }
}
