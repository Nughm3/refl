pub use cstree::interning::TokenKey;
pub use generated::*;
pub use text_size::TextRange;

use crate::span::Span;

mod generated;

pub type SyntaxNode = cstree::syntax::SyntaxNode<SyntaxKind>;
pub type SyntaxToken = cstree::syntax::SyntaxToken<SyntaxKind>;
pub type SyntaxElement = cstree::syntax::SyntaxElement<SyntaxKind>;
pub type SyntaxElementRef<'a> = cstree::syntax::SyntaxElementRef<'a, SyntaxKind>;
pub type SyntaxNodeChildren<'a> = cstree::syntax::SyntaxNodeChildren<'a, SyntaxKind>;

pub type ResolvedNode = cstree::syntax::ResolvedNode<SyntaxKind>;
pub type ResolvedToken = cstree::syntax::ResolvedToken<SyntaxKind>;
pub type ResolvedElement = cstree::syntax::ResolvedElement<SyntaxKind>;

pub trait AstNode: AstElement {}

pub trait AstToken: AstElement {
    fn text_key(&self) -> TokenKey;
}

pub trait AstElement: Sized {
    fn can_cast(kind: SyntaxKind) -> bool;
    fn cast(elem: SyntaxElement) -> Option<Self>
    where
        Self: Sized;
    fn span(&self) -> Span;
}
