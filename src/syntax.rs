pub use generated::*;
pub use text_size::TextRange;

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

pub trait AstToken: AstElement {}

pub trait AstElement: Sized {
    fn can_cast(kind: SyntaxKind) -> bool;
    fn cast(elem: SyntaxElement) -> Option<Self>
    where
        Self: Sized;
    fn span(&self) -> TextRange;
    fn inner(self) -> SyntaxElement;
}

pub(crate) fn children<'a, T: 'a + AstElement>(
    node: &'a SyntaxNode,
) -> impl Iterator<Item = T> + 'a {
    node.children_with_tokens()
        .map(|x| match x {
            SyntaxElementRef::Node(node) => SyntaxElement::Node(node.clone()),
            SyntaxElementRef::Token(token) => SyntaxElement::Token(token.clone()),
        })
        .filter_map(T::cast)
}
