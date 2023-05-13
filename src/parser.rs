use std::iter::Peekable;

use cstree::{
    build::{Checkpoint, GreenNodeBuilder},
    green::GreenNode,
    interning::Interner,
};
use logos::{Logos, SpannedIter};

use crate::syntax::{
    AstElement, Program, SyntaxElement,
    SyntaxKind::{self, *},
    SyntaxNode,
};

pub fn parse(input: &str) -> Option<(Program, impl Interner)> {
    let (cst, interner) = parse_cst(input);
    let elem: SyntaxElement = SyntaxNode::new_root(cst).into();
    Program::cast(elem).map(|program| (program, interner))
}

pub fn parse_cst(input: &str) -> (GreenNode, impl Interner) {
    Parser::new(input).parse()
}

struct Parser<'input> {
    input: &'input str,
    lexer: Peekable<SpannedIter<'input, SyntaxKind>>,
    builder: GreenNodeBuilder<'static, 'static, SyntaxKind>,
}

impl<'input> Parser<'input> {
    fn new(input: &'input str) -> Self {
        Parser {
            input,
            lexer: SyntaxKind::lexer(input).spanned().peekable(),
            builder: GreenNodeBuilder::new(),
        }
    }

    fn parse(mut self) -> (GreenNode, impl Interner) {
        program(&mut self);

        let (cst, interner) = self.builder.finish();
        (
            cst,
            interner
                .expect("no interner")
                .into_interner()
                .expect("couldn't create interner"),
        )
    }

    fn skip_ignored_tokens(&mut self) {
        while let Some((Ok(kind @ Whitespace | kind @ Comment), range)) = self.lexer.peek() {
            let text = &self.input[range.start..range.end];
            self.builder.token(*kind, text);
            self.lexer.next();
        }
    }

    fn next(&mut self) -> SyntaxKind {
        self.skip_ignored_tokens();

        match self.lexer.next() {
            Some((result, range)) => {
                let text = &self.input[range.start..range.end];

                let kind = result.unwrap_or(Error);
                self.builder.token(kind, text);
                kind
            }
            None => Eof,
        }
    }

    fn peek(&mut self) -> SyntaxKind {
        self.skip_ignored_tokens();
        self.lexer
            .peek()
            .map(|(res, _)| res.unwrap_or(Error))
            .unwrap_or(Eof)
    }

    fn at(&mut self, kind: SyntaxKind) -> bool {
        self.peek() == kind
    }

    fn expect(&mut self, kind: SyntaxKind) -> SyntaxKind {
        if self.at(kind) {
            self.next()
        } else {
            self.error()
        }
    }

    fn consume(&mut self, kind: SyntaxKind) -> bool {
        let consumed = self.at(kind);
        if consumed {
            self.next();
        }
        consumed
    }

    fn error(&mut self) -> SyntaxKind {
        let text = self
            .lexer
            .next()
            .map(|(_, range)| &self.input[range.start..range.end])
            .unwrap_or_default();

        self.builder.token(Error, text);
        Error
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.builder.start_node(kind);
    }

    fn start_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) {
        self.builder.start_node_at(checkpoint, kind);
    }

    fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    fn checkpoint(&self) -> Checkpoint {
        self.builder.checkpoint()
    }
}

fn program(p: &mut Parser<'_>) {
    p.start_node(Program);
    while !p.at(Eof) {
        item(p);
    }
    p.finish_node();
}

fn item(p: &mut Parser<'_>) {
    match p.peek() {
        Struct => struct_def(p),
        Enum => enum_def(p),
        Fn => function(p),
        _ => {
            p.error();
        }
    }
}

fn struct_def(p: &mut Parser<'_>) {
    p.start_node(StructDef);
    p.expect(Struct);
    p.expect(Ident);
    struct_body(p);
    p.finish_node();
}

fn struct_body(p: &mut Parser<'_>) {
    p.start_node(StructBody);
    p.expect(LeftBrace);
    comma_separated(p, RightBrace, param);
    p.expect(RightBrace);
    p.finish_node();
}

fn enum_def(p: &mut Parser<'_>) {
    p.start_node(EnumDef);

    p.expect(Enum);
    p.expect(Ident);

    p.expect(LeftBrace);
    comma_separated(p, RightBrace, |p| {
        p.expect(Ident);

        if !p.at(Comma) {
            struct_body(p);
        }
    });
    p.expect(RightBrace);

    p.finish_node();
}

fn function(p: &mut Parser<'_>) {
    p.start_node(Function);

    p.expect(Fn);
    p.expect(Ident);

    p.expect(LeftParen);
    comma_separated(p, RightParen, param);
    p.expect(RightParen);

    if p.consume(Colon) {
        ty(p);
    }

    block(p);
    p.finish_node();
}

fn param(p: &mut Parser<'_>) {
    p.start_node(Param);
    p.expect(Ident);
    p.expect(Colon);
    ty(p);
    p.finish_node();
}

fn ty(p: &mut Parser<'_>) {
    match p.peek() {
        IntType | FloatType | BoolType | CharType | StringType => {
            p.next();
        }
        Fn => {
            p.start_node(FunctionType);
            p.next();
            p.expect(LeftParen);
            comma_separated(p, RightParen, param);
            p.expect(RightParen);
            p.finish_node();
        }
        LeftBracket => {
            p.start_node(ArrayType);
            p.expect(LeftBracket);
            ty(p);
            p.expect(RightBracket);
            p.finish_node();
        }
        Ampersand => {
            p.start_node(PtrType);
            p.next();
            ty(p);
            p.finish_node();
        }
        Ident => {
            p.start_node(CustomType);
            p.next();
            p.finish_node();
        }
        _ => {
            p.error();
        }
    }
}

fn block(p: &mut Parser<'_>) {
    p.start_node(Block);
    p.expect(LeftBrace);

    while !p.at(RightBrace) {
        stmt(p);
    }

    p.expect(RightBrace);
    p.finish_node();
}

fn stmt(p: &mut Parser<'_>) {
    match p.peek() {
        Semicolon => {
            p.next();
        }
        Let => {
            p.start_node(LetStmt);
            p.next();
            p.expect(Ident);
            if p.consume(Colon) {
                ty(p);
            }
            p.expect(Equals);
            expr(p);
            p.finish_node();
        }
        If => {
            fn if_expr(p: &mut Parser<'_>) {
                p.start_node(IfStmt);
                p.expect(If);
                expr(p);
                block(p);
                if p.consume(Else) {
                    if p.at(If) {
                        if_expr(p);
                    } else {
                        block(p);
                    }
                }
                p.finish_node();
            }

            if_expr(p);
        }
        For => {
            p.start_node(ForStmt);
            p.next();
            p.expect(Ident);
            p.expect(In);
            expr(p);
            block(p);
            p.finish_node();
        }
        While => {
            p.start_node(While);
            p.next();
            expr(p);
            block(p);
            p.finish_node();
        }
        Break => {
            p.start_node(BreakStmt);
            p.next();
            p.expect(Semicolon);
            p.finish_node();
        }
        Continue => {
            p.start_node(ContinueStmt);
            p.next();
            p.expect(Semicolon);
            p.finish_node();
        }
        Return => {
            p.start_node(ReturnStmt);
            p.next();
            if !p.at(Semicolon) {
                expr(p);
            }
            p.expect(Semicolon);
            p.finish_node();
        }
        _ => {
            p.start_node(ExprStmt);
            expr(p);
            p.expect(Semicolon);
            p.finish_node();
        }
    }
}

type Power = u8;

fn expr(p: &mut Parser<'_>) {
    expr_bp(p, 0);
}

fn expr_bp(p: &mut Parser<'_>, bp: Power) {
    let checkpoint = p.checkpoint();

    if let Some(r) = prefix_bp(p.peek()) {
        p.start_node(PrefixExpr);
        p.next();
        expr_bp(p, r);
        p.finish_node();
    } else {
        match p.peek() {
            Int | Float | True | False | Char | String => {
                p.next();
            }
            LeftParen => {
                p.start_node(ParenExpr);
                p.next();
                expr(p);
                p.expect(RightParen);
                p.finish_node();
            }
            LeftBracket => {
                p.start_node(ArrayExpr);
                p.next();
                comma_separated(p, RightBracket, expr);
                p.expect(RightBracket);
                p.finish_node();
            }
            Ident => {
                p.next();

                if p.at(LeftBrace) {
                    p.start_node_at(checkpoint, StructExpr);
                    comma_separated(p, RightBrace, |p| {
                        p.expect(Ident);
                        p.expect(Colon);
                        expr(p);
                    });
                    p.expect(RightBrace);
                    p.finish_node();
                }
            }
            _ => {
                p.error();
            }
        }
    }

    loop {
        match p.peek() {
            LeftParen => {
                p.start_node_at(checkpoint, CallExpr);
                p.next();
                comma_separated(p, RightParen, expr);
                p.expect(RightParen);
                p.finish_node();
            }
            LeftBracket => {
                p.start_node_at(checkpoint, IndexExpr);
                p.next();
                expr(p);
                p.expect(RightBracket);
                p.finish_node();
            }
            infix if infix_bp(infix).is_some() => {
                let (l, r) = infix_bp(infix).unwrap();
                if l < bp {
                    break;
                }

                p.start_node_at(checkpoint, InfixExpr);
                p.next();
                expr_bp(p, r);
                p.finish_node();
            }
            _ => break,
        }
    }
}

fn prefix_bp(op: SyntaxKind) -> Option<Power> {
    Some(match op {
        Plus | Minus | Not | Ampersand | Star => 13,
        _ => return None,
    })
}

fn infix_bp(op: SyntaxKind) -> Option<(Power, Power)> {
    Some(match op {
        Star | Slash | Percent => (11, 12),
        Plus | Minus => (9, 10),
        Eq | Neq | Lt | Le | Gt | Ge => (7, 8),
        And => (5, 6),
        Or => (3, 4),
        Equals | PlusEquals | MinusEquals | StarEquals | SlashEquals | PercentEquals => (1, 2),
        _ => return None,
    })
}

fn comma_separated(p: &mut Parser<'_>, end: SyntaxKind, mut f: impl FnMut(&mut Parser<'_>)) {
    if p.at(end) {
        return;
    }

    f(p);
    while p.consume(Comma) {
        if p.at(end) {
            break;
        }

        f(p)
    }
}
