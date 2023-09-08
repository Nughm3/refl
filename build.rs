use std::{collections::HashMap, fs};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use ungrammar::{Grammar, Node, Rule};

const GRAMMAR_PATH: &str = "refl.ungram";
const OUTPUT_PATH: &str = "src/syntax/generated";

fn main() {
    println!("cargo:rerun-if-changed={GRAMMAR_PATH}");
    let grammar: Grammar = fs::read_to_string(GRAMMAR_PATH).unwrap().parse().unwrap();
    let mut gen = Generator::new(&grammar);

    write("kind.rs", gen.gen_kinds());
    write("token.rs", gen.gen_tokens());
    write("ast.rs", gen.gen_ast());
}

fn write(path: &str, tokens: TokenStream) {
    let tokens = quote! {
        #![allow(clippy::all)]
        #tokens
    };

    let file = syn::parse2(tokens).unwrap();
    let text = prettyplease::unparse(&file);
    fs::write(format!("{OUTPUT_PATH}/{path}"), text).unwrap();
}

enum NodeType {
    Struct,
    Enum,
}

enum NodeData {
    Struct(Struct),
    Enum(Enum),
}

struct Struct {
    name: String,
    fields: Vec<Field>,
    type_repeat: HashMap<String, Repeat>,
}

impl Struct {
    fn get_repeat(&mut self, ty: &String, rule: &Rule) -> usize {
        match self.type_repeat.get_mut(ty) {
            Some(Repeat::One(n)) => {
                *n += 1;
                *n
            }
            Some(Repeat::Many) => {
                panic!("rule '{rule:?}' uses type '{ty}' which was already used before",)
            }
            None => {
                self.type_repeat.insert(ty.clone(), Repeat::One(0));
                0
            }
        }
    }

    fn check_repeat(&mut self, ty: &str, r: &Rule) {
        if self.type_repeat.contains_key(ty) {
            panic!("rule '{r:?}' uses type '{ty}' which was already used before",);
        }
    }
}

struct Field {
    kind: FieldKind,
    name: String,
    ty: String,
    repeat: Repeat,
}

enum FieldKind {
    Node,
    Token,
    Hidden,
}

enum Repeat {
    One(usize),
    Many,
}

struct Enum {
    name: String,
    nodes: Vec<Variant>,
    tokens: Vec<String>,
}

struct Variant {
    name: String,
    node: Node,
}

struct TokenData {
    expr: String,
    fmt: String,
    lex: String,
    regex: bool,
}

struct Generator<'a> {
    grammar: &'a Grammar,
    node_types: HashMap<Node, NodeType>,
}

impl<'a> Generator<'a> {
    fn new(grammar: &'a Grammar) -> Self {
        Generator {
            grammar,
            node_types: grammar
                .iter()
                .map(|node| {
                    (
                        node,
                        if let Rule::Alt(_) = &grammar[node].rule {
                            NodeType::Enum
                        } else {
                            NodeType::Struct
                        },
                    )
                })
                .collect(),
        }
    }

    fn gen_kinds(&mut self) -> TokenStream {
        let token_data: Vec<_> = self
            .grammar
            .tokens()
            .map(|n| token_map(&self.grammar[n].name))
            .collect();

        let tokens: Vec<_> = token_data
            .iter()
            .map(|data| {
                let ident = format_ident!("{}", data.expr);
                let lex = data.lex.clone();

                if data.regex {
                    quote! {
                        #[regex(#lex)]
                        #ident
                    }
                } else {
                    quote! {
                        #[token(#lex)]
                        #ident
                    }
                }
            })
            .collect();

        let nodes: Vec<_> = self
            .grammar
            .iter()
            .filter_map(|n| match self.node_types[&n] {
                NodeType::Struct => Some(self.grammar[n].name.to_string()),
                NodeType::Enum => None,
            })
            .collect();

        let nodes: Vec<_> = nodes.iter().map(|node| format_ident!("{node}")).collect();

        let token_display: Vec<_> = token_data
            .iter()
            .map(|data| {
                let TokenData { expr, fmt, .. } = data;
                let name = format_ident!("{expr}");

                let fmt = if fmt == "{" || fmt == "}" {
                    fmt.repeat(2)
                } else {
                    fmt.clone()
                };

                quote! {
                    SyntaxKind::#name => write!(f, #fmt)
                }
            })
            .collect();

        let nodes_display: Vec<_> = nodes
            .iter()
            .map(|name| {
                quote! {
                    SyntaxKind::#name => write!(f, "{}", stringify!(#name))
                }
            })
            .collect();

        quote! {
            use std::fmt;

            use logos::Logos;
            use cstree::Syntax;

            #[repr(u32)]
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Logos, Syntax)]
            pub enum SyntaxKind {
                #(#tokens,)*
                #(#nodes,)*

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
                        #(#token_display,)*
                        #(#nodes_display,)*
                        SyntaxKind::Whitespace => write!(f, "<ws>"),
                        SyntaxKind::Comment => write!(f, "<comment>"),
                        SyntaxKind::Error => write!(f, "<error>"),
                        SyntaxKind::Eof => write!(f, "<EOF>"),
                    }
                }
            }
        }
    }

    fn gen_tokens(&mut self) -> TokenStream {
        let tokens = self.grammar.tokens().map(|token| {
            let TokenData { expr, regex, .. } = token_map(&self.grammar[token].name);

            let ident = format_ident!("{expr}");

            let token_trait_impl = if regex {
                quote! {
                    impl AstToken for #ident {
                        fn text_key(&self) -> TokenKey {
                            self.0.text_key().unwrap()
                        }
                    }
                }
            } else {
                quote!()
            };

            quote! {
                #[derive(Clone, PartialEq, Eq, Hash)]
                pub struct #ident(SyntaxToken);

                impl fmt::Debug for #ident {
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                        fmt::Debug::fmt(&self.0, f)
                    }
                }

                #token_trait_impl

                impl AstElement for #ident {
                    fn can_cast(kind: SyntaxKind) -> bool {
                        kind == SyntaxKind::#ident
                    }

                    fn cast(elem: SyntaxElement) -> Option<Self> {
                        let tok = elem.into_token()?;
                        Self::can_cast(tok.kind()).then(|| Self(tok))
                    }

                    fn span(&self) -> Span {
                        self.0.text_range().into()
                    }
                }
            }
        });

        quote! {
            use std::fmt;
            use crate::syntax::*;

            #(#tokens)*
        }
    }

    fn gen_ast(&mut self) -> TokenStream {
        let nodes = self.gen_nodes();
        let nodes = nodes
			.into_iter()
			.map(|node| {
				match node {
					NodeData::Struct(s) => {
						let name = format_ident!("{}", s.name);
						let fields = s.fields.into_iter().filter_map(|f|
							match f.kind {
                                FieldKind::Node => {
        							let name = format_ident!("{}", f.name);
        							let ty = format_ident!("{}", f.ty);

    								Some(match f.repeat {
    									Repeat::Many => quote! {
    										pub fn #name(&self) -> impl Iterator<Item = #ty> + '_ {
    											children(&self.0)
    										}
    									},
    									Repeat::One(n) => {
                                            let call = if n == 0 {
    											quote!(children(&self.0).next())
                                            } else {
    											quote!(children(&self.0).nth(#n))
                                            };

                                            quote! {
        										pub fn #name(&self) -> Option<#ty> {
                                                    #call
        										}
        									}
                                        },
    								})
    							},
                                FieldKind::Token => {
        							let name = format_ident!("{}", f.name);
        							let ty = format_ident!("{}", f.ty);

    								Some(match f.repeat {
    									Repeat::Many => {
    										quote! {
    											pub fn #name(&self) -> impl Iterator<Item = #ty> + '_ {
    												children(&self.0)
    											}
    										}
    									},
    									Repeat::One(n) => {
                                            let call = if n == 0 {
    											quote!(children(&self.0).next())
                                            } else {
    											quote!(children(&self.0).nth(#n))
                                            };

                                            quote! {
        										pub fn #name(&self) -> Option<#ty> {
                                                    #call
        										}
        									}
    									},
    								})
                                }
                                FieldKind::Hidden => None
							}
						);

						quote! {
							#[derive(Clone, PartialEq, Eq, Hash)]
							pub struct #name(SyntaxNode);

							impl fmt::Debug for #name {
								fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { fmt::Debug::fmt(&self.0, f) }
							}

							impl AstNode for #name {}

							impl AstElement for #name {
								fn can_cast(kind: SyntaxKind) -> bool {
									kind == SyntaxKind::#name
								}

								fn cast(elem: SyntaxElement) -> Option<Self> {
									let node = elem.into_node()?;
									Self::can_cast(node.kind()).then(|| Self(node))
								}

								fn span(&self) -> Span {
									self.0.text_range().into()
								}
							}

							impl #name {
								#(#fields)*
							}
						}
					},
					NodeData::Enum(e) => {
						let name = format_ident!("{}", e.name);
						let token_variants: Vec<_> = e.tokens.iter().map(|variant| format_ident!("{variant}")).collect();

						let node_variants: Vec<_> =
							e.nodes.iter().map(|variant| format_ident!("{}", variant.name)).collect();

						let mut struct_variants = Vec::new();
						let mut enum_variants = Vec::new();

						for variant in e.nodes {
                            let name = format_ident!("{}", variant.name);
							match self.node_types[&variant.node] {
								NodeType::Struct => struct_variants.push(name),
								NodeType::Enum => enum_variants.push(name),
							}
						}

						quote! {
                            #[derive(Clone, Hash, PartialEq, Eq)]
							pub enum #name {
								#(#token_variants(#token_variants),)*
								#(#node_variants(#node_variants),)*
							}

							impl fmt::Debug for #name {
								fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
									match self {
										#(Self::#token_variants(x) => fmt::Debug::fmt(x, f),)*
										#(Self::#node_variants(x) => fmt::Debug::fmt(x, f),)*
									}
								}
							}

							impl AstElement for #name {
								fn can_cast(kind: SyntaxKind) -> bool {
									matches!(kind, #(| SyntaxKind::#token_variants)* #(| SyntaxKind::#struct_variants)*)
									#(|| #enum_variants::can_cast(kind))*
								}

								fn cast(elem: SyntaxElement) -> Option<Self> {
									match elem.kind() {
										#(SyntaxKind::#struct_variants => AstElement::cast(elem.clone()).map(Self::#struct_variants),)*
										#(SyntaxKind::#token_variants => AstElement::cast(elem.clone()).map(Self::#token_variants),)*
										_ => None,
									} #(.or_else(|| AstElement::cast(elem.clone()).map(Self::#enum_variants)))*
								}

								fn span(&self) -> Span {
									match self {
										#(Self::#token_variants(x) => x.span(),)*
										#(Self::#node_variants(x) => x.span(),)*
									}
								}
							}
						}
					},
				}
			});

        quote! {
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

            #(#nodes)*
        }
    }

    fn gen_nodes(&mut self) -> Vec<NodeData> {
        self.grammar
            .iter()
            .map(|n| {
                let node = &self.grammar[n];
                match self.node_types[&n] {
                    NodeType::Struct => {
                        let mut s = Struct {
                            name: node.name.clone(),
                            fields: Vec::new(),
                            type_repeat: HashMap::new(),
                        };

                        self.lower_rule(&mut s, None, &node.rule);
                        NodeData::Struct(s)
                    }
                    NodeType::Enum => {
                        let e = self.lower_enum(node.name.clone(), &node.rule);
                        NodeData::Enum(e)
                    }
                }
            })
            .collect()
    }

    fn lower_rule(&mut self, out: &mut Struct, label: Option<&String>, rule: &Rule) {
        if self.lower_comma_list(out, label, rule) {
            return;
        }

        match rule {
            Rule::Labeled { label, rule } => self.lower_rule(out, Some(label), rule),
            Rule::Node(node) => {
                let ty = self.grammar[*node].name.clone();
                let index = out.get_repeat(&ty, rule);

                out.fields.push(Field {
                    kind: FieldKind::Node,
                    name: label.cloned().unwrap_or_else(|| to_snake_case(&ty)),
                    ty,
                    repeat: Repeat::One(index),
                });
            }
            Rule::Token(token) => {
                let token = &self.grammar[*token].name;
                let ty = token_map(token).expr;
                let index = out.get_repeat(&ty, rule);

                out.fields.push(if let Some(label) = label {
                    Field {
                        kind: FieldKind::Token,
                        name: label.clone(),
                        ty,
                        repeat: Repeat::One(index),
                    }
                } else {
                    Field {
                        kind: FieldKind::Hidden,
                        name: String::new(),
                        ty,
                        repeat: Repeat::One(index),
                    }
                });
            }
            Rule::Seq(rules) => {
                for rule in rules {
                    self.lower_rule(out, label, rule);
                }
            }
            Rule::Alt(_) => panic!("alternation rule is not allowed here: '{rule:?}'",),
            Rule::Opt(rule) => self.lower_rule(out, label, rule),
            Rule::Rep(rule) => {
                if let Rule::Node(node) = rule.as_ref() {
                    let ty = self.grammar[*node].name.clone();
                    out.check_repeat(&ty, rule);

                    out.fields.push(Field {
                        kind: FieldKind::Node,
                        name: label
                            .cloned()
                            .unwrap_or_else(|| pluralize(&to_snake_case(&ty))),
                        ty,
                        repeat: Repeat::Many,
                    });
                }
            }
        }
    }

    fn lower_enum(&mut self, name: String, rule: &Rule) -> Enum {
        let mut nodes = Vec::new();
        let mut tokens = Vec::new();

        let Rule::Alt(alts) = rule else {
            panic!("expected an alternation rule, got '{rule:?}'",)
        };

        for alt in alts {
            match alt {
                Rule::Node(node) => {
                    let data = &self.grammar[*node];
                    nodes.push(Variant {
                        name: data.name.clone(),
                        node: *node,
                    });
                }
                Rule::Token(token) => {
                    let token = token_map(&self.grammar[*token].name).expr;
                    tokens.push(token);
                }
                _ => panic!("expected node or token, got {rule:?}",),
            }
        }

        Enum {
            name,
            nodes,
            tokens,
        }
    }

    // (T (',' T)* ','?)
    fn lower_comma_list(&mut self, out: &mut Struct, label: Option<&String>, r: &Rule) -> bool {
        let Rule::Seq(rule) = r else {
            return false;
        };

        let [Rule::Node(node), Rule::Rep(repeat), Rule::Opt(trailing_comma)] = rule.as_slice()
        else {
            return false;
        };

        let Rule::Seq(repeat) = repeat.as_ref() else {
            return false;
        };

        match repeat.as_slice() {
            [comma, Rule::Node(n)] if comma == trailing_comma.as_ref() && n == node => {}
            _ => return false,
        }

        let ty = self.grammar[*node].name.clone();
        let name = label
            .cloned()
            .unwrap_or_else(|| pluralize(&to_snake_case(&ty)));

        out.check_repeat(&ty, r);
        out.fields.push(Field {
            kind: FieldKind::Node,
            name,
            ty,
            repeat: Repeat::Many,
        });

        true
    }
}

fn to_snake_case(x: &str) -> String {
    const RUST_KEYWORDS: &[&str] = &[
        "abstract", "alignof", "as", "become", "box", "break", "const", "continue", "crate", "do",
        "else", "enum", "extern", "false", "final", "fn", "for", "if", "impl", "in", "let", "loop",
        "macro", "match", "mod", "move", "mut", "offsetof", "override", "priv", "proc", "pub",
        "pure", "ref", "return", "Self", "self", "sizeof", "static", "struct", "super", "trait",
        "true", "type", "typeof", "unsafe", "unsized", "use", "virtual", "where", "while", "yield",
    ];

    let mut s = String::with_capacity(x.len());
    let mut last = '_';
    for c in x.chars() {
        if c.is_ascii_uppercase() {
            if last != '_' {
                s.push('_');
            }
            s.push(c.to_ascii_lowercase());
        } else {
            s.push(c);
        }
        last = c;
    }

    if RUST_KEYWORDS.contains(&s.as_str()) {
        s.push('_');
    }
    s
}

fn pluralize(x: &str) -> String {
    let mut s = String::with_capacity(x.len() + 1);
    s.push_str(x);
    if s.ends_with('_') {
        s.pop();
    }
    s.push('s');
    s
}

fn token_map(token: &str) -> TokenData {
    macro_rules! T {
        ($fmt:expr, $expr:expr) => {
            TokenData {
                fmt: $fmt.to_owned(),
                expr: $expr.to_owned(),
                lex: $fmt.to_owned(),
                regex: false,
            }
        };
        ($fmt:expr, $expr:expr, regex:$lex:expr) => {
            TokenData {
                fmt: $fmt.to_owned(),
                expr: $expr.to_owned(),
                lex: $lex.to_owned(),
                regex: true,
            }
        };
        ($fmt:expr, $expr:expr, $lex:expr) => {
            TokenData {
                fmt: $fmt.to_owned(),
                expr: $expr.to_owned(),
                lex: $lex.to_owned(),
                regex: false,
            }
        };
    }

    match token {
        // Delimiters
        "(" => T!("(", "LeftParen"),
        ")" => T!(")", "RightParen"),
        "[" => T!("[", "LeftBracket"),
        "]" => T!("]", "RightBracket"),
        "{" => T!("{", "LeftBrace"),
        "}" => T!("}", "RightBrace"),
        // Operators
        "+" => T!("+", "Plus"),
        "-" => T!("-", "Minus"),
        "*" => T!("*", "Star"),
        "/" => T!("/", "Slash"),
        "%" => T!("%", "Percent"),
        "==" => T!("==", "Eq"),
        "!=" => T!("!=", "Neq"),
        "<" => T!("<", "Lt"),
        "<=" => T!("<=", "Le"),
        ">" => T!(">", "Gt"),
        ">=" => T!(">=", "Ge"),
        "&&" => T!("&&", "And"),
        "||" => T!("||", "Or"),
        "=" => T!("=", "Equals"),
        "+=" => T!("+=", "PlusEquals"),
        "-=" => T!("-=", "MinusEquals"),
        "*=" => T!("*=", "StarEquals"),
        "/=" => T!("/=", "SlashEquals"),
        "%=" => T!("%=", "PercentEquals"),
        "!" => T!("!", "Not"),
        "&" => T!("&", "Ampersand"),
        // Punctuation
        ":" => T!(":", "Colon"),
        ";" => T!(";", "Semicolon"),
        "." => T!(".", "Dot"),
        "," => T!(",", "Comma"),
        "->" => T!("->", "Arrow"),
        // Keywords
        "struct" => T!("struct", "Struct"),
        "enum" => T!("enum", "Enum"),
        "fn" => T!("fn", "Fn"),
        "let" => T!("let", "Let"),
        "if" => T!("if", "If"),
        "else" => T!("else", "Else"),
        "while" => T!("while", "While"),
        "for" => T!("for", "For"),
        "in" => T!("in", "In"),
        "break" => T!("break", "Break"),
        "continue" => T!("continue", "Continue"),
        "return" => T!("return", "Return"),
        // Literals
        "true" => T!("true", "True"),
        "false" => T!("false", "False"),
        "int" => T!("<number>", "Int", regex:r"\d+"),
        "float" => T!("<float>", "Float", regex:r"\d+\.\d+"),
        "char" => T!("<char>", "Char", regex:r"b?'([^']|\\[\\nrt0'])'"),
        "string" => T!("<string>", "String", regex:r#"r?b?"([^"]|\\[\\nrt0"])*""#),
        "ident" => T!("<ident>", "Ident", regex:r"[a-zA-Z_][a-zA-Z_0-9]*"),
        err => panic!("unknown token: '{err}' (try adding '{err}' to map_token())"),
    }
}
