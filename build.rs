use std::{
    collections::HashMap,
    env, fmt, fs,
    path::{Path, PathBuf},
};

use quote::{format_ident, quote};
use ungrammar::{Grammar, Node, Rule};

const GRAMMAR_PATH: &str = "refl.ungram";
const OUTPUT_PATH: &str = "src/syntax/generated";

fn main() {
    println!("cargo:rerun-if-changed={GRAMMAR_PATH}");
    let grammar: Grammar = fs::read_to_string(GRAMMAR_PATH).unwrap().parse().unwrap();
    let out: PathBuf = format!("{}/{OUTPUT_PATH}", env::var("CARGO_MANIFEST_DIR").unwrap()).into();
    Generator::new(&grammar).generate(&out);
}

enum NodeType {
    Enum,
    Struct,
}

enum NodeData {
    Struct(Struct),
    Enum(Enum),
}

struct Struct {
    name: String,
    fields: Vec<Field>,
    type_cardinality: HashMap<String, Cardinality>,
}

impl Struct {
    fn get_cardinality(&mut self, ty: &String, rule: &Rule, grammar: &Grammar) -> usize {
        match self.type_cardinality.get_mut(ty) {
            Some(Cardinality::One(x)) => {
                *x += 1;
                *x
            }
            Some(Cardinality::Many) => panic!(
                "rule `{}` imports type `{}` which was already used before",
                format_rule(rule, grammar),
                ty
            ),
            None => {
                self.type_cardinality
                    .insert(ty.clone(), Cardinality::One(0));
                0
            }
        }
    }

    fn use_many_cardinality(&mut self, ty: &str, r: &Rule, grammar: &Grammar) {
        if self.type_cardinality.contains_key(ty) {
            panic!(
                "rule `{}` imports type `{}` which was already used before",
                format_rule(r, grammar),
                ty
            );
        }
    }
}

enum Field {
    Token {
        name: String,
        ty: String,
        cardinality: Cardinality,
    },
    Node {
        name: String,
        ty: String,
        cardinality: Cardinality,
    },
}

enum Cardinality {
    One(usize),
    Many,
}

struct Variant {
    name: String,
    node: Node,
}

struct Enum {
    name: String,
    node_variants: Vec<Variant>,
    token_variants: Vec<String>,
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

    fn generate(mut self, out: &Path) {
        write(
            out,
            "kind.rs",
            Some("use cstree::Syntax;\nuse logos::Logos;"),
            self.gen_kinds(),
        );

        write(
            out,
            "token.rs",
            Some("use crate::syntax::*;"),
            self.gen_tokens(),
        );

        write(out, "ast.rs", Some("use crate::syntax::*;"), self.gen_ast());
    }

    fn gen_kinds(&mut self) -> String {
        let token_data: Vec<_> = self
            .grammar
            .tokens()
            .map(|n| map_token(&self.grammar[n].name))
            .collect();

        let tokens: Vec<_> = token_data
            .iter()
            .map(|data| {
                let TokenData {
                    expr, lex, regex, ..
                } = data;

                let ident = format_ident!("{expr}");
                let lex = lex.clone();

                if *regex {
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

        let def = quote! {
            #[repr(u32)]
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Logos, Syntax)]
            pub enum SyntaxKind {
                // Terminal tokens
                #(#tokens,)*

                // Non-terminal nodes
                #(#nodes,)*

                // Misc
                #[regex(r"[ \n\r\t]+")]
                Whitespace,
                #[regex("//.*")]
                Comment,
                Error,
                #[doc(hidden)]
                Eof,
            }
        };

        let token_display: Vec<_> = token_data
            .iter()
            .map(|data| {
                let TokenData { expr, fmt, .. } = data;
                let name = format_ident!("{expr}");

                quote! {
                    SyntaxKind::#name => write!(f, "{}", #fmt)
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

        let display = quote! {
            impl std::fmt::Display for SyntaxKind {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
        };

        format!("{def}\n\n{display}")
    }

    fn gen_tokens(&mut self) -> String {
        self.grammar
            .tokens()
            .map(|node| {
                {
                    let ident = format_ident!("{}", map_token(&self.grammar[node].name).expr);

                    quote! {
                        #[derive(Clone, PartialEq, Eq, Hash)]
                        pub struct #ident(SyntaxToken);

                        impl std::fmt::Debug for #ident {
                            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                                std::fmt::Debug::fmt(&self.0, f)
                            }
                        }

                        impl AstToken for #ident {}

                        impl AstElement for #ident {
                            fn can_cast(kind: SyntaxKind) -> bool {
                                kind == SyntaxKind::#ident
                            }

                            fn cast(elem: SyntaxElement) -> Option<Self> {
                                let tok = elem.into_token()?;
                                Self::can_cast(tok.kind()).then(|| Self(tok))
                            }

                            fn span(&self) -> TextRange {
                                self.0.text_range()
                            }

                            fn inner(self) -> SyntaxElement { self.0.into() }
                        }
                    }
                }
                .to_string()
            })
            .collect::<Vec<_>>()
            .join("\n\n")
    }

    fn gen_ast(&mut self) -> String {
        let nodes = self.gen_nodes();
        nodes
			.into_iter()
			.map(|node| {
				match node {
					NodeData::Struct(s) => {
						let name = format_ident!("{}", s.name);
						let fields = s.fields.into_iter().map(|f| match f {
							Field::Node { name, ty, cardinality } => {
								let name = format_ident!("{}", name);
								let ty = format_ident!("{}", ty);
								match cardinality {
									Cardinality::Many => quote! {
										pub fn #name(&self) -> impl Iterator<Item = #ty> + '_ {
											children(&self.0)
										}
									},
									Cardinality::One(n) => quote! {
										pub fn #name(&self) -> Option<#ty> {
											children(&self.0).nth(#n)
										}
									},
								}
							},
							Field::Token { name, ty, cardinality } => {
								let name = format_ident!("{}", name);
								let ty = format_ident!("{}", ty);
								match cardinality {
									Cardinality::Many => {
										quote! {
											pub fn #name(&self) -> impl Iterator<Item = #ty> + '_ {
												children(&self.0)
											}
										}
									},
									Cardinality::One(n) => {
										quote! {
											pub fn #name(&self) -> Option<#ty> {
												children(&self.0).nth(#n)
											}
										}
									},
								}
							},
						});

						quote! {
							#[derive(Clone, PartialEq, Eq, Hash)]
							pub struct #name(SyntaxNode);

							impl std::fmt::Debug for #name {
								fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { std::fmt::Debug::fmt(&self.0, f) }
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

								fn span(&self) -> TextRange {
									self.0.text_range()
								}

								fn inner(self) -> SyntaxElement { self.0.into() }
							}

							impl #name {
								#(#fields)*
							}
						}
					},
					NodeData::Enum(e) => {
						let name = format_ident!("{}", e.name);
						let token_variants: Vec<_> = e.token_variants.iter().map(|variant| format_ident!("{variant}")).collect();

						let node_variants: Vec<_> =
							e.node_variants.iter().map(|variant| format_ident!("{}", variant.name)).collect();

						let mut struct_variants = Vec::new();
						let mut enum_variants = Vec::new();

						for variant in e.node_variants {
                            let name = format_ident!("{}", variant.name);
							match self.node_types[&variant.node] {
								NodeType::Struct => struct_variants.push(name),
								NodeType::Enum => enum_variants.push(name),
							}
						}

						quote! {
							pub enum #name {
								#(#token_variants(#token_variants),)*
								#(#node_variants(#node_variants),)*
							}

							impl std::fmt::Debug for #name {
								fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
									match self {
										#(Self::#token_variants(x) => std::fmt::Debug::fmt(x, f),)*
										#(Self::#node_variants(x) => std::fmt::Debug::fmt(x, f),)*
									}
								}
							}

							impl AstNode for #name {}

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

								fn span(&self) -> TextRange {
									match self {
										#(Self::#token_variants(x) => x.span(),)*
										#(Self::#node_variants(x) => x.span(),)*
									}
								}

								fn inner(self) -> SyntaxElement {
									match self {
										#(Self::#token_variants(x) => x.inner(),)*
										#(Self::#node_variants(x) => x.inner(),)*
									}
								}
							}
						}
					},
				}
				.to_string()
			})
			.collect::<Vec<_>>().join("\n\n")
    }

    fn gen_nodes(&mut self) -> Vec<NodeData> {
        self.grammar
            .iter()
            .map(|x| {
                let node = &self.grammar[x];
                match self.node_types[&x] {
                    NodeType::Struct => {
                        let mut s = Struct {
                            name: node.name.clone(),
                            fields: Vec::new(),
                            type_cardinality: HashMap::new(),
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
                let index = out.get_cardinality(&ty, rule, self.grammar);

                out.fields.push(Field::Node {
                    name: label.cloned().unwrap_or_else(|| to_snake_case(&ty)),
                    ty,
                    cardinality: Cardinality::One(index),
                });
            }
            Rule::Token(token) => {
                let token = &self.grammar[*token].name;
                let ty = map_token(token).expr;
                let index = out.get_cardinality(&ty, rule, self.grammar);

                out.fields.push(Field::Token {
                    name: label.cloned().unwrap_or_else(|| to_snake_case(&ty)),
                    ty,
                    cardinality: Cardinality::One(index),
                });
            }
            Rule::Seq(rules) => {
                for rule in rules {
                    self.lower_rule(out, label, rule);
                }
            }
            Rule::Alt(_) => panic!(
                "'|' rule is not allowed here: {}",
                format_rule(rule, self.grammar)
            ),
            Rule::Opt(rule) => self.lower_rule(out, label, rule),
            Rule::Rep(rule) => {
                if let Rule::Node(node) = &**rule {
                    let ty = self.grammar[*node].name.clone();
                    out.use_many_cardinality(&ty, rule, self.grammar);

                    out.fields.push(Field::Node {
                        name: label
                            .cloned()
                            .unwrap_or_else(|| pluralize(&to_snake_case(&ty))),
                        ty,
                        cardinality: Cardinality::Many,
                    });
                }
            }
        }
    }

    fn lower_enum(&mut self, name: String, rule: &Rule) -> Enum {
        let mut node_variants = Vec::new();
        let mut token_variants = Vec::new();

        let Rule::Alt(alts) = rule
        else { panic!("expected a '|' rule, got {}", format_rule(rule, self.grammar)) };

        for alt in alts {
            match alt {
                Rule::Node(node) => {
                    let data = &self.grammar[*node];
                    node_variants.push(Variant {
                        name: data.name.clone(),
                        node: *node,
                    });
                }
                Rule::Token(token) => {
                    let token = map_token(&self.grammar[*token].name).expr;
                    token_variants.push(token);
                }
                _ => panic!(
                    "expected node or token, got {}",
                    format_rule(rule, self.grammar)
                ),
            }
        }

        Enum {
            name,
            node_variants,
            token_variants,
        }
    }

    // (T (',' T)* ','?)
    fn lower_comma_list(&mut self, out: &mut Struct, label: Option<&String>, r: &Rule) -> bool {
        let Rule::Seq(rule) = r else { return false; };

        let [Rule::Node(node), Rule::Rep(repeat), Rule::Opt(trailing_comma)] = rule.as_slice() else { return false; };

        let Rule::Seq(repeat) = repeat.as_ref() else { return false; };

        match repeat.as_slice() {
            [comma, Rule::Node(n)] if comma == trailing_comma.as_ref() && n == node => {}
            _ => return false,
        }

        let ty = self.grammar[*node].name.clone();
        let name = label
            .cloned()
            .unwrap_or_else(|| pluralize(&to_snake_case(&ty)));

        out.use_many_cardinality(&ty, r, self.grammar);
        out.fields.push(Field::Node {
            name,
            ty,
            cardinality: Cardinality::Many,
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

fn write(out: &Path, file: &str, imports: Option<&str>, content: String) {
    let mut text = String::from("#![allow(clippy::all)]\n\n");

    if let Some(imports) = imports {
        text.push_str(imports);
        text.push_str("\n\n");
    }

    text.push_str(&content);

    let tree = syn::parse_file(&text).unwrap();
    let text = prettyplease::unparse(&tree);

    fs::write(out.join(file), text).unwrap();
}

fn format_rule<'a>(rule: &'a Rule, grammar: &'a Grammar) -> impl fmt::Display + 'a {
    struct Fmt<'a> {
        rule: &'a Rule,
        grammar: &'a Grammar,
    }

    impl fmt::Display for Fmt<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self.rule {
                Rule::Labeled { label, rule } => {
                    write!(f, "{}:{}", label, format_rule(rule, self.grammar))
                }
                Rule::Node(node) => write!(f, "{}", self.grammar[*node].name),
                Rule::Token(tok) => write!(f, "'{}'", self.grammar[*tok].name),
                Rule::Seq(seq) => {
                    write!(f, "(")?;
                    for rule in seq {
                        write!(f, "{} ", format_rule(rule, self.grammar))?;
                    }
                    write!(f, ")")
                }
                Rule::Alt(opts) => {
                    write!(f, "(")?;
                    for rule in opts {
                        write!(f, "{} | ", format_rule(rule, self.grammar))?;
                    }
                    write!(f, ")")
                }
                Rule::Opt(v) => {
                    write!(f, "{}?", format_rule(v, self.grammar))
                }
                Rule::Rep(v) => {
                    write!(f, "{}*", format_rule(v, self.grammar))
                }
            }
        }
    }

    Fmt { rule, grammar }
}

fn map_token(token: &str) -> TokenData {
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
        // Primitive types
        "int_type" => T!("int", "IntType"),
        "float_type" => T!("float", "FloatType"),
        "bool_type" => T!("bool", "BoolType"),
        "char_type" => T!("char", "CharType"),
        "string_type" => T!("string", "StringType"),
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
