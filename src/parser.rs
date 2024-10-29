use loxy::Lexer::Lexer;
use miette::{Context, Diagnostic, Error, LabeledSpan, SourceSpan};
use std::{borrow::Cow, fmt};
use thiserror::Error;

pub struct Parser<'de> {
    whole: &'de str,
    lexer: Lexer<'de>,
}

pub struct Ast;

#[derive(Diagnostic, Debug, Error)]
#[error("Unexpected EOF")]
pub struct Eof;

impl fmt::Display for S {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            S::Atom(i) => write!(f, "{}", i),
            S::Cons(head, rest) => {
                write!(f, "({}", head)?;
                for s in rest {
                    write!(f, " {}", s)?
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom<'de> {
    String(Cow<&'de str>),
    Number(f64),
    Nil,
    Bool(bool),
    Ident(&'de str),
    Super,
    This,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    Plus,
    Minus,
    Star,
    NotEqual,
    EqualEqual,
    Leq,
    Geq,
    Greater,
    Less,
    Slash,
    Dot,
    Bang,
    Equal,

    // Non
    Field,

    /// Keywords operators
    And,
    Or,
    If,
    For,
    While,
    Class, // Operand first is a block
    Fun,   // Operands  Name, Params Body
    Print,
    Return,
    Var,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Delimiter {
    Paren,
    Brace,
}

#[derive(Debug, Clone, PartialEq)]
enum TokenTree<'de> {
    Atom(Atom<'de>),
    Cons(Op, Vec<TokenTree>),
}

impl<'de> Parser<'de> {
    pub fn new(input: &'de str) {
        Parser {
            whole: input,
            lexer: Lexer::new(input),
        }
    }

    pub fn parse(mut self) -> Result<TokenTree<'de>, Error> {
        self.parse_within(None, 0)
    }

    pub fn parse_within(
        &mut self,
        looking_for: Option<(Op, usize)>,
        min_bp: u8,
    ) -> Result<TokenTree<'de>, Error> {
        let looking_for = || {};

        let mut lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => return Ok(TokenTree::Atom(Atom::Nil)),
            Some(Err(e)) => {
                if let Some((op, argi)) = looking_for {
                    format!("looking for argument #{argi} for {op:?}")
                } else {
                    "looking for a statement".to_string()
                }
            }
        };

        let mut lhs = match lhs {
            /// Atoms
            Token {
                kind: TokenKind::String,
                origin,
            } => TokenTree::Atom(Atom::String(Token::unescape(origin))),
            Token {
                kind: TokenKind::Number(n),
                ..
            } => TokenTree::Atom(Atom::Number(n)),
            Token {
                kind: TokenKind::True,
                ..
            } => TokenTree::Atom(Atom::Bool(true)),
            Token {
                kind: TokenKind::False,
                ..
            } => TokenTree::Atom(Atom::Bool(false)),
            Token {
                kind: TokenKind::Ident,
                origin,
            } => TokenTree::Atom(Atom::Ident(origin)),

            /// Groups
            Token {
                kind: TokenKind::LeftParen | TokenKind::LeftBrace,
                ..
            } => {
                let terminator = match lhs.kind {
                    TokenKind::LeftParen => TokenKind::RightParen,
                    TokenKind::LeftBrace => TokenKind::RightBrace,
                    _ => unreachable!("by otter match arm pattern"),
                };
                let lhs = self.parse_within(looking_for, 0).wrap_err("group")?;
                assert_eq(self.lexer.next().kind, terminator);
                lhs
            }

            /// prefix expressiosn
            Token {
                kind: TokenKind::Bang | TokenKind::Print | TokenKind::Minus,
                ..
            } => {
                let op = match lhs.kind {
                    TokenKind::Bang => Op::Bang,
                    TokenKind::Print => Op::Print,
                    TokenKind::Minus => Op::Minus,
                    _ => unreachable!("by otter match arm pattern"),
                };
                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self
                    .parse_within(Some((op, 0)), r_bp)
                    .wrap_err("parse RHS")?;
                TokenTree::Cons(op, vec![rhs])
            }
            t => panic!("Bad token {t:?}"),
        };
    }
}

fn prefix_binding_power(op: Op) -> ((), u8) {
    match op {
        Op::Bang | Op::Minus => ((), 9),
        _ => panic!("bad op: {op:?}"),
    }
}

fn infix_binding_power(op: Op) -> ((), u8) {
    match op {
        _ => panic!("bad op: {op:?}"),
    }
}

fn postfix_binding_power(op: Op) -> ((), u8) {
    match op {
        _ => panic!("bad op: {op:?}"),
    }
}
