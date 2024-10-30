use crate::lexer::{Lexer, Token, TokenKind};
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

impl fmt::Display for TokenTree<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenTree::Atom(i) => write!(f, "{}", i),
            TokenTree::Cons(head, rest) => {
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
    String(Cow<'de, str>),
    Number(f64),
    Nil,
    Bool(bool),
    Ident(&'de str),
    Super,
    This,
}

impl fmt::Display for Atom<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Atom::String(s) => write!(f, "{s}"),
            Atom::Number(n) => {
                if *n == n.trunc() {
                    write!(f, "{n}.0")
                } else {
                    write!(f, "{n}")
                }
            }
            Atom::Nil => write!(f, "nil"),
            Atom::Bool(b) => write!(f, "{b:?}"),
            Atom::Ident(id) => write!(f, "{id}"),
            Atom::Super => write!(f, "super"),
            Atom::This => write!(f, "this"),
        }
    }
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
    Bang,

    // Non
    Field,

    /// Keywords operators
    And,
    Or,
    For,
    While,
    Class, // Operand first is a block
    Print,
    Return,
    Var,

    //
    Call,
    Group,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::Minus => "-",
                Op::Plus => "+",
                Op::Star => "*",
                Op::NotEqual => "!=",
                Op::EqualEqual => "==",
                Op::Leq => "<=",
                Op::Geq => ">=",
                Op::Less => "<",
                Op::Greater => ">",
                Op::Slash => "/",
                Op::Bang => "!",
                Op::And => "and",
                Op::Or => "or",
                Op::For => "for",
                Op::Class => "class",
                Op::Print => "print",
                Op::Return => "return",
                Op::Field => ".",
                Op::Var => "var",
                Op::While => "while",
                Op::Call => "call",
                Op::Group => "group",
            }
        )
    }
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
    pub fn new(input: &'de str) -> Self {
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
        let looking_for_msg = move || {
            if let Some((op, argi)) = looking_for {
                format!("Looking for #{argi} for {op:?}")
            } else {
                "looking_for a statement".to_string()
            }
        };

        let mut lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => return Ok(TokenTree::Atom(Atom::Nil)),
            Some(Err(e)) => {
                let msg = looking_for_msg();
                return Err(e).wrap_err(msg);
            }
        };

        let mut lhs = match lhs {
            // Atoms
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

            // Groups
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
                match self.lexer.next() {
                    Some(token) if token == terminator => {}
                    Some(token) => {
                        return Err(miette::miette! {
                            labels = vec![
                            LabeledSpan::at(self., label)
                            ],
                        })
                        .wrap_err(looking_for_msg());
                    }
                    None => return Err(Eof).wrap_err(looking_for_msg()),
                }
                assert_eq(self.lexer.next().kind, terminator);
                lhs
            }

            // prefix expressiosn
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
