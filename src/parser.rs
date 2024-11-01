use crate::lexer::{Lexer, Token, TokenKind};
use miette::{Context, Error, LabeledSpan, SourceSpan};
use std::{borrow::Cow, fmt};


pub struct Ast;

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
    Fun,
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
                Op::Fun => "Fun",
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
    Cons(Op, Vec<TokenTree<'de>>),
}

pub struct Parser<'de> {
    whole: &'de str,
    lexer: Lexer<'de>,
}

impl<'de> Parser<'de> {
    pub fn new(input: &'de str) -> Self {
        Parser {
            whole: input,
            lexer: Lexer::new(input),
        }
    }

    pub fn parse(mut self) -> Result<TokenTree<'de>, Error> {
        self.parse_within(0)
    }

    pub fn parse_within(
        &mut self,
        min_bp: u8,
    ) -> Result<TokenTree<'de>, Error> {

        let mut lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => return Ok(TokenTree::Atom(Atom::Nil)),
            Some(Err(e)) => {
                return Err(e).wrap_err("Parsing LHS");
            }
        };

        let mut lhs = match lhs {
            // Atoms
            Token {
                kind: TokenKind::String,
                origin,
                ..
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
                kind: TokenKind::Nil,
                ..
            } => TokenTree::Atom(Atom::Nil),
            Token {
                kind: TokenKind::Ident,
                origin,
                ..
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
                let lhs = self
                    .parse_within(0)
                    .wrap_err("in bracketed expression")?;
                self.lexer
                    .expect(terminator, "Unexpected end to bracked expression")
                    .wrap_err("After bracked expression")?;
                lhs
            }
            
            // unary prefix expression
            Token {
                kind: TokenKind::Bang | TokenKind::Print | TokenKind::Minus | TokenKind::Return,
                ..
            } => {
                let op = match lhs.kind {
                    TokenKind::Bang => Op::Bang,
                    TokenKind::Print => Op::Print,
                    TokenKind::Minus => Op::Minus,
                    TokenKind::Return => Op::Return,
                    _ => unreachable!("by otter match arm pattern"),
                };
                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self
                    .parse_within(r_bp)
                    .wrap_err("in RHS")?;
                TokenTree::Cons(op, vec![rhs])
            }
            
            // prefix, two args
            Token {
                kind: TokenKind::Class | TokenKind::Var,
                ..
            } => {
                
                let op = match lhs.kind {
                    TokenKind::Class => Op::Class,
                    TokenKind::Var => Op::Var,
                    _ => unreachable!("by otter match arm pattern"),
                };

                if matches!(lhs.kind, TokenKind::For | TokenKind::While ) {

                }

                let first = self
                    .parse_within(0)
                    .wrap_err_with(|| format!("in first arg of {op:?} expression"))?;

                if lhs.kind == TokenKind::Var {
                    if matches!(first, TokenTree::Atom(Atom::Ident(_))) {
                        todo!()
                    }
                    self.lexer 
                        .expect(TokenKind::Equal, "missing =")
                        .wrap_err("in variable assignment")?;
                }

                let second = self
                    .parse_within(0)
                    .wrap_err_with(|| format!("in second argument of {op:?} expression"))?;

                TokenTree::Cons(op, vec![first, second])
            },

            
            Token {
                kind: TokenKind::For,
                ..
            } => {
                
                let op = match lhs.kind {
                    TokenKind::For => Op::For,
                    _ => unreachable!("by otter match arm pattern"),
                };

                self.lexer
                    .expect(TokenKind::LeftParen, "missing (")
                    .wrap_err("in for loop conditional")?;

                let init = self
                    .parse_within(0)
                    .wrap_err_with(|| format!("in init of for loop"))?;
                
                self.lexer
                    .expect(TokenKind::Semicolon, "missing ;")
                    .wrap_err("in for loop")?;

                let cond = self
                    .parse_within(0)
                    .wrap_err_with(|| format!("in cond of for loop"))?;

                self.lexer
                    .expect(TokenKind::Semicolon, "missing ;")
                    .wrap_err("in for loop")?;

                let inc = self
                    .parse_within(0)
                    .wrap_err_with(|| format!("in inc of for loop"))?;
                
                self.lexer
                    .expect(TokenKind::RightParen, "missing )")
                    .wrap_err("in for loop")?;
                
                self.lexer
                    .expect(TokenKind::LeftBrace, "missing {")
                    .wrap_err("in for loop")?;
                
                let block = self
                    .parse_within(0)
                    .wrap_err_with(|| format!("in block of for loop"))?;
                
                self.lexer
                    .expect(TokenKind::LeftBrace, "missing }")
                    .wrap_err("in for loop")?;

                TokenTree::Cons(Op::For, vec![init, cond, inc, block])
            },
            
            Token {
                kind: TokenKind::While,
                ..
            } => {
                
                let op = match lhs.kind {
                    TokenKind::For => Op::For,
                    _ => unreachable!("by otter match arm pattern"),
                };

                self.lexer
                    .expect(TokenKind::LeftParen, "missing (")
                    .wrap_err("in for loop conditional")?;


                let cond = self
                    .parse_within(0)
                    .wrap_err_with(|| format!("in cond of for loop"))?;

                self.lexer
                    .expect(TokenKind::Semicolon, "missing ;")
                    .wrap_err("in for loop")?;

                let inc = self
                    .parse_within(0)
                    .wrap_err_with(|| format!("in inc of for loop"))?;
                
                self.lexer
                    .expect(TokenKind::RightParen, "missing )")
                    .wrap_err("in for loop")?;
                
                self.lexer
                    .expect(TokenKind::LeftBrace, "missing {")
                    .wrap_err("in for loop")?;
                
                let block = self
                    .parse_within(0)
                    .wrap_err_with(|| format!("in block of for loop"))?;
                
                self.lexer
                    .expect(TokenKind::LeftBrace, "missing }")
                    .wrap_err("in for loop")?;

                TokenTree::Cons(Op::For, vec![init, cond, inc, block])
            },
            
            // prefix three arguments
            Token {
                kind:  TokenKind::Fun,
                ..
            } => {
                
                let op = match lhs.kind {
                    TokenKind::Var => Op::Fun,
                    _ => unreachable!("by otter match arm pattern"),
                };
                let first = self
                    .parse_within(0)
                    .wrap_err_with(|| format!("in first argument of {op:?} expression"))?;
                let second = self
                    .parse_within(0)
                    .wrap_err_with(|| format!("in second argument of {op:?} expression"))?;
                let third = self
                    .parse_within(0)
                    .wrap_err_with(|| format!("in third argument of {op:?} expression"))?;

                TokenTree::Cons(op, vec![first, second, third])
            },

            t => panic!("Bad token {t:?}"),
        };

        loop {
            let op = self.lexer.peek();
            if op.map_or(false, |op| op.is_err()) {
                return Err(self
                    .lexer
                    .next()
                    .expect("Checked Some Above")
                    .expect_err("Checked error above"))
                .wrap_err_with(looking_for_msg);
            }
            let op = match op.map(|res| res.expect("handled err above")) {
                None => break,
                Some(Token {
                    kind:
                        TokenKind::LeftParen
                        | TokenKind::Dot
                        | TokenKind::Minus
                        | TokenKind::Plus
                        | TokenKind::Star
                        | TokenKind::NotEqual
                        | TokenKind::EqualEqual
                        | TokenKind::Equal
                        | TokenKind::Leq
                        | TokenKind::Geq
                        | TokenKind::Greater
                        | TokenKind::Less
                        | TokenKind::Slash
                        | TokenKind::And
                        | TokenKind::Or,
                    ..
                }) => 
            };
        }
    }
}

fn prefix_binding_power(op: Op) -> ((), u8) {
    match op {
        Op::Bang | Op::Minus | Op::Print | Op::Return => ((), 9),
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
