use miette::{Diagnostic, Error, LabeledSpan, SourceSpan, SpanContents};
use thiserror::Error;

pub struct Lexer<'de> {
    whole: &'de str,
    rest: &'de str,
    byte: usize,
}

#[derive(Diagnostic, Debug, Error)]
#[error("[Lexer] Unexpected Token '{token}' in input")]
pub struct SingleTokenError {
    /// The `Source` that miette will use
    #[source_code]
    src: String,

    pub token: char,

    #[label = "Unrecognized Token"]
    err_span: SourceSpan,
}

impl SingleTokenError {
    pub fn line(&self) -> usize {
        let until_unrecognized = &self.src[..=self.err_span.offset()];
        until_unrecognized.lines().count()
    }
}

impl<'de> Lexer<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            rest: input,
            byte: 0,
        }
    }
}

pub struct Token<'de> {
    kind: TokenKind,
    origin: &'de str,
    offset: usize,
}

enum TokenKind {
    /// Ident
    Ident,

    /// Number
    Number(f64),

    /// Punct
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    NotEqual,
    EqualEqual,
    Equal,
    Bang,
    Star,
    Less,
    Leq,
    Geq,
    Greater,
    Slash,
    Plus,
    Minus,
    Comma,
    Dot,

    ///Keywords
    And,
    Class,
    Else,
    For,
    If,
    Nil,
    Fun,
    True,
    False,
    Or,
    Print,
    Return,
    Super,
    This,
    Var,
    While,
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let origin = self.origin;
        match self.kind {
            TokenKind::LeftBrace => write!(f, " LeftBrace {origin} null"),
            TokenKind::RightBrace => write!(f, " RightBrace {origin} null"),
            TokenKind::LeftParen => write!(f, " LeftParen {origin} null"),
            TokenKind::RightParen => write!(f, " RightParen {origin} null"),
            TokenKind::NotEqual => write!(f, " NotEqual {origin} null"),
            TokenKind::EqualEqual => write!(f, " EqualEqual {origin} null"),
            TokenKind::Equal => write!(f, " Equal {origin} null"),
            TokenKind::Bang => write!(f, " Bang {origin} null"),
            TokenKind::Star => write!(f, " Star {origin} null"),
            TokenKind::Less => write!(f, " Less {origin} null"),
            TokenKind::Leq => write!(f, " Leq {origin} null"),
            TokenKind::Geq => write!(f, " Geq {origin} null"),
            TokenKind::Greater => write!(f, " Greater {origin} null"),
            TokenKind::Slash => write!(f, " Slash {origin} null"),
            TokenKind::Plus => write!(f, " Plus {origin} null"),
            TokenKind::Minus => write!(f, " Minus {origin} null"),
            TokenKind::Comma => write!(f, " Comma {origin} null"),
            TokenKind::Number(a) => write!(f, "Number {origin} {a}"),
            TokenKind::Dot => write!(f, " Dot {origin} null"),
            TokenKind::And => write!(f, " AND {origin} null"),
            TokenKind::Class => write!(f, " CLASS {origin} null"),
            TokenKind::Else => write!(f, " ELSE {origin} null"),
            TokenKind::For => write!(f, " FOR {origin} null"),
            TokenKind::If => write!(f, " IF {origin} null"),
            TokenKind::Nil => write!(f, " NIL {origin} null"),
            TokenKind::Fun => write!(f, " FUN {origin} null"),
            TokenKind::True => write!(f, " TRUE {origin} null"),
            TokenKind::False => write!(f, " FALSE {origin} null"),
            TokenKind::Or => write!(f, " OR {origin} null"),
            TokenKind::Print => write!(f, " PRINT {origin} null"),
            TokenKind::Return => write!(f, " RETURN {origin} null"),
            TokenKind::Super => write!(f, " SUPER {origin} null"),
            TokenKind::This => write!(f, " THIS {origin} null"),
            TokenKind::Var => write!(f, " VAR {origin} null"),
            TokenKind::While => write!(f, " WHILE {origin} null"),
            TokenKind::Ident => write!(f, " Ident {origin} null"),
        }
    }
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let mut chars = self.rest.chars();
            let c = chars.next()?;
            let c_at = self.byte;
            let c_str = &self.rest[..c.len_utf8()];
            let c_onwards = self.rest;
            self.rest = chars.as_str();
            self.byte += c.len_utf8();

            let just = move |kind: TokenKind| -> Option<Result<Token<'de>, Error>> {
                Some(Ok(Token {
                    kind,
                    offset: c_at,
                    origin: c_str,
                }))
            };

            enum Started {
                Slash,
                String,
                Number(char),
                Ident,
                IfEqualElse(TokenKind, TokenKind),
            }

            let started = match c {
                '}' => return just(TokenKind::LeftBrace),
                '{' => return just(TokenKind::RightBrace),
                '(' => return just(TokenKind::LeftParen),
                ')' => return just(TokenKind::RightParen),
                ',' => return just(TokenKind::Comma),
                '*' => return just(TokenKind::Star),
                '.' => return just(TokenKind::Dot),
                '+' => return just(TokenKind::Plus),
                '-' => return just(TokenKind::Minus),
                '/' => Started::Slash,
                '>' => Started::IfEqualElse(TokenKind::Geq, TokenKind::Greater),
                '<' => Started::IfEqualElse(TokenKind::Leq, TokenKind::Less),
                '!' => Started::IfEqualElse(TokenKind::NotEqual, TokenKind::Bang),
                '=' => Started::IfEqualElse(TokenKind::EqualEqual, TokenKind::Equal),
                '0'..='9' => Started::Number(c),
                'a'..='z' | '_' | 'A'..='Z' => Started::Ident,
                '"' => Started::String,
                c if c.is_whitespace() => continue,
                c => {
                    return Some(Err(SingleTokenError {
                        src: self.whole.to_string(),
                        token: c,
                        err_span: SourceSpan::from(self.byte - c.len_utf8()..self.byte),
                    }
                    .into()))
                }
            };

            break match started {
                Started::Number(c) => {
                    let first_non_digit = c_onwards
                        .find(|c| !matches!(c, '.' | '_' | '0'..='9'))
                        .unwrap_or_else(|| c_onwards.len());

                    let mut literal = &c_onwards[..first_non_digit];
                    let mut dotted = literal.splitn(3, '.');
                    if let (Some(one), Some(two), Some(_three)) =
                        (dotted.next(), dotted.next(), dotted.next())
                    {
                        literal = &literal[..one.len() + 1 + two.len()]; //  ####.###.
                    };
                    let extra_byte = literal.len() - c.len_utf8(); // c###.###   don't double count
                    self.byte += extra_byte;
                    self.rest = &self.rest[extra_byte..];

                    let n = match literal.parse() {
                        Ok(n) => n,
                        Err(e) => {
                            return Some(Err(miette::miette! {
                                labels = vec![
                                    LabeledSpan::at(self.byte - literal.len() .. self.byte, "This should be a numeric literal"),
                                ],
                                "[Lexer] {e:?}",
                            }.with_source_code(self.whole.to_string())));
                        }
                    };

                    return Some(Ok(Token {
                        kind: TokenKind::Number(n),
                        origin: literal,
                        offset: c_at,
                    }));
                }
                Started::Ident => {
                    let first_non_ident = c_onwards
                        .find(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'))
                        .unwrap_or_else(|| c_onwards.len());
                    let literal = &c_onwards[..first_non_ident];
                    let extra_bytes = literal.len() - c.len_utf8();
                    self.byte += extra_bytes;
                    self.rest = &self.rest[extra_bytes..];

                    let kind = match literal {
                        "and" => TokenKind::And,
                        "class" => TokenKind::Class,
                        "else" => TokenKind::Else,
                        "for" => TokenKind::For,
                        "if" => TokenKind::If,
                        "nil" => TokenKind::Nil,
                        "fun" => TokenKind::Fun,
                        "true" => TokenKind::True,
                        "false" => TokenKind::False,
                        "or" => TokenKind::Or,
                        "print" => TokenKind::Print,
                        "return" => TokenKind::Return,
                        "super" => TokenKind::Super,
                        "this" => TokenKind::This,
                        "var" => TokenKind::Var,
                        "while" => TokenKind::While,
                        _ => TokenKind::Ident,
                    };
                    return Some(Ok(Token {
                        kind,
                        origin: literal,
                        offset: c_at,
                    }));
                }
                Started::Slash => {
                    if self.rest.starts_with("/") {
                        // comment block
                        let line_end = self.rest.find("\n").unwrap_or_else(|| self.rest.len());
                        self.byte += line_end;
                        self.rest = &self.rest[line_end..];
                        continue;
                    } else {
                        Some(Ok(Token {
                            origin: c_str,
                            offset: c_at,
                            kind: TokenKind::Slash,
                        }))
                    }
                }
                Started::IfEqualElse(yes, no) => {
                    if self.rest.starts_with("=") {
                        let span = &c_onwards[..c.len_utf8() + 1];
                        self.rest = &self.rest[1..];
                        self.byte += 1;
                        Some(Ok(Token {
                            kind: yes,
                            origin: span,
                            offset: c_at,
                        }))
                    } else {
                        Some(Ok(Token {
                            kind: no,
                            origin: c_str,
                            offset: c_at,
                        }))
                    }
                }
                __ => todo!(),
            };
        }
    }
}
