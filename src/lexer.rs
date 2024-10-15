use miette::{Error, LabeledSpan, WrapErr};

pub struct Lexer<'de> {
    whole: &'de str,
    rest: &'de str,
    byte: usize,
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
    //Ident,

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
    Not,
    Slash,
    Plus,
    Minus,
    Comma,
    Dot,
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
            TokenKind::Not => write!(f, " Not {origin} null"),
            TokenKind::Slash => write!(f, " Slash {origin} null"),
            TokenKind::Plus => write!(f, " Plus {origin} null"),
            TokenKind::Minus => write!(f, " Minus {origin} null"),
            TokenKind::Comma => write!(f, " Comma {origin} null"),
            TokenKind::Dot => write!(f, " Dot {origin} null"),
            TokenKind::Number(a) => write!(f, "Number {origin} {a}"),
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
                    return Some(Err(miette::miette! {
                        labels = vec![
                            LabeledSpan::at(c_at + c.len_utf8(), "This Char"),
                        ],
                        "[Lexer] Unexpected Token in input",
                    }
                    .with_source_code(self.whole.to_string())));
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
                    todo!()
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
