use crate::frontend::{ast::BinaryOp, syntax::symbol::Symbol};

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct Token {
  pub data: u64,
  pub pos: u32,
  pub len: u32,
  pub line: u32,
  pub kind_tag: u16
}

impl Token {
  #[inline(always)]
  pub fn new(kind: TokenKind, line: u32, start: u32, len: u32) -> Self {
    let (data, kind_tag) = kind.pack();

    Token {
      data,
      pos: start,
      line,
      len: len,
      kind_tag
    }
  }

  #[inline(always)]
  pub fn kind(&self) -> TokenKind {
    TokenKind::unpack(self.data, self.kind_tag)
  }

  #[inline(always)]
  pub fn start(&self) -> u32 {
    self.pos
  }

  #[inline(always)]
  pub fn line(&self) -> u32 {
    self.line
  }

  #[inline(always)]
  pub fn is_eof(&self) -> bool {
    self.kind_tag == 0
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
  Eof,
  Identifier(Symbol),

  If,
  Let,
  Fun,
  For,
  Match,
  Break,
  Return,
  Import,
  Struct,
  Enum,
  Continue,
  Else,
  In,
  Pub,
  Mut,
  Typeof,
  As,

  Literal(LiteralKind),

  Comma,
  Dot,
  DotDot,
  RightParen,
  LeftParen,
  RightBrace,
  LeftBrace,
  RightBracket,
  LeftBracket,
  Question,
  Colon,
  Equal,
  Bang,
  LessThan,
  GreaterThan,
  Minus,
  And,
  Or,
  Plus,
  Star,
  Slash,
  Caret,
  Percent,
  Arrow,
  EqualEqual,
  NotEqual,
  LessEqual,
  GreaterEqual
}

impl TokenKind {
  #[inline(always)]
  fn pack(&self) -> (u64, u16) {
    match self {
      TokenKind::Eof => (0, 0),
      TokenKind::Identifier(sym) => (sym.0 as u64, 1),

      TokenKind::If => (0, 38),
      TokenKind::Let => (0, 39),
      TokenKind::Fun => (0, 40),
      TokenKind::For => (0, 41),
      TokenKind::Match => (0, 42),
      TokenKind::Break => (0, 43),
      TokenKind::Return => (0, 44),
      TokenKind::Import => (0, 45),
      TokenKind::Struct => (0, 46),
      TokenKind::Enum => (0, 47),
      TokenKind::Continue => (0, 48),
      TokenKind::Else => (0, 49),
      TokenKind::In => (0, 50),
      TokenKind::Pub => (0, 51),
      TokenKind::Mut => (0, 52),
      TokenKind::Typeof => (0, 53),
      TokenKind::As => (0, 54),

      TokenKind::Literal(lit) => match lit {
        LiteralKind::Integer => (0, 3),
        LiteralKind::Float => (0, 4),
        LiteralKind::String => (0, 5),
        LiteralKind::Bool(b) => (*b as u64, 6),
        LiteralKind::Nil => (0, 7),
        LiteralKind::Void => (0, 8)
      },

      TokenKind::Comma => (0, 10),
      TokenKind::Dot => (0, 11),
      TokenKind::RightParen => (0, 12),
      TokenKind::LeftParen => (0, 13),
      TokenKind::RightBrace => (0, 14),
      TokenKind::LeftBrace => (0, 15),
      TokenKind::RightBracket => (0, 16),
      TokenKind::LeftBracket => (0, 17),
      TokenKind::Question => (0, 18),
      TokenKind::Colon => (0, 19),
      TokenKind::Equal => (0, 20),
      TokenKind::Bang => (0, 21),
      TokenKind::LessThan => (0, 22),
      TokenKind::GreaterThan => (0, 23),
      TokenKind::Minus => (0, 24),
      TokenKind::And => (0, 25),
      TokenKind::Or => (0, 26),
      TokenKind::Plus => (0, 27),
      TokenKind::Star => (0, 28),
      TokenKind::Slash => (0, 29),
      TokenKind::Caret => (0, 30),
      TokenKind::Percent => (0, 31),
      TokenKind::Arrow => (0, 32),
      TokenKind::EqualEqual => (0, 33),
      TokenKind::NotEqual => (0, 34),
      TokenKind::LessEqual => (0, 35),
      TokenKind::GreaterEqual => (0, 36),
      TokenKind::DotDot => (0, 37)
    }
  }

  #[inline(always)]
  fn unpack(data: u64, kind_tag: u16) -> TokenKind {
    match kind_tag {
      0 => TokenKind::Eof,
      1 => TokenKind::Identifier(Symbol(data as u32)),

      38 => TokenKind::If,
      39 => TokenKind::Let,
      40 => TokenKind::Fun,
      41 => TokenKind::For,
      42 => TokenKind::Match,
      43 => TokenKind::Break,
      44 => TokenKind::Return,
      45 => TokenKind::Import,
      46 => TokenKind::Struct,
      47 => TokenKind::Enum,
      48 => TokenKind::Continue,
      49 => TokenKind::Else,
      50 => TokenKind::In,
      51 => TokenKind::Pub,
      52 => TokenKind::Mut,
      53 => TokenKind::Typeof,
      54 => TokenKind::As,

      3 => TokenKind::Literal(LiteralKind::Integer),
      4 => TokenKind::Literal(LiteralKind::Float),
      5 => TokenKind::Literal(LiteralKind::String),
      6 => TokenKind::Literal(LiteralKind::Bool(data != 0)),
      7 => TokenKind::Literal(LiteralKind::Nil),
      8 => TokenKind::Literal(LiteralKind::Void),

      10 => TokenKind::Comma,
      11 => TokenKind::Dot,
      12 => TokenKind::RightParen,
      13 => TokenKind::LeftParen,
      14 => TokenKind::RightBrace,
      15 => TokenKind::LeftBrace,
      16 => TokenKind::RightBracket,
      17 => TokenKind::LeftBracket,
      18 => TokenKind::Question,
      19 => TokenKind::Colon,
      20 => TokenKind::Equal,
      21 => TokenKind::Bang,
      22 => TokenKind::LessThan,
      23 => TokenKind::GreaterThan,
      24 => TokenKind::Minus,
      25 => TokenKind::And,
      26 => TokenKind::Or,
      27 => TokenKind::Plus,
      28 => TokenKind::Star,
      29 => TokenKind::Slash,
      30 => TokenKind::Caret,
      31 => TokenKind::Percent,
      32 => TokenKind::Arrow,
      33 => TokenKind::EqualEqual,
      34 => TokenKind::NotEqual,
      35 => TokenKind::LessEqual,
      36 => TokenKind::GreaterEqual,
      37 => TokenKind::DotDot,
      _ => unreachable!()
    }
  }

  #[inline(always)]
  pub fn eq_variant(&self, other: &TokenKind) -> bool {
    match (self, other) {
      (TokenKind::Eof, TokenKind::Eof) |
      (TokenKind::Identifier(_), TokenKind::Identifier(_)) |
      (TokenKind::If, TokenKind::If) |
      (TokenKind::Let, TokenKind::Let) |
      (TokenKind::Fun, TokenKind::Fun) |
      (TokenKind::For, TokenKind::For) |
      (TokenKind::Match, TokenKind::Match) |
      (TokenKind::Break, TokenKind::Break) |
      (TokenKind::Return, TokenKind::Return) |
      (TokenKind::Import, TokenKind::Import) |
      (TokenKind::Struct, TokenKind::Struct) |
      (TokenKind::Enum, TokenKind::Enum) |
      (TokenKind::Continue, TokenKind::Continue) |
      (TokenKind::Else, TokenKind::Else) |
      (TokenKind::In, TokenKind::In) |
      (TokenKind::Pub, TokenKind::Pub) |
      (TokenKind::Mut, TokenKind::Mut) |
      (TokenKind::Typeof, TokenKind::Typeof) |
      (TokenKind::As, TokenKind::As) |
      (TokenKind::Literal(_), TokenKind::Literal(_)) |
      (TokenKind::Comma, TokenKind::Comma) |
      (TokenKind::Dot, TokenKind::Dot) |
      (TokenKind::DotDot, TokenKind::DotDot) |
      (TokenKind::RightParen, TokenKind::RightParen) |
      (TokenKind::LeftParen, TokenKind::LeftParen) |
      (TokenKind::RightBrace, TokenKind::RightBrace) |
      (TokenKind::LeftBrace, TokenKind::LeftBrace) |
      (TokenKind::RightBracket, TokenKind::RightBracket) |
      (TokenKind::LeftBracket, TokenKind::LeftBracket) |
      (TokenKind::Question, TokenKind::Question) |
      (TokenKind::Colon, TokenKind::Colon) |
      (TokenKind::Equal, TokenKind::Equal) |
      (TokenKind::Bang, TokenKind::Bang) |
      (TokenKind::LessThan, TokenKind::LessThan) |
      (TokenKind::GreaterThan, TokenKind::GreaterThan) |
      (TokenKind::Minus, TokenKind::Minus) |
      (TokenKind::And, TokenKind::And) |
      (TokenKind::Or, TokenKind::Or) |
      (TokenKind::Plus, TokenKind::Plus) |
      (TokenKind::Star, TokenKind::Star) |
      (TokenKind::Slash, TokenKind::Slash) |
      (TokenKind::Caret, TokenKind::Caret) |
      (TokenKind::Percent, TokenKind::Percent) |
      (TokenKind::Arrow, TokenKind::Arrow) |
      (TokenKind::EqualEqual, TokenKind::EqualEqual) |
      (TokenKind::NotEqual, TokenKind::NotEqual) |
      (TokenKind::LessEqual, TokenKind::LessEqual) |
      (TokenKind::GreaterEqual, TokenKind::GreaterEqual) => true,
      _ => false
    }
  }

  #[inline(always)]
  pub fn to_binary_op(&self) -> BinaryOp {
    match self {
      TokenKind::Plus => BinaryOp::Add,
      TokenKind::Minus => BinaryOp::Sub,
      TokenKind::Star => BinaryOp::Mul,
      TokenKind::Slash => BinaryOp::Div,
      TokenKind::Percent => BinaryOp::Mod,
      TokenKind::And => BinaryOp::And,
      TokenKind::Or => BinaryOp::Or,
      TokenKind::EqualEqual => BinaryOp::Eq,
      TokenKind::NotEqual => BinaryOp::Neq,
      TokenKind::LessThan => BinaryOp::Lt,
      TokenKind::LessEqual => BinaryOp::Lte,
      TokenKind::GreaterThan => BinaryOp::Gt,
      TokenKind::GreaterEqual => BinaryOp::Gte,
      _ => unreachable!()
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralKind {
  Integer,
  Float,
  String,
  Bool(bool),
  Void,
  Nil
}
