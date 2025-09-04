use crate::frontend::syntax::symbol::Symbol;
use crate::frontend::syntax::token::*;
use crate::utils::CHAR_TABLE;

pub struct Lexer<'a> {
  bytes: &'a [u8],
  pos: usize,
  line: u32,

  pub lookahead: Option<Token>
}

impl<'a> Lexer<'a> {
  pub fn new(source: &'a str) -> Self {
    let bytes = source.as_bytes();

    Lexer {
      bytes,
      pos: 0,
      line: 1,
      lookahead: None
    }
  }

  #[inline(always)]
  fn current_byte(&self) -> Option<u8> {
    self.bytes.get(self.pos).copied()
  }

  #[inline(always)]
  fn advance(&mut self) -> Option<u8> {
    let byte = self.current_byte()?;
    self.pos += 1;

    if byte == b'\n' {
      self.line += 1;
    }

    Some(byte)
  }

  pub fn next_token(&mut self) -> Token {
    if let Some(token) = self.lookahead.take() {
      return token;
    }

    self.next_token_impl()
  }

  #[inline]
  fn skip_whitespace_and_comments(&mut self) {
    while let Some(byte) = self.current_byte() {
      match byte {
        b' ' | b'\t' | b'\r' => {
          self.pos += 1;
        }
        b'\n' => {
          self.pos += 1;
          self.line += 1;
        }
        b'#' =>
          while let Some(b) = self.advance() {
            if b == b'\n' {
              break;
            }
          },
        _ => break
      }
    }
  }

  fn next_token_impl(&mut self) -> Token {
    self.skip_whitespace_and_comments();

    let start = self.pos;
    let Some(first_byte) = self.advance() else {
      return Token::new(TokenKind::Eof, self.line, start as u32, 0);
    };

    match CHAR_TABLE[first_byte as usize] {
      1 => return self.read_identifier_or_keyword(start),
      2 => return self.read_number(start, false),
      _ => {}
    }

    match first_byte {
      b'.' =>
        if let Some(b'.') = self.current_byte() {
          self.pos += 1;
          Token::new(TokenKind::DotDot, self.line, start as u32, 2)
        } else if let Some(byte) = self.current_byte() {
          if byte.is_ascii_digit() {
            self.read_number(start, true)
          } else {
            Token::new(TokenKind::Dot, self.line, start as u32, 1)
          }
        } else {
          Token::new(TokenKind::Dot, self.line, start as u32, 1)
        },
      b'"' => self.read_string(start),
      b'=' =>
        if let Some(b'=') = self.current_byte() {
          self.pos += 1;
          Token::new(TokenKind::EqualEqual, self.line, start as u32, 2)
        } else {
          Token::new(TokenKind::Equal, self.line, start as u32, 1)
        },
      b'!' =>
        if let Some(b'=') = self.current_byte() {
          self.pos += 1;
          Token::new(TokenKind::NotEqual, self.line, start as u32, 2)
        } else {
          Token::new(TokenKind::Bang, self.line, start as u32, 1)
        },
      b'<' =>
        if let Some(b'=') = self.current_byte() {
          self.pos += 1;
          Token::new(TokenKind::LessEqual, self.line, start as u32, 2)
        } else {
          Token::new(TokenKind::LessThan, self.line, start as u32, 1)
        },
      b'>' =>
        if let Some(b'=') = self.current_byte() {
          self.pos += 1;
          Token::new(TokenKind::GreaterEqual, self.line, start as u32, 2)
        } else {
          Token::new(TokenKind::GreaterThan, self.line, start as u32, 1)
        },
      b'-' =>
        if let Some(b'>') = self.current_byte() {
          self.pos += 1;
          Token::new(TokenKind::Arrow, self.line, start as u32, 2)
        } else {
          Token::new(TokenKind::Minus, self.line, start as u32, 1)
        },
      b'+' => Token::new(TokenKind::Plus, self.line, start as u32, 1),
      b'*' => Token::new(TokenKind::Star, self.line, start as u32, 1),
      b'/' => Token::new(TokenKind::Slash, self.line, start as u32, 1),
      b'^' => Token::new(TokenKind::Caret, self.line, start as u32, 1),
      b'%' => Token::new(TokenKind::Percent, self.line, start as u32, 1),
      b'|' => Token::new(TokenKind::Or, self.line, start as u32, 1),
      b'&' => Token::new(TokenKind::And, self.line, start as u32, 1),
      b',' => Token::new(TokenKind::Comma, self.line, start as u32, 1),
      b'(' => Token::new(TokenKind::LeftParen, self.line, start as u32, 1),
      b')' => Token::new(TokenKind::RightParen, self.line, start as u32, 1),
      b'{' => Token::new(TokenKind::LeftBrace, self.line, start as u32, 1),
      b'}' => Token::new(TokenKind::RightBrace, self.line, start as u32, 1),
      b'[' => Token::new(TokenKind::LeftBracket, self.line, start as u32, 1),
      b']' => Token::new(TokenKind::RightBracket, self.line, start as u32, 1),
      b'?' => Token::new(TokenKind::Question, self.line, start as u32, 1),
      b':' => Token::new(TokenKind::Colon, self.line, start as u32, 1),
      _ => panic!(
        "Unexpected character '{}' at line {}",
        first_byte as char, self.line
      )
    }
  }

  #[inline]
  fn read_identifier_or_keyword(&mut self, start: usize) -> Token {
    while let Some(byte) = self.current_byte() {
      let char_type = CHAR_TABLE[byte as usize];

      if char_type == 1 || char_type == 2 || byte == b'_' {
        self.pos += 1;
      } else {
        break;
      }
    }

    let text_len = self.pos - start;
    let bytes = &self.bytes[start..self.pos];

    let kind = match text_len {
      2 if bytes == b"if" => TokenKind::If,
      3 => match bytes {
        b"let" => TokenKind::Let,
        b"mut" => TokenKind::Mut,
        b"fun" => TokenKind::Fun,
        b"pub" => TokenKind::Pub,
        b"for" => TokenKind::For,
        b"nil" => TokenKind::Literal(LiteralKind::Nil),
        _ => self.bytes_to_identifier(bytes)
      },
      4 => match bytes {
        b"true" => TokenKind::Literal(LiteralKind::Bool(true)),
        b"enum" => TokenKind::Enum,
        b"else" => TokenKind::Else,
        _ => self.bytes_to_identifier(bytes)
      },
      5 if bytes == b"false" => TokenKind::Literal(LiteralKind::Bool(false)),
      6 => match bytes {
        b"return" => TokenKind::Return,
        b"import" => TokenKind::Import,
        b"typeof" => TokenKind::Typeof,
        _ => self.bytes_to_identifier(bytes)
      },
      _ => match bytes {
        b"match" => TokenKind::Match,
        b"break" => TokenKind::Break,
        b"struct" => TokenKind::Struct,
        b"continue" => TokenKind::Continue,
        b"in" => TokenKind::In,
        b"as" => TokenKind::As,
        _ => self.bytes_to_identifier(bytes)
      }
    };

    Token::new(kind, self.line, start as u32, text_len as u32)
  }

  #[inline]
  fn bytes_to_identifier(&self, bytes: &[u8]) -> TokenKind {
    let s = unsafe { std::str::from_utf8_unchecked(bytes) };
    TokenKind::Identifier(Symbol::intern(s))
  }

  #[inline]
  fn read_number(&mut self, start: usize, mut has_dot: bool) -> Token {
    while let Some(byte) = self.current_byte() {
      if byte.is_ascii_digit() {
        self.pos += 1;
      } else if byte == b'.' && !has_dot {
        if let Some(next_byte) = self.bytes.get(self.pos + 1) {
          if *next_byte == b'.' {
            break;
          }
        }

        has_dot = true;
        self.pos += 1;
      } else {
        break;
      }
    }

    let token_len = (self.pos - start) as u32;
    let kind = if has_dot {
      TokenKind::Literal(LiteralKind::Float)
    } else {
      TokenKind::Literal(LiteralKind::Integer)
    };

    Token::new(kind, self.line, start as u32, token_len)
  }

  #[inline]
  fn read_string(&mut self, start: usize) -> Token {
    let mut escaped = false;

    while let Some(byte) = self.advance() {
      if escaped {
        escaped = false;
        continue;
      }

      match byte {
        b'\\' => escaped = true,
        b'"' => break,
        b'\n' => self.line += 1,
        _ => {}
      }
    }

    let token_len = (self.pos - start - 1) as u32;
    Token::new(
      TokenKind::Literal(LiteralKind::String),
      self.line,
      start as u32,
      token_len
    )
  }
}
