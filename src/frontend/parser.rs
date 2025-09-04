use crate::frontend::ast::*;
use crate::frontend::syntax::symbol::Symbol;
use crate::frontend::syntax::token::*;

pub struct Parser<'a> {
  tokens: &'a [Token],
  source: &'a str,
  pos: usize
}

impl<'a> Parser<'a> {
  pub fn new(tokens: &'a [Token], source: &'a str) -> Self {
    Parser {
      tokens,
      source,
      pos: 0
    }
  }

  #[inline(always)]
  fn current(&self) -> &Token {
    unsafe { self.tokens.get_unchecked(self.pos) }
  }

  #[inline(always)]
  fn advance(&mut self) -> &Token {
    let token = unsafe { self.tokens.get_unchecked(self.pos) };
    self.pos += 1;

    token
  }

  #[inline(always)]
  fn expect(&mut self, expected: TokenKind) -> &Token {
    let token = self.advance();

    if !token.kind().eq_variant(&expected) {
      panic!(
        "Expected {:?}, found {:?} at line {}",
        expected,
        token.kind(),
        token.line()
      );
    }

    token
  }

  #[inline(always)]
  fn check(&self, kind: &TokenKind) -> bool {
    if self.pos >= self.tokens.len() {
      return false;
    }

    self.current().kind().eq_variant(kind)
  }

  #[inline(always)]
  fn match_token(&mut self, kind: &TokenKind) -> bool {
    if self.check(kind) {
      self.pos += 1;
      true
    } else {
      false
    }
  }

  pub fn parse_program(&mut self) -> Program {
    let mut body = Vec::with_capacity(128);

    while !self.check(&TokenKind::Eof) {
      body.push(self.parse_statement());
    }

    Program {
      body
    }
  }

  fn parse_statement(&mut self) -> Statement {
    let public = self.consume_flag("pub");

    if let TokenKind::Identifier(_sym) = self.current().kind() {
      if self.pos + 1 < self.tokens.len() {
        if self.tokens[self.pos + 1].kind() == TokenKind::Bang {
          return self.parse_macro();
        }
      }
    }

    match self.current().kind() {
      TokenKind::Import => return self.parse_import(public),
      TokenKind::Struct => return self.parse_struct(public),
      TokenKind::Enum => return self.parse_enum(public),
      TokenKind::Let => return self.parse_let(public),
      TokenKind::Fun => return self.parse_fun(public),
      TokenKind::If => return self.parse_if(),
      TokenKind::For => return self.parse_for(),
      TokenKind::Match => return self.parse_match(),
      TokenKind::Return => return self.parse_return(),
      TokenKind::Break => {
        self.advance();
        return Statement::Break;
      }
      TokenKind::Continue => {
        self.advance();
        return Statement::Continue;
      }
      _ => {}
    }

    let expr = self.parse_expression();
    if self.match_token(&TokenKind::Equal) {
      let right = self.parse_expression();
      Statement::Assign {
        left: Box::new(expr),
        right: Box::new(right)
      }
    } else {
      Statement::Expr(expr)
    }
  }

  fn parse_expression(&mut self) -> Expression {
    self.parse_expr_bp(0)
  }

  fn parse_expr_bp(&mut self, min_bp: u8) -> Expression {
    let mut lhs = self.parse_primary();

    loop {
      let current_kind = &self.current().kind();

      match current_kind {
        TokenKind::Plus |
        TokenKind::Minus |
        TokenKind::Star |
        TokenKind::Slash |
        TokenKind::LessEqual |
        TokenKind::GreaterThan |
        TokenKind::GreaterEqual |
        TokenKind::Percent |
        TokenKind::Caret |
        TokenKind::And |
        TokenKind::Or |
        TokenKind::EqualEqual |
        TokenKind::NotEqual |
        TokenKind::LessThan => {
          let (l_bp, r_bp) = self.get_infix_bp(current_kind);
          if l_bp < min_bp {
            break;
          }

          let op = self.advance().kind().to_binary_op();
          let rhs = self.parse_expr_bp(r_bp);

          lhs = Expression::Binary {
            left: Box::new(lhs),
            op,
            right: Box::new(rhs)
          };
        }
        TokenKind::DotDot => {
          let (l_bp, r_bp) = (0, 1);
          if l_bp < min_bp {
            break;
          }

          self.advance();
          let rhs = self.parse_expr_bp(r_bp);

          lhs = Expression::Range {
            start: Box::new(lhs),
            end: Box::new(rhs)
          };
        }
        TokenKind::LeftParen => {
          self.advance();
          let mut args = Vec::with_capacity(4);

          if !self.check(&TokenKind::RightParen) {
            loop {
              args.push(self.parse_expression());
              if !self.match_token(&TokenKind::Comma) {
                break;
              }
            }
          }

          self.expect(TokenKind::RightParen);
          lhs = Expression::Call {
            callee: Box::new(lhs),
            args,
            generic_args: vec![]
          };
        }
        TokenKind::Dot => {
          self.advance();
          let method = self.expect_identifier();

          if self.check(&TokenKind::LeftParen) {
            self.advance();
            let mut args = Vec::with_capacity(4);

            if !self.check(&TokenKind::RightParen) {
              loop {
                args.push(self.parse_expression());
                if !self.match_token(&TokenKind::Comma) {
                  break;
                }
              }
            }

            self.expect(TokenKind::RightParen);

            lhs = Expression::Call {
              callee: Box::new(Expression::Member {
                object: Box::new(lhs),
                property: method
              }),
              args,
              generic_args: vec![]
            };
          } else {
            lhs = Expression::Member {
              object: Box::new(lhs),
              property: method
            };
          }
        }
        TokenKind::Question => {
          if 1 < min_bp {
            break;
          }
          self.advance();

          let then_branch = self.parse_expr_bp(0);
          self.expect(TokenKind::Colon);
          let else_branch = self.parse_expr_bp(0);

          lhs = Expression::Ternary {
            condition: Box::new(lhs),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch)
          };
        }
        TokenKind::As => {
          self.advance();
          let ty = self.parse_type();

          lhs = Expression::Cast {
            expr: Box::new(lhs),
            ty
          };
        }
        _ => break
      }
    }

    lhs
  }

  #[inline]
  fn parse_primary(&mut self) -> Expression {
    if self.check(&TokenKind::Typeof) {
      self.advance();

      let expr = self.parse_primary();
      return Expression::TypeOf(Box::new(expr));
    }

    if matches!(self.current().kind(), TokenKind::LeftBrace) {
      self.advance();

      let next_kind = if self.pos < self.tokens.len() {
        Some(self.tokens[self.pos].kind())
      } else {
        None
      };

      if matches!(next_kind, Some(TokenKind::Identifier(_))) {
        let mut fields = Vec::new();

        loop {
          let key = self.expect_identifier();
          self.expect(TokenKind::Colon);

          let value = self.parse_expression();
          fields.push((key, value));

          if !self.match_token(&TokenKind::Comma) {
            break;
          }
        }

        self.expect(TokenKind::RightBrace);
        return Expression::Object(fields);
      } else {
        panic!(
          "Unexpected '{{' in expression at line {}",
          self.current().line()
        );
      }
    }

    let tok = self.advance();

    match &tok.kind() {
      TokenKind::Literal(lit) => match lit {
        LiteralKind::Integer => {
          let start = tok.start() as usize;
          let end = start + tok.len as usize;
          let bytes = unsafe { self.source.as_bytes().get_unchecked(start..end) };

          let mut value = 0i64;
          for &byte in bytes {
            value = value * 10 + (byte - b'0') as i64;
          }

          Expression::Literal(LiteralValue::Integer(value))
        }
        LiteralKind::Float => {
          let start = tok.start() as usize;
          let end = start + tok.len as usize;
          let s = unsafe { self.source.get_unchecked(start..end) };
          let value = fast_float::parse(s).unwrap();

          Expression::Literal(LiteralValue::Float(value))
        }
        LiteralKind::Bool(b) => Expression::Literal(LiteralValue::Bool(*b)),
        LiteralKind::Nil => Expression::Literal(LiteralValue::Nil),
        LiteralKind::Void => Expression::Literal(LiteralValue::Void),
        LiteralKind::String => {
          let start = tok.start() as usize + 1;
          let end = start + tok.len as usize - 1;
          let s = unsafe { self.source.get_unchecked(start..end) };

          Expression::Literal(LiteralValue::String(Symbol::intern(s)))
        }
      },
      TokenKind::Identifier(sym) => Expression::Identifier(*sym),
      TokenKind::LeftParen => {
        let expr = self.parse_expression();
        self.expect(TokenKind::RightParen);

        expr
      }
      TokenKind::LeftBracket => {
        let mut elements = Vec::new();

        if !self.check(&TokenKind::RightBracket) {
          loop {
            elements.push(self.parse_expression());

            if !self.match_token(&TokenKind::Comma) {
              break;
            }
          }
        }

        self.expect(TokenKind::RightBracket);
        Expression::Array(elements)
      }
      TokenKind::Minus => {
        let rhs = self.parse_expr_bp(13);

        Expression::Unary {
          op: UnaryOp::Neg,
          expr: Box::new(rhs)
        }
      }
      TokenKind::Bang => {
        let rhs = self.parse_expr_bp(13);

        Expression::Unary {
          op: UnaryOp::Not,
          expr: Box::new(rhs)
        }
      }
      _ => panic!(
        "Unexpected token {:?} in expression at line {}",
        tok.kind(),
        tok.line()
      )
    }
  }

  #[inline(always)]
  fn get_infix_bp(&mut self, op: &TokenKind) -> (u8, u8) {
    match op {
      TokenKind::Or => (1, 2),
      TokenKind::And => (3, 4),
      TokenKind::EqualEqual | TokenKind::NotEqual => (5, 6),
      TokenKind::LessThan |
      TokenKind::LessEqual |
      TokenKind::GreaterThan |
      TokenKind::GreaterEqual => (7, 8),
      TokenKind::Plus | TokenKind::Minus => (9, 10),
      TokenKind::Star | TokenKind::Slash | TokenKind::Percent => (11, 12),
      TokenKind::Caret => (13, 14),
      _ => (0, 0)
    }
  }

  fn parse_let(&mut self, public: bool) -> Statement {
    self.expect(TokenKind::Let);

    let mutable = self.consume_flag("mut");
    let name = self.expect_identifier();

    let ty = if self.current().kind() == TokenKind::Colon {
      self.advance();
      self.parse_type()
    } else {
      panic!("Variable '{}' must declare a type", name.0);
    };

    let init = if self.current().kind() == TokenKind::Equal {
      self.advance();
      Some(self.parse_expression())
    } else {
      panic!("Variable '{}' must have an initializer", name.0);
    };

    Statement::Let {
      name,
      public,
      mutable,
      ty,
      init
    }
  }

  fn parse_fun(&mut self, public: bool) -> Statement {
    self.expect(TokenKind::Fun);

    let name = self.expect_identifier();
    let generic_params = if self.check(&TokenKind::LessThan) {
      self.parse_generic_type()
    } else {
      vec![]
    };

    self.expect(TokenKind::LeftParen);
    let mut params = Vec::new();

    if !self.check(&TokenKind::RightParen) {
      loop {
        let mutable = self.consume_flag("mut");
        let param_name = self.expect_identifier();
        self.expect(TokenKind::Colon);

        let param_type = self.parse_type();
        params.push(Param {
          name: param_name,
          ty: param_type,
          mutable
        });

        if !self.match_token(&TokenKind::Comma) {
          break;
        }
      }
    }

    self.expect(TokenKind::RightParen);
    let ret_type = if self.match_token(&TokenKind::Arrow) {
      self.parse_type()
    } else {
      panic!("Function '{}' must declare a return type", name.0);
    };

    let body = self.parse_block();
    Statement::Fun {
      name,
      public,
      params,
      ret_type,
      generic_params,
      body
    }
  }

  fn parse_macro(&mut self) -> Statement {
    let name = if let TokenKind::Identifier(sym) = self.advance().kind() {
      sym
    } else {
      panic!("Expected macro name")
    };

    self.expect(TokenKind::Bang);
    self.expect(TokenKind::LeftParen);

    let mut args = Vec::new();
    if !self.check(&TokenKind::RightParen) {
      loop {
        args.push(self.parse_expression());
        if !self.match_token(&TokenKind::Comma) {
          break;
        }
      }
    }

    self.expect(TokenKind::RightParen);
    Statement::Macro {
      name,
      args
    }
  }

  fn parse_return(&mut self) -> Statement {
    self.expect(TokenKind::Return);

    if self.check(&TokenKind::RightBrace) {
      panic!(
        "Return statement must return a value at line {}",
        self.current().line()
      );
    }

    let expression = self.parse_expression();
    Statement::Return(expression)
  }

  fn parse_import(&mut self, public: bool) -> Statement {
    self.expect(TokenKind::Import);

    let tok = self.advance();
    let path = match tok.kind() {
      TokenKind::Literal(LiteralKind::String) => {
        let start = tok.start() as usize + 1;
        let end = start + tok.len as usize - 1;
        let s = unsafe { self.source.get_unchecked(start..end) };

        Symbol::intern(s)
      }
      _ => panic!(
        "Expected string literal for module name at line {}",
        tok.line()
      )
    };

    Statement::Import {
      path,
      public
    }
  }

  fn parse_struct(&mut self, public: bool) -> Statement {
    self.expect(TokenKind::Struct);
    let name = self.expect_identifier();

    self.expect(TokenKind::LeftBrace);
    let mut fields = Vec::new();

    while !self.check(&TokenKind::RightBrace) {
      let field_name = self.expect_identifier();
      self.expect(TokenKind::Colon);

      let field_type = self.parse_type();

      if !self.match_token(&TokenKind::Comma) && !self.check(&TokenKind::RightBrace) {
        panic!(
          "Expected ',' or '}}' after struct field '{}' at line {}",
          field_name.0,
          self.current().line()
        );
      }

      fields.push((field_name, field_type));
    }

    self.expect(TokenKind::RightBrace);
    Statement::Struct {
      name,
      public,
      fields
    }
  }

  fn parse_enum(&mut self, public: bool) -> Statement {
    self.expect(TokenKind::Enum);
    let name = self.expect_identifier();

    self.expect(TokenKind::LeftBrace);
    let mut fields = Vec::new();

    while !self.check(&TokenKind::RightBrace) {
      let field_name = self.expect_identifier();
      fields.push(field_name);

      if !self.match_token(&TokenKind::Comma) && !self.check(&TokenKind::RightBrace) {
        panic!(
          "Expected ',' or '}}' after enum field '{}' at line {}",
          field_name.0,
          self.current().line()
        );
      }
    }

    self.expect(TokenKind::RightBrace);
    Statement::Enum {
      name,
      fields,
      public
    }
  }

  fn parse_if(&mut self) -> Statement {
    self.expect(TokenKind::If);

    let condition = self.parse_expression();
    let then_branch = self.parse_block();
    let mut else_branch = None;

    if self.check(&TokenKind::Else) {
      self.advance();

      if self.check(&TokenKind::If) {
        else_branch = Some(Block {
          statements: vec![self.parse_if()]
        });
      } else if self.check(&TokenKind::LeftBrace) {
        else_branch = Some(self.parse_block());
      } else {
        panic!(
          "Unexpected token after 'else' at line {}",
          self.current().line()
        );
      }
    }

    Statement::If {
      condition,
      then_branch,
      else_branch
    }
  }

  fn parse_for(&mut self) -> Statement {
    self.expect(TokenKind::For);
    let iterator = self.expect_identifier();

    self.expect_keyword("in");
    let iterable = self.parse_expression();
    let body = self.parse_block();

    Statement::For {
      iterator,
      iterable,
      body
    }
  }

  fn parse_match(&mut self) -> Statement {
    self.expect(TokenKind::Match);

    let match_expr = self.parse_expression();
    self.expect(TokenKind::LeftBrace);

    let mut arms = Vec::new();
    while !self.check(&TokenKind::RightBrace) {
      let pattern = self.parse_pattern();
      self.expect(TokenKind::Colon);

      let body = if self.check(&TokenKind::LeftBrace) {
        self.parse_block()
      } else {
        let expr = self.parse_expression();
        Block {
          statements: vec![Statement::Expr(expr)]
        }
      };

      arms.push((pattern, body));
      self.match_token(&TokenKind::Comma);
    }

    self.expect(TokenKind::RightBrace);
    Statement::Match {
      expr: match_expr,
      arms
    }
  }

  fn parse_pattern(&mut self) -> Pattern {
    match self.current().kind() {
      TokenKind::Identifier(sym) => {
        let start = self.current().start() as usize;
        let len = self.current().len as usize;
        let s = unsafe { self.source.get_unchecked(start..start + len) };

        self.advance();

        if s == "_" {
          Pattern::Wild
        } else {
          Pattern::Identifier(sym)
        }
      }
      TokenKind::Literal(lit) => {
        let kind = lit;
        self.advance();

        Pattern::Literal(kind)
      }
      _ => {
        panic!(
          "Unexpected token {:?} in pattern at line {}",
          self.current().kind(),
          self.current().line()
        )
      }
    }
  }

  fn parse_type(&mut self) -> Type {
    let mut ty = match self.current().kind() {
      TokenKind::Identifier(sym) => {
        let base = Type::Simple(sym);
        self.advance();

        base
      }
      TokenKind::LeftBrace => {
        self.advance();

        let mut fields = Vec::new();
        if !self.check(&TokenKind::RightBrace) {
          loop {
            let field_name = self.expect_identifier();
            self.expect(TokenKind::Colon);

            let field_ty = self.parse_type();
            fields.push((field_name, field_ty));

            if !self.match_token(&TokenKind::Comma) {
              break;
            }
          }
        }

        self.expect(TokenKind::RightBrace);
        Type::Object(fields)
      }
      _ => panic!(
        "Unexpected token {:?} at line {} while parsing type",
        self.current().kind(),
        self.current().line()
      )
    };

    if self.match_token(&TokenKind::LessThan) {
      let mut params = Vec::new();

      if !self.check(&TokenKind::GreaterThan) {
        loop {
          params.push(self.parse_type());

          if !self.match_token(&TokenKind::Comma) {
            break;
          }
        }
      }

      self.expect(TokenKind::GreaterThan);
      ty = Type::Generic {
        base: Box::new(ty),
        params
      };
    }

    while self.match_token(&TokenKind::LeftBracket) {
      self.expect(TokenKind::RightBracket);
      ty = Type::Array(Box::new(ty));
    }

    if self.match_token(&TokenKind::Or) {
      let mut types = vec![ty];
      loop {
        types.push(self.parse_type());

        if !self.match_token(&TokenKind::Or) {
          break;
        }
      }

      ty = Type::Union(types);
    }

    ty
  }

  fn parse_generic_type(&mut self) -> Vec<Symbol> {
    let mut generics = Vec::new();
    self.expect(TokenKind::LessThan);

    loop {
      generics.push(self.expect_identifier());

      if !self.match_token(&TokenKind::Comma) {
        break;
      }
    }

    self.expect(TokenKind::GreaterThan);
    generics
  }

  fn parse_block(&mut self) -> Block {
    self.expect(TokenKind::LeftBrace);
    let mut statements = Vec::new();

    while !self.check(&TokenKind::RightBrace) {
      statements.push(self.parse_statement());
    }

    self.expect(TokenKind::RightBrace);
    Block {
      statements
    }
  }

  #[inline(always)]
  fn expect_identifier(&mut self) -> Symbol {
    if let TokenKind::Identifier(sym) = self.advance().kind() {
      sym
    } else {
      panic!("Expected identifier")
    }
  }

  #[inline(always)]
  fn expect_keyword(&mut self, kw: &str) {
    let tok = self.current();
    let start = tok.start() as usize;
    let end = start + tok.len as usize;
    let s = unsafe { self.source.get_unchecked(start..end) };

    if s != kw {
      panic!("Expected keyword '{}', found '{}'", kw, s);
    }

    self.advance();
  }

  #[inline(always)]
  fn consume_flag(&mut self, kw: &str) -> bool {
    match kw {
      "pub" => self.match_token(&TokenKind::Pub),
      "mut" => self.match_token(&TokenKind::Mut),
      _ => false
    }
  }
}
