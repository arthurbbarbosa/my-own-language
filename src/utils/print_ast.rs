use crate::frontend::ast::*;
use crate::frontend::syntax::token::LiteralKind;
use std::fmt::Write;
use std::time::Duration;

fn binop_str(op: &BinaryOp) -> &'static str {
  match op {
    BinaryOp::Add => "+",
    BinaryOp::Sub => "-",
    BinaryOp::Mul => "*",
    BinaryOp::Div => "/",
    BinaryOp::Mod => "%",
    BinaryOp::And => "&",
    BinaryOp::Or => "|",
    BinaryOp::Eq => "==",
    BinaryOp::Neq => "!=",
    BinaryOp::Lt => "<",
    BinaryOp::Gt => ">",
    BinaryOp::Lte => "<=",
    BinaryOp::Gte => ">="
  }
}

fn unop_str(op: &UnaryOp) -> &'static str {
  match op {
    UnaryOp::Neg => "-",
    UnaryOp::Not => "!"
  }
}

fn type_to_string(ty: &Type) -> String {
  match ty {
    Type::Simple(sym) => sym.as_str().to_string(),
    Type::Generic {
      base,
      params
    } => {
      let mut s = type_to_string(base);
      s.push('<');

      for (i, p) in params.iter().enumerate() {
        if i != 0 {
          s.push_str(", ");
        }

        s.push_str(&type_to_string(p));
      }

      s.push('>');
      s
    }
    Type::Array(inner) => format!("{}[]", type_to_string(inner)),
    Type::Union(variants) => variants
      .iter()
      .map(type_to_string)
      .collect::<Vec<_>>()
      .join(" | "),
    Type::Object(fields) => {
      let mut s = String::from("{ ");
      for (i, (n, t)) in fields.iter().enumerate() {
        if i != 0 {
          s.push_str(", ");
        }

        write!(s, "{}: {}", n.as_str(), type_to_string(t)).ok();
      }

      s.push_str(" }");
      s
    }
    Type::VarArgs(inner) => format!("{}...", type_to_string(inner))
  }
}

fn literal_to_string(lit: &LiteralValue) -> String {
  match lit {
    LiteralValue::Integer(i) => i.to_string(),
    LiteralValue::Float(f) => f.to_string(),
    LiteralValue::String(sym) => format!("\"{}\"", sym.as_str()),
    LiteralValue::Bool(b) => b.to_string(),
    LiteralValue::Nil => "nil".to_string(),
    LiteralValue::Void => "void".to_string()
  }
}

fn pattern_to_string(p: &Pattern) -> String {
  match p {
    Pattern::Wild => "_".to_string(),
    Pattern::Identifier(sym) => sym.as_str().to_string(),
    Pattern::Literal(lit) => match lit {
      LiteralKind::Integer => "<integer>".to_string(),
      LiteralKind::Float => "<float>".to_string(),
      LiteralKind::String => "<string>".to_string(),
      LiteralKind::Bool(b) => b.to_string(),
      LiteralKind::Nil => "nil".to_string(),
      LiteralKind::Void => "void".to_string()
    }
  }
}

fn write_tree_indent(buf: &mut String, prefix: &str, is_last: bool) {
  buf.push_str(prefix);
  buf.push_str(if is_last { "└─ " } else { "├─ " });
}

fn expr_to_tree(expr: &Expression, buf: &mut String, prefix: &str, is_last: bool) {
  write_tree_indent(buf, prefix, is_last);
  let new_prefix = format!("{}{}", prefix, if is_last { "  " } else { "│ " });

  match expr {
    Expression::Literal(l) => {
      let _ = writeln!(buf, "Literal: {}", literal_to_string(l));
    }
    Expression::Identifier(sym) => {
      let _ = writeln!(buf, "Identifier: {}", sym.as_str());
    }
    Expression::Binary {
      left,
      op,
      right
    } => {
      let _ = writeln!(buf, "Binary: {}", binop_str(op));
      expr_to_tree(left, buf, &new_prefix, false);
      expr_to_tree(right, buf, &new_prefix, true);
    }
    Expression::Unary {
      op,
      expr
    } => {
      let _ = writeln!(buf, "Unary: {}", unop_str(op));
      expr_to_tree(expr, buf, &new_prefix, true);
    }
    Expression::Call {
      callee,
      args,
      generic_args
    } => {
      if !generic_args.is_empty() {
        let g = generic_args
          .iter()
          .map(type_to_string)
          .collect::<Vec<_>>()
          .join(", ");

        let _ = writeln!(buf, "Call <{}>:", g);
      } else {
        let _ = writeln!(buf, "Call:");
      }

      expr_to_tree(callee, buf, &new_prefix, args.is_empty());
      for (i, a) in args.iter().enumerate() {
        expr_to_tree(a, buf, &new_prefix, i == args.len() - 1);
      }
    }
    Expression::Member {
      object,
      property
    } => {
      let _ = writeln!(buf, "Member: .{}", property.as_str());
      expr_to_tree(object, buf, &new_prefix, true);
    }
    Expression::Ternary {
      condition,
      then_branch,
      else_branch
    } => {
      let _ = writeln!(buf, "Ternary:");

      expr_to_tree(condition, buf, &new_prefix, false);
      expr_to_tree(then_branch, buf, &new_prefix, false);
      expr_to_tree(else_branch, buf, &new_prefix, true);
    }
    Expression::Cast {
      expr,
      ty
    } => {
      let _ = writeln!(buf, "Cast: as {}", type_to_string(ty));
      expr_to_tree(expr, buf, &new_prefix, true);
    }
    Expression::Range {
      start,
      end
    } => {
      let _ = writeln!(buf, "Range:");

      expr_to_tree(start, buf, &new_prefix, false);
      expr_to_tree(end, buf, &new_prefix, true);
    }
    Expression::TypeOf(e) => {
      let _ = writeln!(buf, "TypeOf:");
      expr_to_tree(e, buf, &new_prefix, true);
    }
    Expression::Object(fields) => {
      let _ = writeln!(buf, "Object:");

      for (i, (name, val)) in fields.iter().enumerate() {
        write_tree_indent(buf, &new_prefix, i == fields.len() - 1);
        let _ = writeln!(buf, "{}:", name.as_str());

        expr_to_tree(
          val,
          buf,
          &format!(
            "{}{}",
            new_prefix,
            if i == fields.len() - 1 { "  " } else { "│ " }
          ),
          true
        );
      }
    }
    Expression::Array(elems) => {
      let _ = writeln!(buf, "Array:");

      for (i, e) in elems.iter().enumerate() {
        expr_to_tree(e, buf, &new_prefix, i == elems.len() - 1);
      }
    }
  }
}

fn expr_to_tree_no_prefix(expr: &Expression, buf: &mut String, prefix: &str, is_last: bool) {
  let new_prefix = format!("{}{}", prefix, if is_last { "  " } else { "│ " });

  match expr {
    Expression::Literal(l) => {
      let _ = writeln!(buf, "Literal: {}", literal_to_string(l));
    }
    Expression::Identifier(sym) => {
      let _ = writeln!(buf, "Identifier: {}", sym.as_str());
    }
    Expression::Binary {
      left,
      op,
      right
    } => {
      let _ = writeln!(buf, "Binary: {}", binop_str(op));

      expr_to_tree(left, buf, &new_prefix, false);
      expr_to_tree(right, buf, &new_prefix, true);
    }
    Expression::Unary {
      op,
      expr
    } => {
      let _ = writeln!(buf, "Unary: {}", unop_str(op));
      expr_to_tree(expr, buf, &new_prefix, true);
    }
    Expression::Call {
      callee,
      args,
      generic_args
    } => {
      if !generic_args.is_empty() {
        let g = generic_args
          .iter()
          .map(type_to_string)
          .collect::<Vec<_>>()
          .join(", ");

        let _ = writeln!(buf, "Call <{}>:", g);
      } else {
        let _ = writeln!(buf, "Call:");
      }

      expr_to_tree(callee, buf, &new_prefix, args.is_empty());

      for (i, a) in args.iter().enumerate() {
        expr_to_tree(a, buf, &new_prefix, i == args.len() - 1);
      }
    }
    Expression::Member {
      object,
      property
    } => {
      let _ = writeln!(buf, "Member: .{}", property.as_str());
      expr_to_tree(object, buf, &new_prefix, true);
    }
    Expression::Ternary {
      condition,
      then_branch,
      else_branch
    } => {
      let _ = writeln!(buf, "Ternary:");

      expr_to_tree(condition, buf, &new_prefix, false);
      expr_to_tree(then_branch, buf, &new_prefix, false);
      expr_to_tree(else_branch, buf, &new_prefix, true);
    }
    Expression::Cast {
      expr,
      ty
    } => {
      let _ = writeln!(buf, "Cast: as {}", type_to_string(ty));
      expr_to_tree(expr, buf, &new_prefix, true);
    }
    Expression::Range {
      start,
      end
    } => {
      let _ = writeln!(buf, "Range:");

      expr_to_tree(start, buf, &new_prefix, false);
      expr_to_tree(end, buf, &new_prefix, true);
    }
    Expression::TypeOf(e) => {
      let _ = writeln!(buf, "TypeOf:");
      expr_to_tree(e, buf, &new_prefix, true);
    }
    Expression::Object(fields) => {
      let _ = writeln!(buf, "Object:");

      for (i, (name, val)) in fields.iter().enumerate() {
        write_tree_indent(buf, &new_prefix, i == fields.len() - 1);
        let _ = writeln!(buf, "{}:", name.as_str());

        expr_to_tree(
          val,
          buf,
          &format!(
            "{}{}",
            new_prefix,
            if i == fields.len() - 1 { "  " } else { "│ " }
          ),
          true
        );
      }
    }
    Expression::Array(elems) => {
      let _ = writeln!(buf, "Array:");

      for (i, e) in elems.iter().enumerate() {
        expr_to_tree(e, buf, &new_prefix, i == elems.len() - 1);
      }
    }
  }
}

fn stmt_to_tree(stmt: &Statement, buf: &mut String, prefix: &str, is_last: bool) {
  write_tree_indent(buf, prefix, is_last);
  let new_prefix = format!("{}{}", prefix, if is_last { "  " } else { "│ " });

  match stmt {
    Statement::Let {
      name,
      public,
      mutable,
      ty,
      init
    } => {
      let vis = if *public { "pub " } else { "" };
      let mut_str = if *mutable { "mut " } else { "" };

      let _ = writeln!(
        buf,
        "Let: {}{}{}: {}",
        vis,
        mut_str,
        name.as_str(),
        type_to_string(ty)
      );

      if let Some(init) = init {
        expr_to_tree(init, buf, &new_prefix, true);
      }
    }
    Statement::Fun {
      name,
      public,
      params,
      ret_type,
      generic_params,
      body
    } => {
      let vis = if *public { "pub " } else { "" };

      let args = params
        .iter()
        .map(|p| {
          format!(
            "{}{}: {}",
            if p.mutable { "mut " } else { "" },
            p.name.as_str(),
            type_to_string(&p.ty)
          )
        })
        .collect::<Vec<_>>()
        .join(", ");

      let generics = if generic_params.is_empty() {
        "".to_string()
      } else {
        format!(
          "<{}>",
          generic_params
            .iter()
            .map(|g| g.as_str())
            .collect::<Vec<_>>()
            .join(", ")
        )
      };

      let _ = writeln!(
        buf,
        "Fun: {}{}({}) {} -> {}",
        vis,
        name.as_str(),
        args,
        generics,
        type_to_string(ret_type)
      );

      for (i, s) in body.statements.iter().enumerate() {
        stmt_to_tree(s, buf, &new_prefix, i == body.statements.len() - 1);
      }
    }
    Statement::Macro {
      name,
      args
    } => {
      let _ = writeln!(buf, "Macro: {}!", name.as_str());

      for (i, arg) in args.iter().enumerate() {
        expr_to_tree(arg, buf, &new_prefix, i == args.len() - 1);
      }
    }
    Statement::Expr(e) => {
      expr_to_tree_no_prefix(e, buf, prefix, is_last);
    }
    Statement::Return(expr) => {
      let _ = writeln!(buf, "Return:");
      expr_to_tree(expr, buf, &new_prefix, true);
    }
    Statement::Assign {
      left,
      right
    } => {
      let _ = writeln!(buf, "Assign:");

      expr_to_tree(left, buf, &new_prefix, false);
      expr_to_tree(right, buf, &new_prefix, true);
    }
    Statement::Struct {
      name,
      fields,
      public
    } => {
      let vis = if *public { "pub " } else { "" };
      let _ = writeln!(buf, "Struct: {}{}", vis, name.as_str());

      for (i, (fname, fty)) in fields.iter().enumerate() {
        write_tree_indent(buf, &new_prefix, i == fields.len() - 1);
        let _ = writeln!(buf, "{}: {}", fname.as_str(), type_to_string(fty));
      }
    }
    Statement::Enum {
      name,
      fields,
      public
    } => {
      let vis = if *public { "pub " } else { "" };
      let _ = writeln!(buf, "Enum: {}{} -> {:?}", vis, name.as_str(), fields);
    }
    Statement::Import {
      path,
      public
    } => {
      let _ = writeln!(
        buf,
        "Import: {}{}",
        if *public { "pub " } else { "" },
        path.as_str()
      );
    }
    Statement::If {
      condition,
      then_branch,
      else_branch
    } => {
      let _ = writeln!(buf, "If:");
      expr_to_tree(condition, buf, &new_prefix, false);

      let _ = writeln!(buf, "{}Then:", new_prefix);

      for s in &then_branch.statements {
        stmt_to_tree(s, buf, &format!("{}  ", new_prefix), true);
      }

      if let Some(eb) = else_branch {
        let _ = writeln!(buf, "{}Else:", new_prefix);

        for s in &eb.statements {
          stmt_to_tree(s, buf, &format!("{}  ", new_prefix), true);
        }
      }
    }

    Statement::For {
      iterator,
      iterable,
      body
    } => {
      let _ = writeln!(buf, "For: {}", iterator.as_str());
      expr_to_tree(iterable, buf, &new_prefix, false);

      for (i, s) in body.statements.iter().enumerate() {
        stmt_to_tree(s, buf, &new_prefix, i == body.statements.len() - 1);
      }
    }

    Statement::Match {
      expr,
      arms
    } => {
      let _ = writeln!(buf, "Match:");
      expr_to_tree(expr, buf, &new_prefix, false);

      for (_i, (pat, blk)) in arms.iter().enumerate() {
        let _ = writeln!(buf, "{}Arm: {}", new_prefix, pattern_to_string(pat));

        for (j, s) in blk.statements.iter().enumerate() {
          stmt_to_tree(
            s,
            buf,
            &format!("{}  ", new_prefix),
            j == blk.statements.len() - 1
          );
        }
      }
    }

    Statement::Continue => {
      let _ = writeln!(buf, "Continue");
    }

    Statement::Break => {
      let _ = writeln!(buf, "Break");
    }
  }
}

pub fn print_ast(program: &Program, lexer_time: Duration, parser_time: Duration) {
  let mut buf = String::new();

  let _ = writeln!(buf, "Time elapsed by Lexer:   {:?}", lexer_time);
  let _ = writeln!(buf, "Time elapsed by Parser:  {:?}", parser_time);

  for (i, stmt) in program.body.iter().enumerate() {
    stmt_to_tree(stmt, &mut buf, "", i == program.body.len() - 1);
  }

  println!("{}", buf);
}
