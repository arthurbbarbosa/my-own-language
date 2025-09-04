use crate::frontend::syntax::symbol::Symbol;
use crate::frontend::syntax::token::*;

#[derive(Debug, Clone)]
pub struct Program {
  pub body: Vec<Statement>
}

#[derive(Debug, Clone)]
pub struct Block {
  pub statements: Vec<Statement>
}

#[derive(Debug, Clone)]
pub enum Statement {
  Let {
    name: Symbol,
    public: bool,
    mutable: bool,
    ty: Type,
    init: Option<Expression>
  },
  Fun {
    name: Symbol,
    public: bool,
    params: Vec<Param>,
    ret_type: Type,
    generic_params: Vec<Symbol>,
    body: Block
  },
  Import {
    path: Symbol,
    public: bool
  },
  If {
    condition: Expression,
    then_branch: Block,
    else_branch: Option<Block>
  },
  For {
    iterator: Symbol,
    iterable: Expression,
    body: Block
  },
  Continue,
  Break,
  Return(Expression),
  Match {
    expr: Expression,
    arms: Vec<(Pattern, Block)>
  },
  Assign {
    left: Box<Expression>,
    right: Box<Expression>
  },
  Struct {
    name: Symbol,
    fields: Vec<(Symbol, Type)>,
    public: bool
  },
  Enum {
    name: Symbol,
    fields: Vec<Symbol>,
    public: bool
  },
  Macro {
    name: Symbol,
    args: Vec<Expression>
  },
  Expr(Expression)
}

#[derive(Debug, Clone)]
pub enum Expression {
  Literal(LiteralValue),
  Identifier(Symbol),
  Binary {
    left: Box<Expression>,
    op: BinaryOp,
    right: Box<Expression>
  },
  Unary {
    op: UnaryOp,
    expr: Box<Expression>
  },
  Call {
    callee: Box<Expression>,
    args: Vec<Expression>,
    generic_args: Vec<Type>
  },
  Member {
    object: Box<Expression>,
    property: Symbol
  },
  Ternary {
    condition: Box<Expression>,
    then_branch: Box<Expression>,
    else_branch: Box<Expression>
  },
  Cast {
    expr: Box<Expression>,
    ty: Type
  },
  Range {
    start: Box<Expression>,
    end: Box<Expression>
  },
  TypeOf(Box<Expression>),
  Object(Vec<(Symbol, Expression)>),
  Array(Vec<Expression>)
}

#[derive(Debug, Clone)]
pub enum Pattern {
  Wild,
  Identifier(Symbol),
  Literal(LiteralKind)
}

#[derive(Debug, Clone)]
pub struct Param {
  pub name: Symbol,
  pub ty: Type,
  pub mutable: bool
}

#[derive(Debug, Clone)]
pub enum Type {
  Simple(Symbol),
  Generic { base: Box<Type>, params: Vec<Type> },
  Array(Box<Type>),
  Union(Vec<Type>),
  Object(Vec<(Symbol, Type)>),
  VarArgs(Box<Type>)
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
  Integer(i64),
  Float(f64),
  String(Symbol),
  Bool(bool),
  Void,
  Nil
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  And,
  Or,
  Eq,
  Neq,
  Lt,
  Gt,
  Lte,
  Gte
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
  Neg,
  Not
}
