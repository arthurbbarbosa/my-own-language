#![allow(dead_code, unused_variables)]
use crate::frontend::ast::*;
use crate::frontend::syntax::symbol::Symbol;

use rustc_hash::FxHashMap;

#[derive(Debug, Clone)]
pub struct CommonTypes {
  pub string: Symbol,
  pub boolean: Symbol,
  pub integer: Symbol,
  pub float: Symbol,
  pub nil: Symbol,
  pub void: Symbol,
  pub unknown: Symbol,
  pub array: Symbol,
  pub object: Symbol,
  pub fun: Symbol
}

impl CommonTypes {
  pub fn new() -> Self {
    Self {
      string: Symbol::intern("string"),
      boolean: Symbol::intern("bool"),
      integer: Symbol::intern("integer"),
      float: Symbol::intern("float"),
      nil: Symbol::intern("nil"),
      void: Symbol::intern("void"),
      unknown: Symbol::intern("unknown"),
      array: Symbol::intern("array"),
      object: Symbol::intern("object"),
      fun: Symbol::intern("fun")
    }
  }
}

#[derive(Debug, Clone)]
pub struct TypeEnv {
  pub variables: FxHashMap<Symbol, Type>,
  pub structs: FxHashMap<Symbol, StructType>,
  pub enums: FxHashMap<Symbol, EnumType>,
  pub modules: FxHashMap<Symbol, TypeEnv>,
  pub globals: FxHashMap<Symbol, FunctionType>,
  pub methods: FxHashMap<Symbol, FxHashMap<Symbol, FunctionType>>,
  pub common: CommonTypes
}

#[derive(Debug, Clone)]
pub struct FunctionType {
  params: Vec<Type>,
  return_type: Box<Type>,
  generic_params: Vec<Symbol>
}

#[derive(Debug, Clone)]
pub struct StructType {
  fields: FxHashMap<Symbol, Type>,
  generic_params: Vec<Symbol>
}

#[derive(Debug, Clone)]
pub struct EnumType {
  variants: Vec<Symbol>,
  generic_params: Vec<Symbol>
}

pub struct TypeChecker {
  env: TypeEnv,
  current_function_return: Option<Type>,
  generic_context: FxHashMap<Symbol, Type>,
  type_cache: FxHashMap<*const Expression, Type>
}

impl TypeChecker {
  pub fn new() -> Self {
    let mut env = TypeEnv {
      variables: FxHashMap::with_capacity_and_hasher(1024, Default::default()),
      structs: FxHashMap::with_capacity_and_hasher(128, Default::default()),
      enums: FxHashMap::with_capacity_and_hasher(128, Default::default()),
      modules: FxHashMap::with_capacity_and_hasher(64, Default::default()),
      globals: FxHashMap::with_capacity_and_hasher(256, Default::default()),
      methods: FxHashMap::with_capacity_and_hasher(64, Default::default()),
      common: CommonTypes::new()
    };

    env.add_builtins();
    TypeChecker {
      env,
      current_function_return: None,
      generic_context: FxHashMap::with_capacity_and_hasher(128, Default::default()),
      type_cache: FxHashMap::with_capacity_and_hasher(2048, Default::default())
    }
  }

  pub fn check_program(&mut self, program: &Program) {
    for stmt in &program.body {
      self.check_statement(stmt);
    }
  }

  fn check_statement(&mut self, stmt: &Statement) {
    todo!();
  }
}

impl TypeEnv {
  fn add_builtins(&mut self) {
    self.add_global_functions();
    self.add_array_methods();
    self.add_string_methods();
    self.add_integer_methods();
    self.add_float_methods();
  }

  fn add_global_functions(&mut self) {
    let c = &self.common;

    self
      .globals
      .insert(Symbol::intern("println"), FunctionType {
        params: vec![Type::Simple(c.string)],
        return_type: Box::new(Type::Simple(c.void)),
        generic_params: vec![]
      });

    self.globals.insert(Symbol::intern("format"), FunctionType {
      params: vec![
        Type::Simple(c.string),
        Type::VarArgs(Box::new(Type::Simple(c.unknown))),
      ],
      return_type: Box::new(Type::Simple(c.string)),
      generic_params: vec![]
    });

    self.globals.insert(Symbol::intern("env"), FunctionType {
      params: vec![Type::Simple(c.string)],
      return_type: Box::new(Type::Simple(c.string)),
      generic_params: vec![]
    });
  }

  fn add_method(&mut self, base: Symbol, name: &str, f: FunctionType) {
    let entry = self
      .methods
      .entry(base)
      .or_insert_with(|| FxHashMap::with_capacity_and_hasher(32, Default::default()));

    entry.insert(Symbol::intern(name), f);
  }

  fn add_string_methods(&mut self) {
    let c = &self.common.clone();

    self.add_method(c.string, "contains", FunctionType {
      params: vec![Type::Simple(c.string)],
      return_type: Box::new(Type::Simple(c.boolean)),
      generic_params: vec![]
    });

    self.add_method(c.string, "starts_with", FunctionType {
      params: vec![Type::Simple(c.string)],
      return_type: Box::new(Type::Simple(c.boolean)),
      generic_params: vec![]
    });

    self.add_method(c.string, "ends_with", FunctionType {
      params: vec![Type::Simple(c.string)],
      return_type: Box::new(Type::Simple(c.boolean)),
      generic_params: vec![]
    });

    self.add_method(c.string, "trim", FunctionType {
      params: vec![],
      return_type: Box::new(Type::Simple(c.string)),
      generic_params: vec![]
    });

    self.add_method(c.string, "to_lower_case", FunctionType {
      params: vec![],
      return_type: Box::new(Type::Simple(c.string)),
      generic_params: vec![]
    });

    self.add_method(c.string, "to_upper_case", FunctionType {
      params: vec![],
      return_type: Box::new(Type::Simple(c.string)),
      generic_params: vec![]
    });

    self.add_method(c.string, "trunc", FunctionType {
      params: vec![Type::Simple(c.integer)],
      return_type: Box::new(Type::Simple(c.string)),
      generic_params: vec![]
    });
  }

  fn add_integer_methods(&mut self) {
    let c = &self.common.clone();

    self.add_method(c.integer, "to_string", FunctionType {
      params: vec![],
      return_type: Box::new(Type::Simple(c.string)),
      generic_params: vec![]
    });
  }

  fn add_float_methods(&mut self) {
    let c = &self.common.clone();

    self.add_method(c.float, "to_string", FunctionType {
      params: vec![],
      return_type: Box::new(Type::Simple(c.string)),
      generic_params: vec![]
    });

    self.add_method(c.float, "to_precision", FunctionType {
      params: vec![Type::Simple(c.integer)],
      return_type: Box::new(Type::Union(vec![
        Type::Simple(c.float),
        Type::Simple(c.integer),
      ])),
      generic_params: vec![]
    });
  }

  fn add_array_methods(&mut self) {
    let c = &self.common.clone();
    let t = Symbol::intern("T");

    self.add_method(c.array, "map", FunctionType {
      params: vec![],
      return_type: Box::new(Type::Union(vec![Type::Simple(t), Type::Simple(c.nil)])),
      generic_params: vec![t]
    });

    self.add_method(c.array, "last", FunctionType {
      params: vec![],
      return_type: Box::new(Type::Union(vec![Type::Simple(t), Type::Simple(c.nil)])),
      generic_params: vec![t]
    });

    self.add_method(c.array, "push", FunctionType {
      params: vec![Type::Simple(t)],
      return_type: Box::new(Type::Generic {
        base: Box::new(Type::Simple(c.array)),
        params: vec![Type::Simple(t)]
      }),
      generic_params: vec![t]
    });

    self.add_method(c.array, "concat", FunctionType {
      params: vec![Type::Generic {
        base: Box::new(Type::Simple(c.array)),
        params: vec![Type::Simple(t)]
      }],
      return_type: Box::new(Type::Generic {
        base: Box::new(Type::Simple(c.array)),
        params: vec![Type::Simple(t)]
      }),
      generic_params: vec![t]
    });

    self.add_method(c.array, "filter", FunctionType {
      params: vec![Type::Generic {
        base: Box::new(Type::Simple(c.fun)),
        params: vec![Type::Simple(t), Type::Simple(c.boolean)]
      }],
      return_type: Box::new(Type::Generic {
        base: Box::new(Type::Simple(c.array)),
        params: vec![Type::Simple(t)]
      }),
      generic_params: vec![t]
    });
  }
}
