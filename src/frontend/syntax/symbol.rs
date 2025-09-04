use rustc_hash::FxHashMap;
use std::cell::RefCell;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol(pub(crate) u32);

thread_local! {
  static INTERNER: RefCell<Interner> =
    RefCell::new(Interner::new());
}

#[derive(Debug)]
struct Interner {
  map: FxHashMap<&'static str, Symbol>,
  vec: Vec<&'static str>,
  small_cache: [Option<(&'static str, Symbol)>; 16]
}

impl Interner {
  fn new() -> Self {
    Self {
      map: FxHashMap::with_capacity_and_hasher(1024, Default::default()),
      vec: Vec::with_capacity(1024),
      small_cache: [None; 16]
    }
  }

  #[inline]
  fn intern(&mut self, s: &str) -> Symbol {
    if s.len() <= 8 {
      let cache_idx = (s.as_bytes()[0] as usize) & 0xF;

      if let Some((cached_str, sym)) = self.small_cache[cache_idx] {
        if cached_str == s {
          return sym;
        }
      }
    }

    if let Some(&sym) = self.map.get(s) {
      return sym;
    }

    let boxed: Box<str> = s.to_owned().into_boxed_str();
    let static_str: &'static str = Box::leak(boxed);

    let sym = Symbol(self.vec.len() as u32);
    self.vec.push(static_str);
    self.map.insert(static_str, sym);

    if s.len() <= 8 {
      let cache_idx = (s.as_bytes()[0] as usize) & 0xF;
      self.small_cache[cache_idx] = Some((static_str, sym));
    }

    sym
  }

  #[inline(always)]
  fn resolve(&self, sym: Symbol) -> &'static str {
    unsafe { *self.vec.get_unchecked(sym.0 as usize) }
  }
}

impl Symbol {
  #[inline(always)]
  pub fn intern(s: &str) -> Symbol {
    INTERNER.with(|i| i.borrow_mut().intern(s))
  }

  #[inline(always)]
  pub fn as_str(self) -> &'static str {
    INTERNER.with(|i| i.borrow().resolve(self))
  }
}
