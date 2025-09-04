pub mod print_ast;
pub mod runner;

pub const CHAR_TABLE: [u8; 256] = {
  let mut table = [0u8; 256];
  let mut i = 0;

  while i < 256 {
    table[i] = match i as u8 {
      b'a'..=b'z' | b'A'..=b'Z' | b'_' => 1,
      b'0'..=b'9' => 2,
      _ => 0
    };

    i += 1;
  }

  table
};
