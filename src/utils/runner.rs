use crate::utils::print_ast::print_ast;

use crate::frontend::lexer::Lexer;
use crate::frontend::parser::Parser;

pub fn run_code(code: &str) {
  let lexer_start_time = std::time::Instant::now();
  let mut lexer = Lexer::new(code);

  let mut tokens = Vec::new();
  loop {
    let tok = lexer.next_token();
    tokens.push(tok);

    if tokens.last().unwrap().is_eof() {
      break;
    }
  }

  let lexer_time = lexer_start_time.elapsed();

  let parser_start_time = std::time::Instant::now();
  let mut parser = Parser::new(&tokens, code);

  let program = parser.parse_program();
  let parser_time = parser_start_time.elapsed();

  print_ast(&program, lexer_time, parser_time);
}
