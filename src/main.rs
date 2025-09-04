use crate::utils::runner::run_code;

mod frontend;
mod middle;
mod utils;

fn main() {
  let code = r#"
    let x: string = "Hello, World!"
    println!(x)
  "#;

  run_code(code);
}
