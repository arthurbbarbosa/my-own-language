# Compiler of My Language

In this project you will observe how a compiler operates and the complete step-by-step process for creating a programming language. Specifically, this project presents how a **compiled, strongly-typed, multi-paradigm language** was created with a focus on **maximum optimization**, balancing high-level expressiveness with low-level control.

This project was produced exclusively for educational purposes. Although the language - modestly speaking - shows considerable promise and is one I would personally consider using, there is no intention to provide official support or long-term maintenance comparable to widely adopted languages (for example, Rust, Go, Java).

The entire compiler is implemented in pure Rust. This choice was based on a series of experiments with other languages such as C, C++, TypeScript, JavaScript and even Elixir. Rust was selected for its combination of safe memory management, high performance, and native interoperability with low-level code such as assembly.

## Step-By-Step
Below follows a step-by-step description of each stage in the compiler creation process:

1. **EBNF (Extended Backus–Naur Form).**
  [EBNF is a meta-syntactic notation for expressing a context-free grammar.](https://pt.wikipedia.org/wiki/Formalismo_de_Backus-Naur_Estendido#:~:text=Formalismo%20de%20Backus%2DNaur%20Estendido%20%28tamb%C3%A9m%20conhecido%20como%20EBNF%29%20%C3%A9%20uma%20fam%C3%ADlia%20de%20nota%C3%A7%C3%B5es%20meta%2Dsintaxe%2C%20qualquer%20que%20pode%20ser%20usado%20para%20expressar%20uma%20gram%C3%A1tica%20livre%20de%20contexto.) Consequently, it served as a principal resource for structuring the language grammar and for specifying the syntax, including naming of expressions and declarations that comprise the program’s blocks of statements.

2. **Lexical Analysis**
  In lexical analysis (the *lexer* stage), the compiler transforms the input source code (received as a `String`, in my case) into a stream of bytes and scans that stream to produce tokens, while ignoring whitespace, comments and line breaks. Ultimately, the lexer returns a vector of tokens annotated with line number, token kind, position and length. At this stage the compiler does not interpret syntactic structure; its sole responsibility is tokenization.

3. **Syntactic Analysis**
  In syntactic analysis (the *parser* stage), the compiler transforms the token vector produced by the lexer into an Abstract Syntax Tree (AST). The parser organizes the program into blocks of declarations and expressions. At this stage semantic concerns are not addressed; the parser’s only responsibility is to construct a correct AST.

4. **TODO: Semantic Analysis**
  In semantic analysis (informally the *TypeChecker*), the compiler ceases to treat constructs purely as syntactic entities and begins to assign meaning. Semantic analysis is particularly important in a strongly-typed language that requires the programmer to provide or infer types for declarations. The TypeChecker verifies, for example, whether an attempt is made to modify an immutable variable (in which case a semantic error must be reported and compilation prevented).

## Approximate Time of Compilation
The following times are approximate, measured for the example program below.
```
let x: string = "Hello, World!"
println!(x)
```
| Stage                           | Time Elapsed |
| ------------------------------- | ------------ |
| **Lexical Analysis (Lexer)**    | 40.2 µs      |
| **Syntactic Analysis (Parser)** | 28.7 µs      |
| **Total**                       | 68.9 µs      |

> ⚠ Note: Times may vary depending on program size, system load, and hardware. Semantic analysis and code generation are not included.
