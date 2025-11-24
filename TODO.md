## Approach
- Incorporate whitespace and indentation handling fully into the lexer (no emmiting of indentation tokens).
- Parse everything (`enum`, `inductive`, `struct`, `interface`, `new`, `fn`).
- Create a typed interpreter (this will get ellaborated into its own tasks when I know how).

## Features
- Syntax for re-generalisation of types for traits (allows describing generic behaviour without opinions on structure).