import Bloom.Frontend.Lexer
import Bloom.Frontend.Compositor
import Bloom.Basic

#eval do
  let text ← readFile "./examples/math.bloom"
  IO.println text
  let info : EditorInfo := {
    tabSize := 4
  }
  match lex info text with
    | .error err => IO.println <| repr err
    | .ok toks => for t in toks do -- Id.run <| composite toks.toList
      IO.println <| repr t
