import Bloom.Frontend.Lexer
import Bloom.Frontend.Compositor
import Bloom.Basic

#eval do
  let text ← readFile "./examples/math.bloom"
  IO.println text
  match lex text with
    | .error err => IO.println <| repr err
    | .ok toks => for t in Id.run <| composite toks.toList do
      IO.println <| repr t
