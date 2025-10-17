import Bloom.Frontend
import Bloom.Frontend.Parser
import Bloom.Frontend.Compositor

inductive ParseErr
  | silent
  | unexpectedEndOfFile
  | expectedSnakeCaseIdentifier (found : Located Token)
  | expectedPascalCaseIdentifier (found : Located Token)
  | expectedSymbolicIdentifier (found : Located Token)
  | expectedNewline (found : Located Token)
  | expectedFound (expected : Token) (found : Located Token)

instance : DefaultErr ParseErr where
  silent := .silent

-- the reader monads here are for indentation context tracking

abbrev BaseParser e := ParserT (Located Token) e (ReaderM String)

abbrev Parser := BaseParser ParseErr

abbrev BranchParser := BaseParser (BranchErr ParseErr ParseErr)

def parse_indentation : Parser String := single .unexpectedEndOfFile fun
  | _ @: .indentation i => .ok i
  | tok                 => .error <| .expectedNewline tok

/--
  Wraps a distinct semantic region in the AST
-/
inductive ASTNode (a : Type)
  | poisoned (err : ParseErr)
  | spanned (value : Located a)

instance : Functor ASTNode where
  map f := fun
    | .poisoned err => .poisoned err
    | .spanned <| s @: x => .spanned <| s @: f x

instance : Monad ASTNode where
  pure x := .spanned <| .none @: x
  bind mx f := match mx with
    | .poisoned err       => .poisoned err
    | .spanned <| s  @: x => match f x with
      | .poisoned err       => .poisoned err
      | .spanned <| s' @: y => .spanned <| (s ++ s') @: y

unsafe def rest (x : a) : Parser (Located a) := do
  let tokens <- (any ParseErr.silent).most
  let mut location : Located a := pure x
  for token in tokens do
    location := location <* token
  return location

unsafe def with_location (p : Parser a) : Parser (Located a)
  := p.layer rest

unsafe def parse_node (region : BaseParser e Unit) (p : Parser a) : BaseParser e (ASTNode a)
  := region.layer <| fun _ => with_location p
    <&> (fun x => .spanned x)
    <?> fun err => return .poisoned err

/--
  not just a string because of funny business
-/
inductive Identifier
  | parsedName (name : String)
  | generated (id : Nat)

def parse_snake : Parser Identifier := single .unexpectedEndOfFile fun
  | _ @: .snakeCaseIdentifier name => .ok    <| .parsedName name
  | tok                            => .error <| .expectedSnakeCaseIdentifier tok

def parse_pascal : Parser Identifier := single .unexpectedEndOfFile fun
  | _ @: .pascalCaseIdentifier name => .ok    <| .parsedName name
  | tok                             => .error <| .expectedPascalCaseIdentifier tok

def parse_symbolic : Parser Identifier := single .unexpectedEndOfFile fun
  | _ @: .symbolicIdentifier name => .ok    <| .parsedName name
  | tok                           => .error <| .expectedSymbolicIdentifier tok

def parse_token (expectation : Token) : Parser Unit := single .unexpectedEndOfFile fun
  (x @: tok) => if tok == expectation
    then .ok ()
    else .error <| .expectedFound expectation (x @: tok)

def parenthesise (parser : Parser a) : Parser a
  := parse_token (.openParens) *> parser <* parse_token (.closeParens)

def parenthesise_branch (parser : BranchParser a) : BranchParser a
  := (parse_token .openParens).expect *> parser <* require (parse_token .openParens)

inductive Expr
  | useVariable (iden : Identifier)

mutual
unsafe def parse_compact_expr : Parser Expr := do
  parenthesise parse_expr <|> .useVariable <$> parse_snake

unsafe def parse_chain_expr : Parser Expr := sorry

-- totally unbounded expression
unsafe def parse_expr : Parser Expr := sorry
end

-- for x in xs { ... }
-- xs.for x => ...

-- yield x
-- x.yield

-- syntax x => ...
/-
  syntax x => x
    .foo
    .bar

  syntax
    .foo
    .bar
-/
/-
  syntax x => match x
    | Foo u => ...
    | Bar v => ...

  syntax
    | Foo u => ...
    | Bar v => ...
-/
