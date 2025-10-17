import Bloom

def greet (name : String) := do
    puts s!"Hello {name}!"

def main : IO Unit
  := runDefault <| asks "What is your name?" >>= greet

-- inductive ParseError e where
--   | unexpectedEOS
--   | expectationError (err : e)

-- unsafe inductive LL1ParserT (t e : Type) (m : Type -> Type) [Functor m] (a : Type) where
--   | building (expect : t -> ExceptT e m (LL1ParserT t e m a))
--   | ready (otherwise : m a) (suppose : t -> Option (m <| LL1ParserT t e m a))
