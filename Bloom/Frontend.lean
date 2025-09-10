inductive Located (a: Type) where
  | position
      (line: Nat)
      (index: Nat)
      : a -> Located a
  | singleLine
      (line: Nat)
      (startIndex: Nat)
      (endIndex: Nat)
      : a -> Located a
  | multiLine
      (startLine: Nat)
      (startIndex: Nat)
      (endLine: Nat)
      (endIndex: Nat)
      : a -> Located a
