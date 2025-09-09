structure PopulatedList (a: Type) where
  head : a
  tail : List a

instance : Coe (PopulatedList a) (List a) where
  coe xs := xs.head :: xs.tail

@[match_pattern] infix:100 " ::| " => PopulatedList.mk

def PopulatedList.concat
    (xs : PopulatedList a)
    (ys : List a)
    : PopulatedList a where
  head := xs.head
  tail := xs.tail ++ ys

def PopulatedList.prepend
    (xs : PopulatedList a)
    (ys : List a)
    : PopulatedList a
  := ys.foldr (fun y xs' => y ::| xs') xs

instance : HAppend (List a) (PopulatedList a) (PopulatedList a) where
  hAppend xs ys := ys.prepend xs

instance : HAppend (PopulatedList a) (List a) (PopulatedList a) where
  hAppend xs ys := xs.concat ys

instance : HAppend (PopulatedList a) (PopulatedList a) (PopulatedList a) where
  hAppend xs ys := xs.concat ys

def PopulatedList.foldHead
    (xs: PopulatedList a)
    (f: a -> a -> a)
    : a
  := xs.tail.foldl f xs.head

instance : Monad PopulatedList where
  pure x := x ::| []
  bind xs f := xs.tail.foldl (fun ys x => ys.concat <| f x) <| f xs.head

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

structure StreamIO (a : Type) where
  (run: IO.FS.Stream -> IO.FS.Stream -> IO a)

instance : Monad StreamIO where
  pure x := { run := fun _ _ => pure x }
  bind mx f := { run := fun i o => do
    let y <- mx.run i o
    (f y).run i o
  }

def liftIO (mx : IO a) : StreamIO a := { run := fun _ _ => mx }

def runDefault (mx : StreamIO a) : IO a := do
  let stdin <- IO.getStdin
  let stdout <- IO.getStdout

  mx.run stdin stdout

def puts (msg : String) : StreamIO Unit := { run := fun _ o => o.putStrLn msg }

def takes : StreamIO String := { run := fun i _ => i.getLine }
