-- MARK: Lists

-- regular

def consF [Functor f] (x : a) (fxs : f (List a))
  := List.cons x <$> fxs

infixr:67 " :$: " => consF

-- populated

structure PopulatedList (a : Type) where
  head : a
  tail : List a

instance : Coe (PopulatedList a) (List a) where
  coe xs := xs.head :: xs.tail

instance [Functor f] : Coe (f <| PopulatedList a) (f <| List a) where
  coe := Functor.map Coe.coe

infixr:67 " ::| " => PopulatedList.mk

def popConsF [Functor f] (x : a) (fxs : f (List a))
  := PopulatedList.mk x <$> fxs

infixr:67 " :$:| " => popConsF

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

-- alternating

mutual
  inductive AlternatingList (a : Type) (b : Type)
    | wrap : b -> AlternatingList a b
    | cons : b -> WithAlternatingList a b -> AlternatingList a b

  structure WithAlternatingList (a : Type) (b : Type) where
    head : a
    tail : AlternatingList a b
end

infixr:67 " >:: " => WithAlternatingList.mk

infixr:67 " ::< " => AlternatingList.cons

def withAltMkF [Functor f] (x : a) (fxys : f <| AlternatingList a b) : f <| WithAlternatingList a b
  := WithAlternatingList.mk x <$> fxys

infixr:67 " >:$: " => withAltMkF

def altConsF [Functor f] (y : b) (fxys : f <| WithAlternatingList a b) : f <| AlternatingList a b
  := AlternatingList.cons y <$> fxys

infixr:67 " :$:< " => altConsF

def AlternatingList.fences : AlternatingList a b -> List a := fun
  | .wrap _ => []
  | _ ::< x >:: xys => x :: xys.fences

def AlternatingList.posts : AlternatingList a b -> PopulatedList b := fun
  | .wrap y => y ::| []
  | y ::< _ >:: xys => y ::| xys.posts

def AlternatingList.concat : AlternatingList a b ->
    WithAlternatingList a b ->
    AlternatingList a b := fun
  | .wrap y => .cons y
  | y ::< x >:: xys => fun rest => y ::< x >:: xys.concat rest

infixr:67 " +::< " => AlternatingList.concat

def AlternatingList.concatF [Functor f]
    (xys : AlternatingList a b)
    (fxys : f <| WithAlternatingList a b)
    : f <| AlternatingList a b
  := AlternatingList.concat xys <$> fxys

infixr:67 " +:$:< " => AlternatingList.concatF

instance : Functor (AlternatingList a) where
  map := let rec map' f xys := match xys with
    | .wrap y => .wrap <| f y
    | y ::< x >:: xys' => f y ::< x >:: (map' f xys')
    map'

instance : Monad (AlternatingList a) where
  pure := .wrap
  bind := let rec bind' xys f := match xys with
    | .wrap y => f y
    | y ::< x >:: xys' => f y +::< x >:: (bind' xys' f)
    bind'

-- MARK: IO

structure StreamIO (a : Type) where
  run: IO.FS.Stream -> IO.FS.Stream -> IO a

instance : Monad StreamIO where
  pure x := { run _ _ := pure x }
  bind mx f := { run i o := do
    let y <- mx.run i o
    (f y).run i o
  }

def liftIO (mx : IO a) : StreamIO a where
  run _ _ := mx

instance : MonadLift IO StreamIO where
  monadLift := liftIO

def runDefault (mx : StreamIO a) : IO a := do
  let stdin <- IO.getStdin
  let stdout <- IO.getStdout

  mx.run stdin stdout

def puts (msg : String) : StreamIO Unit where
  run _ o := o.putStrLn msg

def asks (msg : String) : StreamIO String where
  run i o := o.putStr msg *> i.getLine

def readFile (path : String) : IO String := do
  let handle ← IO.FS.Handle.mk path IO.FS.Mode.read
  let content ← handle.readToEnd
  pure content
