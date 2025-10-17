import Bloom.Basic

-- MARK: Parser

/--
  Parses some number of tokens `t` within the input stream to produce a result.
  The process of taking in the input stream and producing a result
  emits an effect in the transformed monad `m` **before** erroring occurs.
-/
def ParserT (t e : Type) (m : Type -> Type) (a : Type) :=
  (ts : Array t) -> (ExceptT e m <| a × Fin (ts.size + 1))

instance [Monad m] : Monad (ParserT t e m) where
  map f mx := fun ts => mx ts <&> Prod.map f id
  pure x := fun _ => return (x, 0)
  bind mx f := fun ts => do
    let (x, ⟨consumed, _⟩) <- mx ts
    let (y, ⟨consumed', _⟩) <- f x (ts.drop consumed)
    return (y, ⟨consumed + consumed', by grind⟩)

instance [Monad m] : MonadLift (ExceptT e m) (ParserT t e m) where
  monadLift mx := fun _ => return (<- mx, 0)

/--
  Maps the parsers error type.
-/
def ParserT.adapt [Monad m]
    (mx : ParserT t e m a)
    (f : e -> e')
    : ParserT t e' m a
  := fun ts => ExceptT.adapt f (mx ts)

/--
  On failure, runs the recovery function.
  (may adapt the error type)
-/
def ParserT.catchAdapt [Monad m]
    (mx: ParserT t e m a)
    (f : e -> ParserT t e' m a)
    : (ParserT t e' m a) := fun ts => do
  match <- (mx ts).run with
    | .ok x => return x
    | .error err => f err ts

infixl:20 " <?> " => ParserT.catchAdapt

infixl:20 " ?> " => fun mx err => mx <?> fun _ => throw err

infixr:20 " <? " => fun err mx => mx <?> fun _ => throw err

instance [Monad m] : MonadExcept e (ParserT t e m) where
  throw e := fun _ => throw e
  tryCatch body handler := body <?> handler

/--
  Runs the parser on a given token array, consuming everything is not required.
-/
def ParserT.run [Monad m] (mx : ParserT t e m a) (arr : Array t):
  ExceptT e m a := mx arr <&> fun (x, _) => x

-- MARK: Fundimental

/--
  Backtrack on success.
-/
def ParserT.scry [Monad m] (mx : ParserT t e m a): ParserT t e m a :=
  fun arr => mx arr <&> fun (x, _) => (x, 0)

/--
  Only succeeds on end of stream, otherwise raises the given error.
-/
def eos [Monad m] (err : e) : ParserT t e m Unit :=
  fun arr => if arr.isEmpty then return ((), 0) else throw err

/--
  Runs the given parser within the consumed tokens of the original parser.
-/
def ParserT.layer [Monad m]
    (mx : ParserT t e m a)
    (f : a -> ParserT t e m b):
    ParserT t e m b := fun ts => do
  let (x, consumed) <- mx ts
  let (y, _) <- f x <| ts.shrink consumed
  return (y, consumed)

instance [Monad m] [Coe e e'] : Coe (ParserT t e m a) (ParserT t e' m a) where
  coe mx := mx.adapt Coe.coe

-- MARK: Combinators

/--
  Match a single token, **transformatively**.
-/
def singleTrans [Monad m]
    (unexpectedEnd : m e)
    (f : t -> ExceptT e m a)
    : ParserT t e m a := fun ts =>
  match h : ts.size with
    | 0 => do throw (<- unexpectedEnd)
    | n+1 => return ((<- f ts[0]), 0)

/--
  Match a single token.
-/
def single [Monad m]
    (unexpectedEnd : e)
    (f : t -> Except e a)
    : ParserT t e m a := fun ts =>
  match h : ts.size with
    | 0 => throw unexpectedEnd
    | n+1 => (f ts[0]).map fun a => (a, 0)

/--
  Matches **any** token.
-/
def any [Monad m]
    (unexpectedEnd : e)
    : ParserT t e m t
  := single unexpectedEnd .ok

-- MARK: Branching

/--
  An error type to be returned by error-semantic parser branches.
  - Errors `onEntry` signify that the parsed tokens were not unique to the branch.
  - Errors `afterEntered` signify that the parsed tokens can only belong to the branch.
-/
inductive BranchErr (entry_err entered_err : Type) where
  | onEntry : entry_err -> BranchErr entry_err entered_err
  | afterEntered : entered_err -> BranchErr entry_err entered_err

/--
  Makes the entering portion of a branch, should be used before any `require` for example:
  ```lean4
  do
    let _ <- some_parser.expect
    ...
    return require do
      ...
  ```
-/
def ParserT.expect [Monad m]
    (mx: ParserT t e m a)
    : ParserT t (BranchErr e e') m a
  := mx.adapt .onEntry

/--
  Marks the post-entry postion of a branch, will usually be written after a `return`, for example:
  ```lean4
  do
    let _ <- some_parser.expect
    ...
    return require do
      ...
  ```
-/
def require [Monad m]
    (mx: ParserT t e m a)
    : ParserT t (BranchErr e' e) m a
  := mx.adapt .afterEntered

/--
  The error type of a parser mid-branching.
-/
inductive BranchingErr (entry_err entered_err : Type) where
  | selectionFailures : PopulatedList entry_err -> BranchingErr entry_err entered_err
  | canonicalFailure : entered_err -> BranchingErr entry_err entered_err

class DefaultErr (e : Type) where
  silent : e

instance : DefaultErr Unit where
  silent := ()

instance : Coe (BranchErr entry_err entered_err) (BranchingErr entry_err entered_err) where
  coe
  | .onEntry err => .selectionFailures <| err ::| []
  | .afterEntered err => .canonicalFailure err

/--
  Unlike alternative the leftmost success or `.canonicalFailure` is retuend,
  otherwise all `.selectionFailures` are tracked.
-/
def split [Monad m]
    (mx : ParserT t (BranchingErr entry_err entered_err) m a)
    (my : ParserT t (BranchingErr entry_err entered_err) m a)
    : ParserT t (BranchingErr entry_err entered_err) m a :=
  try mx catch
  | .selectionFailures errs =>
    try my catch
    | .selectionFailures errs' =>
      throw (.selectionFailures <| errs ++ errs')
    | e => throw e
  | e => throw e


infixl:20 " <:> " => split

/--
  Splits parsing between multiple branches and unifies the resulting `BranchingErr`.
-/
def branch [Monad m]
    (combine : PopulatedList entry_err -> e)
    (mxs : PopulatedList (ParserT t (BranchErr entry_err e) m a))
    : ParserT t e m a
  := (mxs <&> fun mx => (mx : ParserT t (BranchingErr entry_err e) m a)).foldHead split <?> fun
    | .canonicalFailure err => throw err
    | .selectionFailures errs => throw <| combine errs

instance [Monad m] [DefaultErr e]: Alternative (ParserT t e m) where
  failure := throw DefaultErr.silent
  orElse mx my := do
    try
      mx
    catch _ => my ()

-- MARK: Recovery

/--
  **Always** succeeds returning either the parsed value or caught error.
-/
def ParserT.recover [Monad m]
    (mx : ParserT t e m a)
    : ParserT t e' m (Except e a)
  := mx
    <&> .ok
    <?> fun err => return .error err

/--
  Recoveres only errors on parse branch entry, storing the error. deep errors are rethrown.
-/
def ParserT.recoverEntry [Monad m]
    (mx : ParserT t (BranchErr entry_err entered_err) m a)
    : ParserT t entered_err m (Except entry_err a)
  := mx
    <&> .ok
    <?> fun
      | .onEntry err => return .error err
      | .afterEntered err => throw err

/--
  **Always** succeeds returning some parsed value if present or nothing if there was an error.
-/
def ParserT.opt [Monad m]
    (mx : ParserT t e m a)
    : ParserT t e' m (Option a)
  := mx
    <&> .some
    <?> fun _ => return .none

/--
  Recoveres only errors on parse branch entry, returning nothing if so. deep errors are rethrown.
-/
def ParserT.optEntry [Monad m]
    (mx : ParserT t (BranchErr entry_err entered_err) m a)
    : ParserT t entered_err m (Option a)
  := mx
    <&> .some
    <?> fun
      | .onEntry _ => return .none
      | .afterEntered err => throw err

/--
  Bundles multiple recovered errors together ready to be composed and thrown.
-/
inductive RecoveryBundle (e a : Type) where
    | errors : PopulatedList e -> RecoveryBundle e a
    | parsed : a -> RecoveryBundle e a

/--
  Constructs a `RecoveryBundle` from an error.
-/
def bundle_err (err: e): RecoveryBundle e Never := .errors <| err ::| []

instance : Applicative (RecoveryBundle err) where
  map f := fun
    | .errors errs => .errors errs
    | .parsed x => .parsed (f x)
  pure x := .parsed x
  seq mf pmx := match mf, (pmx ()) with
    | .errors errs, .errors errs' => .errors (errs ++ errs')
    | .errors err, .parsed _ => .errors err
    | .parsed _, .errors err' => .errors err'
    | .parsed f, .parsed x => .parsed <| f x

/--
  Layers a recovered parser ontop of the initial parser,
  converting the recovered `Except` into a `RecoveryBundle`
  in preparation for error bundling.
-/
def ParserT.bundle [Monad m]
    (mx : ParserT t e m a)
    (my : ParserT t e' m b)
    : ParserT t e m (RecoveryBundle e' b)
  := mx.layer (fun _ => my.recover) <&> fun
    | .ok y => .parsed y
    | .error err => bundle_err err

-- MARK: Recursion

/--
  Repeaedly parses until failure.
-/
unsafe def ParserT.most [Monad m]
    (mx : ParserT t e m a)
    : (ParserT t e' m (List a))
  := mx
    >>= (fun x => List.cons x <$> mx.most)
    <?> fun _ => return []

/--
  Repeatedly parses a non-zero amount of items.
-/
unsafe def ParserT.some [Monad m]
    (mx : ParserT t e m a)
    : ParserT t e m (PopulatedList a)
  := PopulatedList.mk <$> mx <*> mx.most

/--
  Alternate between the original term and the gap term.
  Will only provide the error of the original term.
-/
unsafe def ParserT.alternate [Monad m]
    (mx : ParserT t e m a)
    (my : ParserT t e' m b)
    : ParserT t e m (AlternatingList b a) := do
  let x <- mx
  match <- my.opt with
    | .none => return .wrap x
    | .some y => do
      let xys <- mx.alternate my
      return x ::< y >:: xys

/--
  Parses the least amount of the original term until the specified end.
  Will only provide the error of the expected end.
-/
unsafe def ParserT.leastUntil [Monad m]
    (mx : ParserT t e m a)
    (my : ParserT t e' m b)
    : (ParserT t e' m (List a × b))
  := my
    <&> (fun y => ([], y))
    <?> fun err => do
      let x <- mx ?> err
      let (xs, y) <- mx.leastUntil my
      return (x :: xs, y)
