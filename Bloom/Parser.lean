class DefaultErr (e: Type) where
    silent : e

/--
    Parses some number of tokens `t` within the input stream to produce a result.
    The process of taking in the input stream and producing a result
    emmits an effect in the transformed monad `m` **before** erroring occurs.
-/
structure Parser (t e: Type) (m : Type -> Type) (a : Type) where
    parse {n: Nat} (_ : Vector t n) :
        m <| Except e <| a × {consumed : Nat // consumed ≤ n}

instance [Monad m]: MonadLift m (Parser t e m) where
    monadLift mx := { parse {n: Nat} _ := mx <&> fun x => .ok (x, ⟨0, Nat.zero_le n⟩) }

instance [Monad m]: MonadLift (Except e) (Parser t e m) where
    monadLift mx := { parse {n: Nat} _ := return mx <&> fun x => (x, ⟨0, Nat.zero_le n⟩)}

/--
    Runs the parser on a given token array, consuming everything is not required.
-/
def Parser.run [Functor m]
    (mx: Parser t e m a)
    (arr: Array t)
    : m <| Except e a
    := mx.parse (arr.toVector) <&> Except.map fun (x, _) => x

infixl:60 " +<=+ " => Nat.le_trans

-- MARK: Fundimental

/--
    Only succeeds on end of stream, otherwise raises the given error.
-/
def eos [Applicative m] (err: e): Parser t e m Unit where
    parse {n: Nat} _ := pure <| match n with
        | 0 => .ok ((), ⟨0, by decide⟩)
        | _ => .error err

/--
    Runs the given parser within the consumed tokens of the original parser.
-/
def Parser.layer [Monad m]
    (mx: Parser t e m a)
    (f: a -> Parser t e m b):
    Parser t e m b where
    parse ts := mx.parse ts >>= fun
        | .error err => return .error err
        | .ok (x, consumed) => (f x).parse (ts.shrink consumed)
            <&> Except.map fun (y, _) => (y, consumed)

/--
    **Always** fails, throws the given error.
-/
def throw [Monad m]
    (err: e)
    : Parser t e m a where
    parse _ := return .error err

/--
    On failure, runs the recovery function.
-/
def Parser.catch [Monad m]
    (mx: Parser t e m a)
    (f: e -> Parser t e' m a)
    : (Parser t e' m a) where
    parse ts := mx.parse ts >>= fun
        | .ok (x, consumed) => return .ok (x, consumed)
        | .error err => (f err).parse ts

infixl:20 " <?> " => Parser.catch

infixl:20 " <? " => fun e mx => Parser.catch mx (fun _ => throw e)

infixl:20 " ?> " => fun mx e => Parser.catch mx (fun _ => throw e)

/--
    Maps the parsers error type.
-/
def Parser.map_err [Functor m]
    (mx: Parser t e m a)
    (f: e -> e')
    : Parser t e' m a where
    parse ts := mx.parse ts <&> Except.mapError f

instance [Functor m] [Coe e e']: Coe (Parser t e m a) (Parser t e' m a) where
    coe mx := mx.map_err Coe.coe

-- MARK: Combinators

/--
    Match a single token, **transformatively**.
-/
def single_trans [Functor m]
    (unexpected_end: m e)
    (f: t -> m (Except e a))
    : Parser t e m a where
    parse {n: Nat} ts := match n with
        | 0 => .error <$> unexpected_end
        | .succ n' => f (ts.get 1)
            <&> Except.map fun x => (x, ⟨0, Nat.zero_le n'.succ⟩)

/--
    Match a single token.
-/
def single [Applicative m]
    (unexpected_end: e)
    (f: t -> Except e a)
    : Parser t e m a where
    parse {n: Nat} ts := pure <| match n with
        | 0 => .error unexpected_end
        | .succ n' => f (ts.get 1)
            <&> fun x => (x, ⟨0, Nat.zero_le n'.succ⟩)

/--
    Matches **any** token.
-/
def any [Applicative m]
    (unexpected_end: e)
    : Parser t e m t
    := single unexpected_end .ok

instance [Functor m]: Functor (Parser t e m) where
    map f mx := { parse ts := mx.parse ts <&> Except.map (Prod.map f id)}

instance [Monad m]: Monad (Parser t e m) where
    pure x := { parse {n: Nat} _ := pure <| pure <| (x, ⟨0, Nat.zero_le n⟩) }
    bind mx f :=  { parse ts := mx.parse ts >>= fun
        | .error err => return .error err
        | .ok (x, ⟨consumed, bound⟩) => (f x).parse (ts.drop consumed)
            <&> Except.map fun (y, ⟨consumed', bound'⟩) => (y, ⟨
                consumed + consumed',
                Nat.le_of_eq (Nat.add_comm consumed consumed')
                +<=+ Nat.add_le_add_right bound' consumed
                +<=+ Nat.le_of_eq (Nat.sub_add_cancel bound)
            ⟩)
    }

-- MARK: Branching

/--
    An error type to be returned by error-semantic parser branches.
    - Errors `on_entry` signify that the parsed tokens were not unique to the branch.
    - Errors `after_entered` signify that the parsed tokens can only belong to the branch.
-/
inductive BranchErr (entry_err entered_err : Type) where
    | on_entry : entry_err -> BranchErr entry_err entered_err
    | after_entered : entered_err -> BranchErr entry_err entered_err

/--
    The error type of a parser mid-branching.
-/
inductive BranchingErr (entry_err entered_err : Type) where
    | selection_failures : List entry_err -> BranchingErr entry_err entered_err
    | canonical_failure : entered_err -> BranchingErr entry_err entered_err

instance: DefaultErr (BranchingErr entry_err entered_err) where
    silent := .selection_failures []

instance: Coe (BranchErr entry_err entered_err) (BranchingErr entry_err entered_err) where
    coe := fun
        | .on_entry err => .selection_failures [err]
        | .after_entered err => .canonical_failure err

/--
    Unlike alternative the leftmost success or `.canonical_failure` is retuend,
    otherwise all `.selection_failures` are tracked.
-/
def split [Monad m]
    (mx: Parser t (BranchingErr entry_err entered_err) m a)
    (my: Parser t (BranchingErr entry_err entered_err) m a)
    : Parser t (BranchingErr entry_err entered_err) m a where
    parse ts := mx.parse ts >>= fun
        | .ok (x, consumed) => return .ok (x, consumed)
        | .error (.canonical_failure err) => return .error (.canonical_failure err)
        | .error (.selection_failures errs) => my.parse ts <&> fun
            | .ok (y, consumed) => .ok (y, consumed)
            | .error (.canonical_failure err) => .error (.canonical_failure err)
            | .error (.selection_failures errs') =>.error (.selection_failures <| errs ++ errs')

infixl:20 " <:> " => split

/--
    Splits parsing between multiple branches and unifies the resulting `BranchingErr`.
-/
def branch [Monad m]
    (raise: List entry_err -> e)
    (mxs: List (Parser t (BranchingErr entry_err e) m a))
    : Parser t e m a
    := (mxs.foldl split <| throw DefaultErr.silent).map_err fun
        | .canonical_failure err => err
        | .selection_failures errs => raise errs

instance [Monad m] [DefaultErr e]: Alternative (Parser t e m) where
    failure := throw DefaultErr.silent
    orElse mx my := mx <?> fun _ => my ()

-- MARK: Recovery

/--
    **Always** succeeds returning either the parsed value or caught error.
-/
def Parser.recover [Monad m]
    (mx: Parser t e m a)
    : Parser t e' m (Except e a)
    := mx
        <&> (.ok)
        <?> fun err => return .error err

/--
    **Always** succeeds returning some parsed value if present or nothing if there was an error.
-/
def Parser.opt [Monad m]
    (mx: Parser t e m a)
    : Parser t e' m (Option a)
    := mx
        <&> (.some)
        <?> fun _ => return .none

/--
    Bundles multiple recovered errors together ready to be composed and thrown.
-/
inductive RecoveryBundle (e a : Type) where
    | errors : Vector e n -> n >= 1 -> RecoveryBundle e a
    | parsed : a -> RecoveryBundle e a

/--
    Constructs a `RecoveryBundle` from an error.
-/
def bundle_err (err: e): RecoveryBundle e Never := .errors #v[err] (by decide)

instance: Applicative (RecoveryBundle err) where
    map f := fun
        | .errors errs h => .errors errs h
        | .parsed x => .parsed (f x)
    pure x := .parsed x
    seq mf pmx := match mf, (pmx ()) with
        | .errors errs h, .errors errs' h' => .errors (errs ++ errs') (by decide +<=+ Nat.add_le_add h h')
        | .errors err h, .parsed _ => .errors err h
        | .parsed _, .errors err' h' => .errors err' h'
        | .parsed f, .parsed x => .parsed <| f x

/--
    Layers a recovered parser ontop of the initial parser,
    converting the recovered `Except` into a `RecoveryBundle`
    in preparation for error bundling.
-/
def Parser.bundle [Monad m]
    (mx: Parser t e m a)
    (my: Parser t e' m b)
    : Parser t e m (RecoveryBundle e' b) := mx.layer (fun _ => my.recover)
        <&> fun
            | .ok y => .parsed y
            | .error err => bundle_err err

-- MARK: Recursion

/--
    Repeaedly parses until failure.
-/
partial def Parser.most [Monad m]
    (mx: Parser t e m a)
    : (Parser t e' m (List a))
    := mx
        >>= (fun x => List.cons x <$> mx.most)
        <?> fun _ => return []

/--
    Parses the least amount of the original term until the specified end.
    Will only provide the error of the expected end.
-/
partial def Parser.least_until [Monad m]
    (mx: Parser t e m a)
    (my: Parser t e' m b)
    : (Parser t e' m (List a × b))
    := my
        <&> (fun y => ([], y))
        <?> fun err => do
            let x <- mx ?> err
            let (xs, y) <- mx.least_until my
            return (x :: xs, y)
