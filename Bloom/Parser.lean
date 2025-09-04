abbrev Token: Type := Unit

abbrev TokenErr: Type := Unit

inductive BranchErr (entry_err entered_err : Type) where
    | on_entry : entry_err -> BranchErr entry_err entered_err
    | after_entered : entered_err -> BranchErr entry_err entered_err

inductive BranchingErr (entry_err entered_err : Type) where
    | selection_failures : Array entry_err -> BranchingErr entry_err entered_err
    | canonical_failure : entered_err -> BranchingErr entry_err entered_err

structure Parser (e: Type) (d : Nat) (m : Type -> Type) (A : Type) where
    parse {n: Nat} (_ : Vector Token n) :
        m <| Except e <| A × {consumed : Nat // d ≤ consumed ∧ consumed ≤ n}

infixl:60 "+<=+" => Nat.le_trans

-- MARK: Coercion

def coerce_down {d d': Nat} [Functor m]
    (down: d' <= d)
    (mx: Parser e d m a)
    : Parser e d' m a where
    parse := fun {n: Nat} (ts: Vector _ n) => mx.parse ts <&> fun
        | .ok (x, ⟨consumed, lower, upper⟩) => .ok (x, ⟨consumed, down +<=+ lower, upper⟩)
        | .error err => .error err

def hoist_err [Functor m] (f: e -> e') (mx: Parser e d m a): Parser e' d m a where
    parse := fun ts => mx.parse ts <&> fun
        | .ok x => .ok x
        | .error err => .error <| f err

instance: Coe (BranchErr e f) (BranchingErr e f) where
    coe := fun
        | .on_entry err => .selection_failures #[err]
        | .after_entered err => .canonical_failure err

instance [Functor m] [Coe e e']:
    Coe (Parser e d m a) (Parser e' d m a) where

    coe := hoist_err fun x => x

instance (p : Parser e d m a) [Functor m]:
    CoeDep (Parser e d m a) p (Parser e 0 m a) where

    coe := coerce_down (Nat.zero_le d) p

-- MARK: Branching

def common_branch [Monad m]
    (mx: Parser (BranchingErr entry_err entered_err) d m a)
    (my: Parser (BranchingErr entry_err entered_err) d' m a)
    : Parser (BranchingErr entry_err entered_err) (min d d') m a where
    parse := fun {n: Nat} (ts: Vector _ n) => do
        match <- mx.parse ts with
            | .ok (x, ⟨consumed, lower, upper⟩) =>
                return .ok (x, ⟨consumed, Nat.min_le_left d d' +<=+ lower, upper⟩)
            | .error (.canonical_failure err) => return .error (.canonical_failure err)
            | .error (.selection_failures errs) => match <- my.parse ts with
                | .ok (y, ⟨consumed, lower, upper⟩) =>
                    return .ok (y, ⟨consumed, Nat.min_le_right d d' +<=+ lower, upper⟩)
                | .error (.canonical_failure err) => return .error (.canonical_failure err)
                | .error (.selection_failures errs') =>
                    return .error (.selection_failures <| errs ++ errs')

def branch [Monad m]
    (mx: Parser (BranchingErr entry_err entered_err) d m a)
    (my: Parser (BranchingErr entry_err entered_err) d m a)
    : Parser (BranchingErr entry_err entered_err) d m a
    := coerce_down (Nat.min_self d |> Eq.symm |> Nat.le_of_eq) <| common_branch mx my

infixl:20 "-<:>" => common_branch
infixl:20 "<:>" => branch

def common_alt [Monad m]
    (mx: Parser e d m a)
    (my: Parser e d' m a)
    : Parser e (min d d') m a where
    parse := fun ts => do
        match <- mx.parse ts with
            | .ok (x, ⟨consumed, lower, upper⟩) =>
                return .ok (x, ⟨consumed, Nat.min_le_left d d' +<=+ lower, upper⟩)
            | .error err => match <- my.parse ts with
                | .ok (y, ⟨consumed, lower, upper⟩) =>
                    return .ok (y, ⟨consumed, Nat.min_le_right d d' +<=+ lower, upper⟩)
                | .error _ => return .error err

def alt [Monad m]
    (mx: Parser e d m a)
    (my: Parser e d m a)
    : Parser e d m a
    := coerce_down (Nat.min_self d |> Eq.symm |> Nat.le_of_eq) <| common_alt mx my

-- MARK: Combinators

instance [Functor m]: Functor (Parser e d m) where
    map f mx := { parse := fun ts => mx.parse ts <&> Except.map (Prod.map f id)}

def addative_join [Monad m]
    (mmx: Parser e d m (Parser e d' m a))
    : Parser e (d + d') m a where
    parse := fun ts => do
        match <- mmx.parse ts with
            | .error err => return .error err
            | .ok (mx, ⟨consumed, lower, upper⟩) => match <- mx.parse <| ts.drop consumed with
                | .error err => return .error err
                | .ok (x, ⟨consumed', lower', upper'⟩) => return .ok (x, ⟨
                    consumed + consumed',
                    Nat.add_le_add lower lower',
                    Nat.le_of_eq (Nat.add_comm consumed consumed')
                    +<=+ Nat.add_le_add_right upper' consumed
                    +<=+ Nat.le_of_eq (Nat.sub_add_cancel upper)
                ⟩)

def addative_bind [Monad m]
    (mx: Parser e d m a)
    (f: a -> Parser e d' m b)
    : Parser e (d + d') m b
    := addative_join <| f <$> mx

infixl:55 "+>>=" => addative_bind

def addative_apply [Monad m]
    (mf: Parser e d m (a -> b))
    (mx: Parser e d' m a)
    : Parser e (d + d') m b
    := mf +>>= Functor.mapRev mx

infixl:100 "<+>" => addative_apply

def compose_discard [Monad m]
    (mx: Parser e d m a)
    (my: Parser e d' m b)
    : Parser e (d + d') m b
    := ((fun _ y => y) <$> mx) <+> my

infixl:60 ">+>" => compose_discard

def compose_preserve [Monad m]
    (mx: Parser e d m a)
    (my: Parser e d' m b)
    : Parser e (d + d') m a
    := ((fun x _ => x) <$> mx) <+> my

infixl:60 "<+<" => compose_preserve

instance {e : Type} {m: Type -> Type} [Monad m]: Monad (Parser e 0 m) where
    pure x := { parse := fun {n: Nat} (_: Vector _ n) =>
        pure <| pure <| (x, ⟨0, by decide, Nat.zero_le n⟩)
    }
    bind mx f := mx +>>= f

-- MARK: Recursion

-- no clue how I am going to do this erganomically NGL
-- ideally I can find a way to interact with leans implicit halting checker
-- and describe a constraint on `d` in `Parser e d m a` to allow recursion
