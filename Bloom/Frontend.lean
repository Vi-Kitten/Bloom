
structure Position where
  line : Nat
  char : Nat

instance : ToString Position where
  toString pos := s!"{pos.line + 1}:{pos.char + 1}"

instance : Ord Position where
  compare px py := match compare px.line py.line with
    | .eq => compare px.line py.line
    | ord => ord

instance : Min Position where
  min px py := match compare px py with
    | .lt => px
    | _ => py

instance : Max Position where
  max px py := match compare px py with
    | .gt => px
    | _ => py

structure Span where
  spanStart : Position
  spanEnd : Position

instance : ToString Span where
  toString span := s!"{span.spanStart}-{span.spanEnd}"

instance : Repr Span where
  reprPrec span _ := ToString.toString span

instance : HAppend Span Span Span where
  hAppend sx sy := {
    spanStart := min sx.spanStart sy.spanStart
    spanEnd := max sx.spanEnd sy.spanEnd
  }

structure Located (a: Type) where
  span : Option Span
  val : a
  deriving Repr

instance : Monad Located where
  pure := Located.mk .none
  bind lx f :=
    let ly := f lx.val
    Located.mk (HAppend.hAppend <$> lx.span <*> ly.span) ly.val

structure EditorInfo where
  tabSize : Nat
