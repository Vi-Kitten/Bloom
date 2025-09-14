import Bloom.Frontend

-- MARK: Charecters

inductive LexChar where
  | openCurly
  | closeCurly
  | openSquare
  | closeSquare
  | openParens
  | closeParens
  | backSlash
  | doubleQuote
  | hashtag
  | newLine
  | inlineWhiteSpace : Char -> LexChar
  | comma
  | semicolon
  | symbolic : Char -> LexChar
  | underscore
  | digit : Fin 10 -> LexChar
  | lowercase : Char -> LexChar
  | uppercase : Char -> LexChar
  deriving Repr

inductive LexerError
  | unclosedString
  | invalidEscape : LexChar -> LexerError
  | unclosedCurly
  | unexpectedCloseCurly
  | expectedFound : String -> LexChar -> LexerError
  | invalidSnake : LexChar -> LexerError
  | invalidPascal : LexChar -> LexerError
  | unexpectedEOS
  | reservedCharecter : Char -> LexerError
  | unclassifiableCharecter : Char -> LexerError
  deriving Repr

def classify: Char -> Except LexerError LexChar := fun
  | '{'  => return .openCurly
  | '}'  => return .closeCurly
  | '['  => return .openSquare
  | ']'  => return .closeSquare
  | '('  => return .openParens
  | ')'  => return .closeParens
  | '\\' => return .backSlash
  | '"'  => return .doubleQuote
  | '#'  => return .hashtag
  | '\n' => return .newLine
  | ','  => return .comma
  | ';'  => return .semicolon
  | '_'  => return .underscore
  | '0'  => return .digit 0
  | '1'  => return .digit 1
  | '2'  => return .digit 2
  | '3'  => return .digit 3
  | '4'  => return .digit 4
  | '5'  => return .digit 5
  | '6'  => return .digit 6
  | '7'  => return .digit 7
  | '8'  => return .digit 7
  | '9'  => return .digit 8
  | '<'  => return .symbolic '<'
  | '>'  => return .symbolic '>'
  | '^'  => return .symbolic '^'
  | '$'  => return .symbolic '$'
  | '%'  => return .symbolic '%'
  | '*'  => return .symbolic '*'
  | '+'  => return .symbolic '+'
  | '-'  => return .symbolic '-'
  | '/'  => return .symbolic '/'
  | '~'  => return .symbolic '~'
  | '|'  => return .symbolic '|'
  | '&'  => return .symbolic '&'
  | '='  => return .symbolic '='
  | '.'  => return .symbolic '.'
  | ':'  => return .symbolic ':'
  | '!'  => return .symbolic '!'
  | '?'  => return .symbolic '?'
  | '@'  => throw <| .reservedCharecter '@'
  | '`'  => throw <| .reservedCharecter '`'
  | c => if c.isWhitespace then
    return .inlineWhiteSpace c
  else if c.isLower then
    return .lowercase c
  else if c.isUpper then
    return .uppercase c
  else
    throw <| .unclassifiableCharecter c

def LexChar.print : LexChar -> Char := fun
  | .openCurly => '{'
  | .closeCurly => '}'
  | .openSquare => '['
  | .closeSquare => ']'
  | .openParens => '('
  | .closeParens => ')'
  | .backSlash => '\\'
  | .doubleQuote => '"'
  | .hashtag => '#'
  | .newLine => '\n'
  | .inlineWhiteSpace c => c
  | .comma => ','
  | .semicolon => ';'
  | .symbolic c => c
  | .underscore => '_'
  | .digit 0 => '0'
  | .digit 1 => '1'
  | .digit 2 => '2'
  | .digit 3 => '3'
  | .digit 4 => '4'
  | .digit 5 => '5'
  | .digit 6 => '6'
  | .digit 7 => '7'
  | .digit 8 => '8'
  | .digit 9 => '9'
  | .lowercase c => c
  | .uppercase c => c

-- MARK: Lexer Monad

inductive LexerToken
  | indentation : String -> LexerToken
  | whiteSpace
  | symbolic : String -> LexerToken
  | snake : String -> LexerToken
  | pascal : String -> LexerToken
  | openCurly
  | closeCurly
  | decorator
  | openSquare
  | closeSquare
  | openParens
  | closeParens
  | comma
  | semicolon
  | documentationComment : String -> LexerToken
  | stringLiteral : String -> LexerToken
  | stringInterpolateStart : String -> LexerToken
  | StringInterpolateMiddle : String -> LexerToken
  | stringInterpolateEnd : String -> LexerToken
  | natural : Nat -> LexerToken
  deriving Repr

structure LexerEffect (a : Type) where
  tokens: Except (Position × LexerError) (Array (Located LexerToken) × a)

instance : Monad LexerEffect where
  pure x := LexerEffect.mk <| return (#[], x)
  bind mx f := LexerEffect.mk <| do
    let (ts, x) <- mx.tokens
    let (ts', y) <- (f x).tokens
    return (ts ++ ts', y)

abbrev PositionalLexerEffect := ReaderT (Position × Position) LexerEffect

def beforeChar : PositionalLexerEffect Position := do
  let (before, _) <- ReaderT.read
  return before

def afterChar : PositionalLexerEffect Position := do
  let (_, after) <- ReaderT.read
  return after

-- MARK: Position

def Position.recordCharWidth
    (pos : Position)
    (n : Nat)
    : Position
  := Position.mk pos.line (pos.char + n)

def Position.update
    (pos : Position)
    (i : EditorInfo)
    (c : LexChar)
    : Position
  := match c with
  | .newLine => Position.mk (pos.line + 1) 0
  | .inlineWhiteSpace '\t' => pos.recordCharWidth i.tabSize
  | .inlineWhiteSpace '\r' => pos.recordCharWidth 0
  | _ => pos.recordCharWidth 1

-- MARK: Lexer

inductive StringStart
  | regular
  | fromInterpolation

def StringStart.endRegular : StringStart -> String -> LexerToken := fun
  | .regular => .stringLiteral
  | .fromInterpolation => .stringInterpolateEnd

def StringStart.endInterpolate : StringStart -> String -> LexerToken := fun
  | .regular => .stringInterpolateStart
  | .fromInterpolation => .StringInterpolateMiddle

inductive CommentFlavour
  | regular
  | documentation

inductive LexerState
  | string : StringStart -> String -> LexerState
  | stringEscaped : StringStart -> String -> LexerState
  | indentation : String -> LexerState
  | hashtag
  | hashtagVertibar
  | comment : CommentFlavour -> String -> LexerState
  | symbolic : String -> LexerState
  | snake : String -> LexerState
  | pascal : String -> LexerState
  | number : Array (Fin 10) -> LexerState
  | whiteSpace
  | waiting

inductive ContextLayer
  | openCurly
  | interpolation

structure Lexer where
  context : List ContextLayer
  tokenStart : Position
  state : LexerState

def failLexingAt
    (err : LexerError)
    (pos : Position)
    : LexerEffect a
  := LexerEffect.mk <| .error (pos, err)

def failLexing
    (err : LexerError)
    : PositionalLexerEffect a := do
  LexerEffect.mk <| .error (<- beforeChar, err)

def exceptAt
    (exc : Except LexerError a)
    (pos : Position)
    : LexerEffect a := do match exc with
  | .ok x => return x
  | .error err => failLexingAt err pos

/--
  Yields a token with span start given by the lexer and span end **exclusive** of the current position.
-/
def Lexer.yieldToken
    (lexer : Lexer)
    (tok : LexerToken)
    : PositionalLexerEffect Unit := do
  LexerEffect.mk <| .ok (#[Located.mk (Span.mk lexer.tokenStart (<- beforeChar)) tok], ())

/--
  Yields a token with span start given by the lexer and span end **includive** of the current position.
-/
def Lexer.yieldTokenInclusive
    (lexer : Lexer)
    (tok : LexerToken)
    : PositionalLexerEffect Unit := do
  LexerEffect.mk <| .ok (#[Located.mk (Span.mk lexer.tokenStart (<- afterChar)) tok], ())

def yieldSingletonToken
    (tok : LexerToken)
    : PositionalLexerEffect Unit := do
  LexerEffect.mk <| .ok (#[Located.mk (Span.mk (<- beforeChar) (<- afterChar)) tok], ())

def Lexer.close (lexer : Lexer) : PositionalLexerEffect Unit := do
  match lexer.state with
    | .string _ _ => failLexing .unclosedString
    | .stringEscaped _ _ => failLexing .unclosedString
    | .whiteSpace => lexer.yieldToken <| .whiteSpace
    | .indentation wscs => lexer.yieldToken <| .indentation wscs
    | .hashtag => failLexing .unexpectedEOS
    | .hashtagVertibar => failLexing .unexpectedEOS
    | .comment flavour com => match flavour with
      | .regular => return ()
      | .documentation => lexer.yieldToken <| .documentationComment com
    | .symbolic symb => lexer.yieldToken <| .symbolic symb
    | .snake snek => lexer.yieldToken <| .snake snek
    | .pascal pasc => lexer.yieldToken <| .pascal pasc
    | .number digs => lexer.yieldToken <| .natural <| Array.foldl (fun n d => 10*n + d.toNat) 0 digs
    | .waiting => return ()

def Lexer.updateState
    (lexer : Lexer)
    : LexerState -> Lexer
  := Lexer.mk lexer.context lexer.tokenStart

/--
  Creates a new state, **inclusive** of the charecter at the current position.
-/
def Lexer.newState
    (lexer : Lexer)
    (state : LexerState)
    : PositionalLexerEffect Lexer := do
  return Lexer.mk lexer.context (<- beforeChar) state

/--
  Creates a new state, **exclusive** of the charecter at the current position.
-/
def Lexer.newStateExclusive
    (lexer : Lexer)
    (state : LexerState)
    : PositionalLexerEffect Lexer := do
  return Lexer.mk lexer.context (<- afterChar) state

def Lexer.eos (lexer : Lexer) : PositionalLexerEffect Unit := match lexer.context with
  | .openCurly :: _ => failLexing .unclosedCurly
  | .interpolation :: _ => failLexing .unclosedCurly
  | [] => lexer.close

-- MARK: State Machine

def Lexer.update
    (lexer : Lexer)
    (c : LexChar)
    : PositionalLexerEffect Lexer := do match lexer.state, c with

  --| STRING LITERALS
  -- handle escape charecters and the start of interpolation

  | .stringEscaped start str, .lowercase 't' =>
    return lexer.updateState <| .string start <| str.push '\t'

  | .stringEscaped start str, .lowercase 'r' =>
    return lexer.updateState <| .string start <| str.push '\r'

  | .stringEscaped start str, .lowercase 'n' =>
    return lexer.updateState <| .string start <| str.push '\n'

  | .stringEscaped start str, .newLine =>
    return lexer.updateState <| .string start <| str.push '\n'

  | .stringEscaped start str, .backSlash =>
    return lexer.updateState <| .string start str

  | .stringEscaped start str, .openParens =>
    return lexer.updateState <| .string start <| str.push '{'

  | .stringEscaped start str, .doubleQuote =>
    return lexer.updateState <| .string start <| str.push '"'

  | .stringEscaped _ _, c =>
    failLexing <| .invalidEscape c

  | .string start str, .backSlash =>
    return lexer.updateState (.stringEscaped start str)

  | .string start str, .doubleQuote =>
    lexer.yieldTokenInclusive <| start.endRegular str
    lexer.newStateExclusive .waiting

  | .string start str, .openCurly =>
    lexer.yieldTokenInclusive <| start.endInterpolate str
    return Lexer.mk (.interpolation :: lexer.context) (<- afterChar) .waiting

  | .string start str, c =>
    return lexer.updateState <| .stringEscaped start <| str.push c.print

  --| HASHTAG
  -- hashtag syntax finder
  -- I should probably make this nicer if more hashtag syntax is made
  -- but for now this will do

  | .hashtag, .inlineWhiteSpace _ =>
    return lexer.updateState <| .comment .regular ""

  | .hashtag, .openSquare =>
    lexer.yieldToken <| .decorator
    return lexer.updateState .waiting

  | .hashtag, .symbolic '|' =>
    return lexer.updateState <| .hashtagVertibar

  | .hashtag, c =>
    failLexing <| .expectedFound "whitespace, '[' or '|'" c

  | .hashtagVertibar, .inlineWhiteSpace ' ' =>
    return lexer.updateState <| .comment .documentation ""

  | .hashtagVertibar, c =>
    failLexing <| .expectedFound "' '" c

  --| INDENTATION HANDLING

  | .indentation wscs, .inlineWhiteSpace c =>
    return lexer.updateState <| .indentation (wscs.push c)

  --| COMMENTS
  -- single lines only

  | .comment flavour com, c =>
    return lexer.updateState <| .comment flavour (com.push c.print)

  --| SYMBOLIC NAMES
  -- uniform requirements
  -- can touch snake and camel case

  | .symbolic symb, .symbolic c =>
    return lexer.updateState <| .symbolic (symb.push c)

  | .symbolic symb, .backSlash =>
    return lexer.updateState <| .symbolic (symb.push '\\')

  --| SNAKE CASE
  -- starts with lower case
  -- cant have capitals

  | .snake snek, .lowercase c =>
    return lexer.updateState <| .snake (snek.push c)

  | .snake snek, .underscore =>
    return lexer.updateState <| .snake (snek.push '_')

  | .snake _, .uppercase _ =>
    failLexing <| .invalidSnake c

  | .snake snek, .digit 0 =>
    return lexer.updateState <| .snake (snek.push '0')

  | .snake snek, .digit 1 =>
    return lexer.updateState <| .snake (snek.push '1')

  | .snake snek, .digit 2 =>
    return lexer.updateState <| .snake (snek.push '2')

  | .snake snek, .digit 3 =>
    return lexer.updateState <| .snake (snek.push '3')

  | .snake snek, .digit 4 =>
    return lexer.updateState <| .snake (snek.push '4')

  | .snake snek, .digit 5 =>
    return lexer.updateState <| .snake (snek.push '5')

  | .snake snek, .digit 6 =>
    return lexer.updateState <| .snake (snek.push '6')

  | .snake snek, .digit 7 =>
    return lexer.updateState <| .snake (snek.push '7')

  | .snake snek, .digit 8 =>
    return lexer.updateState <| .snake (snek.push '8')

  | .snake snek, .digit 9 =>
    return lexer.updateState <| .snake (snek.push '9')

  --| PASCAL CASE
  -- starts with upper case
  -- cant have underscores

  | .pascal pasc, .lowercase c =>
    return lexer.updateState <| .pascal (pasc.push c)

  | .pascal pasc, .uppercase c =>
    return lexer.updateState <| .pascal (pasc.push c)

  | .pascal _, .underscore =>
    failLexing <| .invalidPascal c

  | .pascal pasc, .digit 0 =>
    return lexer.updateState <| .pascal (pasc.push '0')

  | .pascal pasc, .digit 1 =>
    return lexer.updateState <| .pascal (pasc.push '1')

  | .pascal pasc, .digit 2 =>
    return lexer.updateState <| .pascal (pasc.push '2')

  | .pascal pasc, .digit 3 =>
    return lexer.updateState <| .pascal (pasc.push '3')

  | .pascal pasc, .digit 4 =>
    return lexer.updateState <| .pascal (pasc.push '4')

  | .pascal pasc, .digit 5 =>
    return lexer.updateState <| .pascal (pasc.push '5')

  | .pascal pasc, .digit 6 =>
    return lexer.updateState <| .pascal (pasc.push '6')

  | .pascal pasc, .digit 7 =>
    return lexer.updateState <| .pascal (pasc.push '7')

  | .pascal pasc, .digit 8 =>
    return lexer.updateState <| .pascal (pasc.push '8')

  | .pascal pasc, .digit 9 =>
    return lexer.updateState <| .pascal (pasc.push '9')

  --| NUMBER
  -- natural number syntax

  | .number digits, .digit n =>
    return lexer.updateState <| .number (digits.push n)

  | .number _, .uppercase _ =>
    failLexing <| .invalidSnake c

  --| WHITESPACE
  -- don't track whitespace multiple times

  | .whiteSpace, .inlineWhiteSpace _ =>
    return lexer.updateState .whiteSpace

  --| DEFAULTS

  | _, c =>
    lexer.close
    match c with
      --| Whitespace
      | .inlineWhiteSpace _ => lexer.newState .whiteSpace
      | .newLine            => lexer.newStateExclusive <| .indentation ""

      --| Identifiers
      | .symbolic symb  => lexer.newState <| .symbolic symb.toString
      | .backSlash      => lexer.newState <| .symbolic "\\"
      | .lowercase snek => lexer.newState <| .snake snek.toString
      | .underscore     => lexer.newState <| .snake "_"
      | .uppercase pasc => lexer.newState <| .pascal pasc.toString

      --| Curly Brackets
      | .openCurly => yieldSingletonToken .openCurly
        *> return Lexer.mk (.openCurly :: lexer.context) (<- afterChar) .waiting
      | .closeCurly => match lexer.context with
        | .openCurly     :: context' => yieldSingletonToken .closeCurly
          *> return Lexer.mk context' (<- afterChar) .waiting
        | .interpolation :: context' =>
          return Lexer.mk context' (<- afterChar) <| .string .fromInterpolation ""
        | [] => failLexing .unexpectedCloseCurly

      --| Squares and Parens
      | .openSquare  => yieldSingletonToken .openSquare  *> lexer.newStateExclusive .waiting
      | .closeSquare => yieldSingletonToken .closeSquare *> lexer.newStateExclusive .waiting
      | .openParens  => yieldSingletonToken .openParens  *> lexer.newStateExclusive .waiting
      | .closeParens => yieldSingletonToken .closeParens *> lexer.newStateExclusive .waiting

      --| Literals
      | .doubleQuote => lexer.newState <| .string .regular ""
      | .digit d     => lexer.newState <| .number #[d]

      --| Syntax
      | .hashtag   =>                                   lexer.newState .hashtag
      | .comma     => yieldSingletonToken .comma     *> lexer.newStateExclusive .waiting
      | .semicolon => yieldSingletonToken .semicolon *> lexer.newStateExclusive .waiting

-- MARK: Final Step

def lex
    (i : EditorInfo)
    (input : String)
    : Except (Position × LexerError) (Array <| Located LexerToken) := (do
  let mut pos : Position := {
    line := 0
    char := 0
  }
  let mut prev_pos := pos
  let mut lexer : Lexer := {
    context := []
    tokenStart := pos
    state := .indentation ""
  }
  for c in input.toList do
    let lc <- exceptAt (classify c) pos
    pos := pos.update i lc
    lexer <- lexer.update lc (prev_pos, pos)
    prev_pos := pos
  lexer.eos (prev_pos, pos)
).tokens <&> fun (ts, _) => ts
