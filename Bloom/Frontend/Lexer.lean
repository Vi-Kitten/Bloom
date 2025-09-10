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
  tokens: Except LexerError (Array LexerToken × a)

instance : Monad LexerEffect where
  pure x := LexerEffect.mk <| return (#[], x)
  bind mx f := LexerEffect.mk <| do
    let (ts, x) <- mx.tokens
    let (ts', y) <- (f x).tokens
    return (ts ++ ts', y)

def liftLexerError (mx: Except LexerError a): LexerEffect a
  := LexerEffect.mk <| return (#[], <- mx)

instance : MonadLift (Except LexerError) LexerEffect where
  monadLift := liftLexerError

def failLexing
    (err: LexerError)
    : LexerEffect a
  := LexerEffect.mk <| .error err

def yieldToken
    (tok: LexerToken)
    : LexerEffect Unit
  := LexerEffect.mk <| .ok (#[tok], ())

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

def LexerState.close : LexerState -> LexerEffect Unit := fun
  | .string _ _ => failLexing .unclosedString
  | .stringEscaped _ _ => failLexing .unclosedString
  | .whiteSpace => yieldToken <| .whiteSpace
  | .indentation wscs => yieldToken <| .indentation wscs
  | .hashtag => failLexing .unexpectedEOS
  | .hashtagVertibar => failLexing .unexpectedEOS
  | .comment flavour com => match flavour with
    | .regular => return ()
    | .documentation => yieldToken <| .documentationComment com
  | .symbolic symb => yieldToken <| .symbolic symb
  | .snake snek => yieldToken <| .snake snek
  | .pascal pasc => yieldToken <| .pascal pasc
  | .number digs => yieldToken <| .natural <| Array.foldl (fun n d => 10*n + d.toNat) 0 digs
  | .waiting => return ()

inductive ContextLayer
  | openCurly
  | interpolation

structure Lexer where
  context : List ContextLayer
  state : LexerState

def Lexer.eos (lexer : Lexer) : LexerEffect Unit := match lexer.context with
  | .openCurly :: _ => failLexing .unclosedCurly
  | .interpolation :: _ => failLexing .unclosedCurly
  | [] => lexer.state.close

-- MARK: State Machine

def update
    (c : LexChar)
    (lexer : Lexer)
    : LexerEffect Lexer := do match lexer.state, c with

  --| STRING LITERALS
  -- handle escape charecters and the start of interpolation

  | .stringEscaped start str, .lowercase 't' =>
    return Lexer.mk lexer.context (.string start <| str.push '\t')

  | .stringEscaped start str, .lowercase 'r' =>
    return Lexer.mk lexer.context (.string start <| str.push '\r')

  | .stringEscaped start str, .lowercase 'n' =>
    return Lexer.mk lexer.context (.string start <| str.push '\n')

  | .stringEscaped start str, .newLine =>
    return Lexer.mk lexer.context (.string start <| str.push '\n')

  | .stringEscaped start str, .backSlash =>
    return Lexer.mk lexer.context (.string start str)

  | .stringEscaped start str, .openParens =>
    return Lexer.mk lexer.context (.string start <| str.push '{')

  | .stringEscaped start str, .doubleQuote =>
    return Lexer.mk lexer.context (.string start <| str.push '"')

  | .stringEscaped _ _, c =>
    failLexing <| .invalidEscape c

  | .string start str, .backSlash =>
    return Lexer.mk lexer.context (.stringEscaped start str)

  | .string start str, .doubleQuote =>
    yieldToken <| start.endRegular str
    return Lexer.mk lexer.context .waiting

  | .string start str, .openCurly =>
    yieldToken <| start.endInterpolate str
    return Lexer.mk (.interpolation :: lexer.context) .waiting

  | .string start str, c =>
    return Lexer.mk lexer.context (.stringEscaped start <| str.push c.print)

  --| HASHTAG
  -- hashtag syntax finder
  -- I should probably make this nicer if more hashtag syntax is made
  -- but for now this will do

  | .hashtag, .inlineWhiteSpace _ =>
    return Lexer.mk lexer.context <| .comment .regular ""

  | .hashtag, .openSquare =>
    yieldToken <| .decorator
    return Lexer.mk lexer.context .waiting

  | .hashtag, .symbolic '|' =>
    return Lexer.mk lexer.context <| .hashtagVertibar

  | .hashtag, c =>
    failLexing <| .expectedFound "whitespace, '[' or '|'" c

  | .hashtagVertibar, .inlineWhiteSpace ' ' =>
    return Lexer.mk lexer.context <| .comment .documentation ""

  | .hashtagVertibar, c =>
    failLexing <| .expectedFound "' '" c

  --| INDENTATION HANDLING

  | .indentation wscs, .inlineWhiteSpace c =>
    return Lexer.mk lexer.context <| .indentation (wscs.push c)

  --| COMMENTS
  -- single lines only

  | .comment flavour com, c =>
    return Lexer.mk lexer.context <| .comment flavour (com.push c.print)

  --| SYMBOLIC NAMES
  -- uniform requirements
  -- can touch snake and camel case

  | .symbolic symb, .symbolic c =>
    return Lexer.mk lexer.context <| .symbolic (symb.push c)

  | .symbolic symb, .backSlash =>
    return Lexer.mk lexer.context <| .symbolic (symb.push '\\')

  --| SNAKE CASE
  -- starts with lower case
  -- cant have capitals

  | .snake snek, .lowercase c =>
    return Lexer.mk lexer.context <| .snake (snek.push c)

  | .snake snek, .underscore =>
    return Lexer.mk lexer.context <| .snake (snek.push '_')

  | .snake _, .uppercase _ =>
    failLexing <| .invalidSnake c

  | .snake snek, .digit 0 =>
    return Lexer.mk lexer.context <| .snake (snek.push '0')

  | .snake snek, .digit 1 =>
    return Lexer.mk lexer.context <| .snake (snek.push '1')

  | .snake snek, .digit 2 =>
    return Lexer.mk lexer.context <| .snake (snek.push '2')

  | .snake snek, .digit 3 =>
    return Lexer.mk lexer.context <| .snake (snek.push '3')

  | .snake snek, .digit 4 =>
    return Lexer.mk lexer.context <| .snake (snek.push '4')

  | .snake snek, .digit 5 =>
    return Lexer.mk lexer.context <| .snake (snek.push '5')

  | .snake snek, .digit 6 =>
    return Lexer.mk lexer.context <| .snake (snek.push '6')

  | .snake snek, .digit 7 =>
    return Lexer.mk lexer.context <| .snake (snek.push '7')

  | .snake snek, .digit 8 =>
    return Lexer.mk lexer.context <| .snake (snek.push '8')

  | .snake snek, .digit 9 =>
    return Lexer.mk lexer.context <| .snake (snek.push '9')

  --| PASCAL CASE
  -- starts with upper case
  -- cant have underscores

  | .pascal pasc, .lowercase c =>
    return Lexer.mk lexer.context <| .pascal (pasc.push c)

  | .pascal pasc, .uppercase c =>
    return Lexer.mk lexer.context <| .pascal (pasc.push c)

  | .pascal _, .underscore =>
    failLexing <| .invalidPascal c

  | .pascal pasc, .digit 0 =>
    return Lexer.mk lexer.context <| .pascal (pasc.push '0')

  | .pascal pasc, .digit 1 =>
    return Lexer.mk lexer.context <| .pascal (pasc.push '1')

  | .pascal pasc, .digit 2 =>
    return Lexer.mk lexer.context <| .pascal (pasc.push '2')

  | .pascal pasc, .digit 3 =>
    return Lexer.mk lexer.context <| .pascal (pasc.push '3')

  | .pascal pasc, .digit 4 =>
    return Lexer.mk lexer.context <| .pascal (pasc.push '4')

  | .pascal pasc, .digit 5 =>
    return Lexer.mk lexer.context <| .pascal (pasc.push '5')

  | .pascal pasc, .digit 6 =>
    return Lexer.mk lexer.context <| .pascal (pasc.push '6')

  | .pascal pasc, .digit 7 =>
    return Lexer.mk lexer.context <| .pascal (pasc.push '7')

  | .pascal pasc, .digit 8 =>
    return Lexer.mk lexer.context <| .pascal (pasc.push '8')

  | .pascal pasc, .digit 9 =>
    return Lexer.mk lexer.context <| .pascal (pasc.push '9')

  --| NUMBER
  -- natural number syntax

  | .number digits, .digit n =>
    return Lexer.mk lexer.context <| .number (digits.push n)

  | .number _, .uppercase _ =>
    failLexing <| .invalidSnake c

  --| WHITESPACE
  -- don't track whitespace multiple times

  | .whiteSpace, .inlineWhiteSpace _ =>
    return Lexer.mk lexer.context .whiteSpace

  --| DEFAULTS

  | state, c =>
    state.close
    match c with
      --| Whitespace
      | .inlineWhiteSpace _ => return Lexer.mk lexer.context .whiteSpace
      | .newLine => return Lexer.mk lexer.context <| .indentation ""

      --| Identifiers
      | .symbolic symb => return Lexer.mk lexer.context <| .symbolic symb.toString
      | .backSlash => return Lexer.mk lexer.context <| .symbolic "\\"
      | .lowercase snek => return Lexer.mk lexer.context <| .snake snek.toString
      | .underscore => return Lexer.mk lexer.context <| .snake "_"
      | .uppercase pasc => return Lexer.mk lexer.context <| .pascal pasc.toString

      --| Curly Brackets
      | .openCurly => yieldToken .openCurly *> return Lexer.mk (.openCurly :: lexer.context) .waiting
      | .closeCurly => match lexer.context with
        | .openCurly :: context' => yieldToken .closeCurly *> return Lexer.mk context' .waiting
        | .interpolation :: context' => return Lexer.mk context' <| .string .fromInterpolation ""
        | [] => failLexing .unexpectedCloseCurly

      --| Squares and Parens
      | .openSquare => yieldToken .openSquare *> return Lexer.mk lexer.context .waiting
      | .closeSquare => yieldToken .closeSquare *> return Lexer.mk lexer.context .waiting
      | .openParens => yieldToken .openParens *> return Lexer.mk lexer.context .waiting
      | .closeParens => yieldToken .closeParens *> return Lexer.mk lexer.context .waiting

      --| Literals
      | .doubleQuote => return Lexer.mk lexer.context <| .string .regular ""
      | .digit d => return Lexer.mk lexer.context <| .number #[d]

      --| Syntax
      | .hashtag => return Lexer.mk lexer.context .hashtag
      | .comma => yieldToken .comma *> return Lexer.mk lexer.context .waiting
      | .semicolon => yieldToken .semicolon *> return Lexer.mk lexer.context .waiting

-- MARK: Final Step

def initialLexer : Lexer where
  context := []
  state := .indentation ""

def lex (input : String) : Except LexerError (Array LexerToken) := (do
  let mut lexer : Lexer := initialLexer
  for c in input.toList do
    let lc <- liftLexerError <| classify c
    lexer <- update lc lexer
  lexer.eos
).tokens <&> fun (ts, _) => ts
