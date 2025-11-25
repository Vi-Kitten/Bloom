{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Frontend.Lexer (
    Token,
    LexerError,
    lexer
) where

import Data.Set (Set, fromList, member)
import Data.Function ((&))
import Data.Char (isUpper, isLower, isSpace)
import Frontend (EditorInfo (..))
import GHC.TypeLits (Nat)
import Parser.Spanned (TextPos (..), Span (..), Spanned (..), coalesce, coalesceWith)
import Parser (ParserT, expect, Parser, ParseError, runParser)
import GHC.Base (Alternative (..), many)
import Data.List.NonEmpty (NonEmpty (..), toList, some1)
import Control.Arrow ((>>>))
import Data.Functor ((<&>), ($>))
import Parser.Combinators (expectEqualSpanned, expectNotequalSpanned, expectPredSpanned)
import Control.Monad (join)
import Data.Maybe (maybeToList)
import Data.Bifunctor (Bifunctor (..))
import Reporting (PoisonID, CompilerExcept, raise, raiseInit)

symbolicCharecters :: Set Char
symbolicCharecters = fromList "!$%^&*-+=:@~|<>?./"

data ClassifiedChar
    = Escape
    | Upper Char
    | Lower Char
    | Symbolic Char
    | White Char
    | Digit0
    | Digit1
    | Digit2
    | Digit3
    | Digit4
    | Digit5
    | Digit6
    | Digit7
    | Digit8
    | Digit9
    | Quote
    | OpenCurly
    | CloseCurly
    | Singleton Char
    | LineStart
    deriving Eq

classify :: Char -> ClassifiedChar
classify '\\' = Escape
classify '"' = Quote
classify '{' = OpenCurly
classify '}' = CloseCurly
classify '0' = Digit0
classify '1' = Digit1
classify '2' = Digit2
classify '3' = Digit3
classify '4' = Digit4
classify '5' = Digit5
classify '6' = Digit6
classify '7' = Digit7
classify '8' = Digit8
classify '9' = Digit9
classify c
  | symbolicCharecters & member c = Symbolic c
  | isUpper c = Upper c
  | isLower c = Lower c
  | isSpace c = White c
  | otherwise = Singleton c

asChar :: ClassifiedChar -> Char
asChar Escape = '\\'
asChar (Upper c) = c
asChar (Lower c) = c
asChar (Symbolic c) = c
asChar (White c) = c
asChar Digit0 = '0'
asChar Digit1 = '1'
asChar Digit2 = '2'
asChar Digit3 = '3'
asChar Digit4 = '4'
asChar Digit5 = '5'
asChar Digit6 = '6'
asChar Digit7 = '7'
asChar Digit8 = '8'
asChar Digit9 = '9'
asChar Quote = '"'
asChar OpenCurly = '{'
asChar CloseCurly = '}'
asChar LineStart = '\n'
asChar (Singleton c) = c

widthOf :: EditorInfo -> Char -> Nat
widthOf _ '\r' = 0
widthOf info '\t' = info & tabSize
widthOf _ _ = 1

prepareLine :: EditorInfo -> Nat -> String -> [Spanned ClassifiedChar]
prepareLine i l str = Span (TextPos l 0) (TextPos l 0) :@ LineStart : prepareLineInner 0 str
    where
        prepareLineInner _ "" = []
        prepareLineInner n (c : cs) = do
            let n' = n + widthOf i c
            Span (TextPos l n) (TextPos l n') :@ classify c : prepareLineInner n' cs

prepareLines :: EditorInfo -> [String] -> [Spanned ClassifiedChar]
prepareLines i ls = zip [0..] ls >>= uncurry (prepareLine i)

data RawToken
    = RawCamelName String
    | RawSnakeName String
    | RawSymbolicName String
    | RawNatural Nat
    | RawSmallString String
    | RawStringStart String
    | RawStringMiddle String
    | RawStringEnd String
    | RawIndentation String
    | RawDocComment String
    | RawElementary Char
    | DecoratorStart -- #[
    | WhiteSpace

-- TODO: define expectation type
type Expectation = ()

expectSingleton c = expect $ \case
    s :@ Singleton c' -> if c == c'
        then Right $ s :@ c'
        else Left ()
    _ -> Left ()

expectWhite = expect $ \case
    s :@ White c -> Right $ s :@ c
    _ -> Left ()

expectUpper = expect $ \case
    s :@ Upper c -> Right $ s :@ c
    _ -> Left ()

expectLower = expect $ \case
    s :@ Lower c -> Right $ s :@ c
    _ -> Left ()

expectExtendedSymbolic = expect $ \case
    s :@ Escape -> Right $ s :@ '\\'
    s :@ Symbolic c -> Right $ s :@ c
    _ -> Left ()

expectEscape = expect $ \case
    s :@ Escape -> Right $ s :@ '\\'
    _ -> Left ()

expectQuote = expect $ \case
    s :@ Quote -> Right $ s :@ '\"'
    _ -> Left ()

expectOpenCurly = expect $ \case
    s :@ OpenCurly -> Right $ s :@ '{'
    _ -> Left ()

expectCloseCurly = expect $ \case
    s :@ CloseCurly -> Right $ s :@ '}'
    _ -> Left ()

expectLineStart = expect $ \case
    s :@ LineStart -> Right $ s :@ '\n'
    _ -> Left ()

expectDigitChar = expect $ \case
    s :@ Digit0 -> Right $ s :@ '0'
    s :@ Digit1 -> Right $ s :@ '1'
    s :@ Digit2 -> Right $ s :@ '2'
    s :@ Digit3 -> Right $ s :@ '3'
    s :@ Digit4 -> Right $ s :@ '4'
    s :@ Digit5 -> Right $ s :@ '5'
    s :@ Digit6 -> Right $ s :@ '6'
    s :@ Digit7 -> Right $ s :@ '7'
    s :@ Digit8 -> Right $ s :@ '8'
    s :@ Digit9 -> Right $ s :@ '9'
    _ -> Left ()

expectDigit :: ParserT (Spanned ClassifiedChar) () m (Spanned Nat)
expectDigit = expect $ \case
    s :@ Digit0 -> Right $ s :@ 0
    s :@ Digit1 -> Right $ s :@ 1
    s :@ Digit2 -> Right $ s :@ 2
    s :@ Digit3 -> Right $ s :@ 3
    s :@ Digit4 -> Right $ s :@ 4
    s :@ Digit5 -> Right $ s :@ 5
    s :@ Digit6 -> Right $ s :@ 6
    s :@ Digit7 -> Right $ s :@ 7
    s :@ Digit8 -> Right $ s :@ 8
    s :@ Digit9 -> Right $ s :@ 9
    _ -> Left ()

expectEscapedChar :: Parser (Spanned ClassifiedChar) Expectation (Spanned Char)
expectEscapedChar = do
    s :@ _ <- expectEscape
    s' :@ c <- expectEscape
        <|> expectQuote
        <|> expectOpenCurly
        <|> (expectEqualSpanned (Lower 'n') <&> (<$) '\n')
        <|> (expectEqualSpanned (Lower 't') <&> (<$) '\t')
        <|> (expectEqualSpanned (Lower 'r') <&> (<$) '\r')
    return $ (s <> s') :@ c

expectStringChar :: Parser (Spanned ClassifiedChar) Expectation (Spanned Char)
expectStringChar = (expectPredSpanned (\c -> c /= OpenCurly && c /= Quote) <&> fmap asChar)
    <|> expectEscapedChar

type TokenParser = Parser (Spanned ClassifiedChar) Expectation (Spanned RawToken)

lexCamel, lexSnake, lexSymbol, lexIndentation, lexWhiteSpace, lexNumber, lexToken :: TokenParser

lexCamel = do
    c <- expectUpper
    cs <- many (expectLower <|> expectDigitChar <|> expectUpper)
    return $ coalesce (c :| cs) <&> (toList >>> RawCamelName)

lexSnake = do
    c <- expectLower <|> expectSingleton '_'
    cs <- many (expectLower <|> expectDigitChar <|> expectSingleton '_')
    return $ coalesce (c :| cs) <&> (toList >>> RawSnakeName)

lexSymbol = some1 expectExtendedSymbolic <&> (coalesce >>> fmap toList >>> fmap RawSymbolicName)

lexWhiteSpace = some1 expectWhite <&> (coalesce >>> ($> WhiteSpace))

lexNumber = some1 expectDigit <&> (coalesce >>> fmap (foldl1 (\x y -> (x * 10) + y) >>> RawNatural))

-- indentation at start of line
lexIndentation = do
    (s :@ _) <- expectLineStart
    cs <- many expectWhite
    return $ coalesceWith s cs <&> RawIndentation

-- lex a normal thing
lexToken = lexWhiteSpace
    <|> lexCamel
    <|> lexSnake
    <|> lexSymbol
    <|> lexNumber
    <|> lexIndentation

-- decorator syntax and single line comments
lexHashtagGroup :: Parser (Spanned ClassifiedChar) Expectation (Maybe (Spanned RawToken))
lexHashtagGroup = expectSingleton '#' >>= \(s :@ _) ->
    (expectSingleton '|'
        *> expectWhite
        *> many (expectNotequalSpanned LineStart)
        <&> (coalesceWith s >>> fmap (fmap asChar) >>> fmap RawDocComment >>> Just)
    ) <|> (expectSingleton '['
        <&> \(s' :@ _) -> Just $ (s <> s') :@ DecoratorStart
    ) <|> (expectWhite
        *> many (expectNotequalSpanned LineStart)
        $> Nothing
    )

lexCurlySection :: Parser (Spanned ClassifiedChar) Expectation (NonEmpty (Spanned RawToken))
lexCurlySection = do
    s :@ _ <- expectOpenCurly
    toks <- lexTokens
    s' :@ _ <- expectCloseCurly
    return $ s :@ RawElementary '{' :| (toks ++ [s' :@ RawElementary '}'])

-- expecting '"'
lexString :: Parser (Spanned ClassifiedChar) Expectation (NonEmpty (Spanned RawToken))
lexString = do
    s :@ _ <- expectQuote
    s' :@ str <- many expectStringChar <&> coalesceWith s
    (expectQuote <&> \(s'' :@ _) -> (s' <> s'') :@ RawSmallString str :| []) <|> do
        s'' :@ _ <- expectOpenCurly
        toks <- lexTokens
        toks' <- lexStringContinuation
        return $ (s' <> s'') :@ RawStringStart str :| (toks ++ toList toks')

-- expecting '}'
lexStringContinuation :: Parser (Spanned ClassifiedChar) Expectation (NonEmpty (Spanned RawToken))
lexStringContinuation = do
    s :@ _ <- expectCloseCurly
    s' :@ str <- many expectStringChar <&> coalesceWith s
    (expectQuote <&> \(s'' :@ _) -> (s' <> s'') :@ RawStringEnd str :| []) <|> do
        s'' :@ _ <- expectOpenCurly
        toks <- lexTokens
        toks' <- lexStringContinuation
        return $ (s' <> s'') :@ RawStringMiddle str :| (toks ++ toList toks')


lexTokens :: Parser (Spanned ClassifiedChar) Expectation [Spanned RawToken]
lexTokens = fmap join $ many $ (lexToken <&> pure)
    <|> (lexHashtagGroup <&> maybeToList)
    <|> (lexCurlySection <&> toList)
    <|> (lexString <&> toList)
    <|> (expectNotequalSpanned CloseCurly <&> fmap (asChar >>> RawElementary) <&> pure)

camelKeyWords :: Set String
camelKeyWords = fromList [
    -- datatypes
        "Self",
    -- reservations
        "Super"
    ]

snakeKeyWords :: Set String
snakeKeyWords = fromList [
    -- imports
        "mod",
        "pub",
        "prot",
        "use",
        "as",
        "all",
    -- modifiers
        "static",
        "move",
        "inout",
        "mut",
        "pin",
        "ref",
        "pure",
    -- general
        "_", -- discard
        "def",
        "where",
        "for",
        "dyn",
        "in",
        "out",
        "todo", -- leave undefined
    -- datatypes
        "enum",
        "case",
        "struct",
    -- abstractions
        "self",
        "interface",
        "trait",
        "impl",
        "derive",
        "is",
        "super",
    -- control flow
        "let",
        "do",
        "with",
        "match",
        "or",
        "fold",
        "goto",
        "switch",
        "if",
        "else",
        "elif",
        "fn",
        "return",
        "yield",
        "break",
        "continue",
        "nobreak",
        "pass", -- placeholder action
        "throw",
        "try",
        "catch",
    -- reservations
        "class",
        "inherit",
        "mixin",
        "extend",
        "open",
        "close",
        "effect",
        "test",
        "axiom",
        "rule",
        "defer",
        "modality",
        "async",
        "await",
        "spawn",
        "par" -- linear dual of tuple for async environments
    ]

statementContinuationKeyWords :: Set String
statementContinuationKeyWords = fromList [
        "elif",
        "else",
        "catch",
        "with",
        "nobreak"
    ]

symbolicKeyWords :: Set String
symbolicKeyWords = fromList [
    -- function types
        "->",
        "~>",
        "-+",
        "-*",
    -- equation syntax
        ".",
        "?",
        "=",
        "?=",
        "=>",
    -- extras
        "|",
        ":",
        "::",
        "&",
        "@", -- jump label
        "...", -- typed hole
    -- reservations
        "<-",
        "<:", -- subtyping
        "|=", -- implicit tuple (dual to =>) for example: dyn[N] Num N |= N
        "~" -- linear consumer type, like T -* Unit for async environments
    ]

-- cannot be overriden, has special precedent
specialOperators :: Set String
specialOperators = fromList [
    -- numberic
        "+",
        "-",
        "*",
        "/",
    -- logical
        "&&",
        "||"
    ]

data Token
    = Keyword String
    | Snake String
    | Camel String
    | Symbol String
    | Indentation String
    | StringStart String
    | StringMiddle String
    | StringEnd String
    | SmallString String
    | Natural Nat
    | NaturalWithUnit Nat String
    | Decorator String
    | Documentation String
    | Elementary Char
    | Erronious PoisonID
    -- | ErroniousOpenCurly
    -- | ErroniousOpenParens
    deriving Show

-- fatal
data LexerError
    = UnclosedCurly TextPos
    | LexerParseError
    deriving Show

decoratorKeyWords :: Set String
decoratorKeyWords = fromList [
        "unit", -- for units on numbers, such as in `3 + 4i`
        "assign" -- could be a neat way to assign `todo` things to certain groups / people
    ]

composite :: [Spanned RawToken] -> CompilerExcept [Spanned Token]
composite [] = pure []
-- grouping
composite (s :@ RawNatural n   : s' :@ RawSnakeName unit : toks) = ((s <> s') :@ NaturalWithUnit n unit :) <$> composite toks
composite (s :@ DecoratorStart : s' :@ RawSnakeName name : toks) = if decoratorKeyWords & member name
    then ((s <> s') :@ Decorator name      :) <$> composite toks
    else raiseInit ("invalid decorator name " <> show name)
        >>= \poison -> ((s <> s') :@ Erronious poison :) <$> composite toks
composite (s :@ DecoratorStart : s' :@ WhiteSpace : s'' :@ RawSnakeName name : toks) = if decoratorKeyWords & member name
    then ((s <> s' <> s'') :@ Decorator name      :) <$> composite toks
    else raiseInit ("invalid decorator name " <> show name)
        >>= \poison -> ((s <> s' <> s'') :@ Erronious poison :) <$> composite toks
-- spliting
composite (s :@ RawSymbolicName "?." : toks) = (\ts -> s :@ Keyword "?" : s :@ Keyword "." : ts) <$> composite toks
-- errors
composite (s :@ DecoratorStart : toks) = raiseInit "invalid decorator syntax"
    >>= \poison -> (s :@ Erronious poison :) <$> composite toks
composite (s :@ RawCamelName _ : s' :@ RawSnakeName _ : toks) = raiseInit "snake and camel names must be seperated by a space"
    >>= \poison -> ((s <> s') :@ Erronious poison :) <$> composite toks
composite (s :@ RawSnakeName _ : s' :@ RawCamelName _ : toks) = raiseInit "snake and camel names must be seperated by a space"
    >>= \poison -> ((s <> s') :@ Erronious poison :) <$> composite toks
-- keywords
composite (s :@ RawCamelName    name : toks) = if camelKeyWords & member name
    then (s :@ Keyword name :) <$> composite toks
    else (s :@ Camel name   :) <$> composite toks
composite (s :@ RawSnakeName    name : toks) = if snakeKeyWords & member name
    then (s :@ Keyword name :) <$> composite toks
    else (s :@ Snake name   :) <$> composite toks
composite (s :@ RawSymbolicName name : toks) = if symbolicKeyWords & member name
    then (s :@ Keyword name :) <$> composite toks
    else (s :@ Symbol name  :) <$> composite toks
-- defaults
composite (s :@ RawIndentation i    : toks) = (s :@ Indentation i     :) <$> composite toks -- we want to kill this
composite (s :@ RawSmallString str  : toks) = (s :@ SmallString str   :) <$> composite toks
composite (s :@ RawStringStart str  : toks) = (s :@ StringStart str   :) <$> composite toks
composite (s :@ RawStringMiddle str : toks) = (s :@ StringMiddle str  :) <$> composite toks
composite (s :@ RawStringEnd str    : toks) = (s :@ StringEnd str     :) <$> composite toks
composite (s :@ RawDocComment str   : toks) = (s :@ Documentation str :) <$> composite toks
composite (s :@ RawNatural n        : toks) = (s :@ Natural n         :) <$> composite toks
composite (s :@ RawElementary c     : toks) = (s :@ Elementary c      :) <$> composite toks
composite (_ :@ WhiteSpace          : toks) =                          composite toks

convertErr :: ParseError (Spanned ClassifiedChar) Expectation -> CompilerExcept LexerError
convertErr _ = pure LexerParseError

lexer :: EditorInfo -> [String] -> CompilerExcept (Either LexerError [Spanned Token])
lexer i ls = case prepareLines i ls & runParser lexTokens of
    Left err -> Left <$> convertErr err
    Right toks -> Right <$> composite toks