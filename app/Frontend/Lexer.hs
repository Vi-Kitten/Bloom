{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use guards" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module Frontend.Lexer (
    Token,
    debugLexLines
) where

import Data.Set (Set, fromList, member)
import Data.Function ((&))
import Data.Char (isUpper, isLower, isSpace, isAlphaNum)
import Frontend (EditorInfo (..), widthOf)
import GHC.TypeLits (Nat)
import Control.Arrow ((>>>))
import Data.Functor ((<&>), ($>))
import Control.Monad (join)
import Reporting (PoisonID, CompilerExcept, raise, raiseInit)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..), fromList, toList)
import Data.Maybe (mapMaybe)
import Parser.Spanned (Spanned (..), Span (..), TextPos (..))

data MatchFail t
    = UnexpectedEOS
    | ExpectedFound Expectation t
    | UnrecognisedInput t

type Expectation = String

type Matcher t a = t -> Either Expectation (MatchAccept t a)

mapMatcher :: (a -> b) -> Matcher t a -> Matcher t b
mapMatcher f = (>>> fmap (fmap f))

expectThen :: Show t => Eq t => t -> Matcher t a -> Matcher t a
expectThen t matcher t' = if t == t'
    then Right $ Continue Nothing matcher
    else Left $ show t

expectFinally :: Show t => Eq t => t -> a -> Matcher t a
expectFinally t x t' = if t == t'
    then Right $ Finish x
    else Left $ show t

data MatchAccept t a
    = Finish a
    | Continue (Maybe a) (Matcher t a)

instance Functor (MatchAccept t) where
    fmap f (Finish x) = Finish $ f x
    fmap f (Continue mx matcher) = Continue (f <$> mx) $ mapMatcher f matcher

-- legacy, kept for reference
lexWith :: [Matcher t a] -> [t] -> [Either (MatchFail t) a]
lexWith _ [] = []
lexWith matchers (t : ts) = case matchers & mapMaybe (($ t) >>> either (const Nothing) Just) of
        [] -> Left (UnrecognisedInput t) : lexWith matchers ts
        acc : _ -> maxMunch acc ts
    where
        maxMunch (Finish x) ts' = Right x : lexWith matchers ts'
        maxMunch (Continue mx matcher) (t' : ts') = case matcher t' of
            Left ex -> maybe (Left $ ExpectedFound ex t') Right mx : lexWith matchers (t' : ts')
            Right acc -> maxMunch acc ts'
        maxMunch (Continue mx _) [] = [maybe (Left UnexpectedEOS) Right mx]

lexLineWith :: EditorInfo -> Nat -> Nat -> [Matcher Char a] -> [Char] -> [Spanned (Either (MatchFail Char) a)]
lexLineWith _ _ _ _ [] = []
lexLineWith i line_num starting_char matchers (t : ts) =
    let next_char = starting_char + (i & widthOf t) in case matchers & mapMaybe (($ t) >>> either (const Nothing) Just) of
        [] -> between starting_char next_char :@ Left (UnrecognisedInput t) : lexLineWith i line_num next_char matchers ts
        acc : _ -> maxMunch starting_char next_char acc ts
    where
        between c c' = Span (TextPos line_num c) (TextPos line_num c')
        maxMunch c c' (Finish x) ts' = between c c' :@ Right x : lexLineWith i line_num c' matchers ts'
        maxMunch c c' (Continue mx matcher) (t' : ts') = case matcher t' of
            Left ex -> between c c' :@ maybe (Left $ ExpectedFound ex t') Right mx : lexLineWith i line_num c' matchers (t' : ts')
            Right acc -> maxMunch c (c' + (i & widthOf t')) acc ts'
        maxMunch c c' (Continue mx _) [] = [between c c' :@ maybe (Left UnexpectedEOS) Right mx]

data PreToken
    = WhiteSpace
    | StringLit String
    | CharLit Char
    | NumLit Nat
    | SymbolName String
    | SnakeName String
    | PascalName String
    | HashDecorator
    | Comment
    | DocComment String
    | SyntaxChar Char
    | Indentation String
    deriving Show

whiteSpaceMatcher :: Matcher Char PreToken
whiteSpaceMatcher ' '  = Right $ Continue (Just WhiteSpace) whiteSpaceMatcher
whiteSpaceMatcher '\t' = Right $ Continue (Just WhiteSpace) whiteSpaceMatcher
whiteSpaceMatcher '\n' = Right $ Continue (Just WhiteSpace) whiteSpaceMatcher
whiteSpaceMatcher '\r' = Right $ Continue (Just WhiteSpace) whiteSpaceMatcher
whiteSpaceMatcher _    = Left "white space charecter"

stringMatcher :: Matcher Char PreToken
stringMatcher = expectThen '\"' $ mapMatcher StringLit stringBodyMatcher
    where
        stringBodyMatcher :: Matcher Char String
        stringBodyMatcher '\"' = Right $ Finish ""
        stringBodyMatcher '\\' = Right $ Continue Nothing $ \case
            '\\' -> Right $ Continue Nothing $ mapMatcher ('\\' :) stringBodyMatcher
            't'  -> Right $ Continue Nothing $ mapMatcher ('\t' :) stringBodyMatcher
            'n'  -> Right $ Continue Nothing $ mapMatcher ('\n' :) stringBodyMatcher
            'r'  -> Right $ Continue Nothing $ mapMatcher ('\r' :) stringBodyMatcher
            _    -> Left "escaped charecter"
        stringBodyMatcher c = Right $ Continue Nothing $ mapMatcher (c :) stringBodyMatcher

charMatcher :: Matcher Char PreToken
charMatcher = expectThen '\'' $ \case
    '\\' -> Right $ Continue Nothing (\case
        '\\' -> Right $ Continue Nothing $ expectFinally '\'' (CharLit '\\')
        't'  -> Right $ Continue Nothing $ expectFinally '\'' (CharLit '\t')
        'n'  -> Right $ Continue Nothing $ expectFinally '\'' (CharLit '\n')
        'r'  -> Right $ Continue Nothing $ expectFinally '\'' (CharLit '\r')
        _ -> Left "escaped charecter"
        )
    c    -> Right $ Continue Nothing $ expectFinally '\'' (CharLit c)

numMatcher :: Matcher Char PreToken
numMatcher = mapMatcher NumLit $ numContinueMatcher 0
    where
        next n n' = (10 * n) + n'
        continueMatching n n' = Right $ Continue (Just $ next n n') $ numContinueMatcher (next n n')

        numContinueMatcher :: Nat -> Matcher Char Nat
        numContinueMatcher n '0' = continueMatching n 0
        numContinueMatcher n '1' = continueMatching n 1
        numContinueMatcher n '2' = continueMatching n 2
        numContinueMatcher n '3' = continueMatching n 3
        numContinueMatcher n '4' = continueMatching n 4
        numContinueMatcher n '5' = continueMatching n 5
        numContinueMatcher n '6' = continueMatching n 6
        numContinueMatcher n '7' = continueMatching n 7
        numContinueMatcher n '8' = continueMatching n 8
        numContinueMatcher n '9' = continueMatching n 9
        numContinueMatcher _ _   = Left "digit"

symbolicCharecters :: Set Char
symbolicCharecters = Data.Set.fromList "!$%^&*-+=:@~|<>?./"

symbolMatcher :: Matcher Char PreToken
symbolMatcher = mapMatcher SymbolName symbolMatcherInner
    where
        symbolMatcherInner :: Matcher Char String
        symbolMatcherInner c
            | symbolicCharecters & member c = Right $ Continue (Just [c]) $ mapMatcher (c :) symbolMatcherInner
            | otherwise                     = Left "symbolic charecter (one of \"!$%^&*-+=:@~|<>?./\")"

snakeMatcher :: Matcher Char PreToken
snakeMatcher = mapMatcher SnakeName snakeMatcherInner
    where
        snakeMatcherInner :: Matcher Char String
        snakeMatcherInner '_' = Right $ Continue (Just "_") $ mapMatcher ('_' :) snakeMatcherInner
        snakeMatcherInner c
            | isLower c = Right $ Continue (Just [c]) $ mapMatcher (c :) snakeMatcherInner
            | otherwise = Left "lowercase charecter or underscore"

pascalMatcher :: Matcher Char PreToken
pascalMatcher c
    | isUpper c = Right $ Continue (Just $ PascalName [c]) $ mapMatcher ((c :) >>> PascalName) pascalBodyMatcher
    | otherwise = Left "uppercase charecter"
    where
        pascalBodyMatcher :: Matcher Char String
        pascalBodyMatcher c
            | isAlphaNum c = Right $ Continue (Just [c]) $ mapMatcher (c :) pascalBodyMatcher
            | otherwise    = Left "alphanumberic charecter"

-- general purpose matcher thing for `#` syntax
hashMatcher :: Matcher Char PreToken
hashMatcher = expectThen '#' $ \case
    ' ' -> Right $ Continue (Just Comment) $ mapMatcher (const Comment) matchComment
    '|' -> Right $ Continue (Just $ DocComment "") $ expectThen ' ' $ mapMatcher DocComment matchComment
    '[' -> Right $ Finish HashDecorator
    _   -> Left "one of ' ', '|', or '['"
    where
        matchComment :: Matcher Char String
        matchComment '\n' = Left "no newlines"
        matchComment c    = Right $ Continue (Just [c]) $ mapMatcher (c :) matchComment

syntaxCharecters :: Set Char
syntaxCharecters = Data.Set.fromList "()[]{}"

syntaxMatcher :: Matcher Char PreToken
syntaxMatcher c
    | syntaxCharecters & member c = Right $ Finish (SyntaxChar c)
    | otherwise                   = Left "one of \",;()[]{}\""

lexer :: [Matcher Char PreToken]
lexer = [
        whiteSpaceMatcher,
        stringMatcher,
        charMatcher,
        numMatcher,
        symbolMatcher,
        snakeMatcher,
        pascalMatcher,
        hashMatcher,
        syntaxMatcher
    ]

reportToken :: Either (MatchFail Char) a -> CompilerExcept (Either PoisonID a)
reportToken (Left UnexpectedEOS) = raiseInit "unexpected end of line" <&> Left
reportToken (Left (ExpectedFound ex t)) = raiseInit ("expected " ++ ex ++ ", found: " ++ show t) <&> Left
reportToken (Left (UnrecognisedInput t)) = raiseInit ("charecter " ++ show t ++ " is not recognised as the start of any token") <&> Left
reportToken (Right x) = pure $ Right x

reportSpannedToken :: Spanned (Either (MatchFail Char) a) -> CompilerExcept (Spanned (Either PoisonID a))
reportSpannedToken (s :@ tok) = (s :@) <$> reportToken tok

lexLine :: EditorInfo -> Nat -> String -> CompilerExcept [Spanned (Either PoisonID PreToken)]
lexLine i line_number line_txt = do
    let (line_start, line_body) = break (\c -> c /= ' ' || c /= '\t') line_txt
    let start_width = sum [i & widthOf t | t <- line_start]
    let start_span = Span (TextPos line_number 0) (TextPos line_number start_width)
    mapM reportSpannedToken (start_span :@ Right (Indentation line_start) : lexLineWith i line_number start_width lexer line_body)

lexLines :: EditorInfo -> [String] -> CompilerExcept [Spanned (Either PoisonID PreToken)]
lexLines i lines_txt = sequence [ lexLine i n line_txt | (n, line_txt) <- zip [0..] lines_txt] <&> join


debugLexLines :: EditorInfo -> [String] -> CompilerExcept [String]
debugLexLines i lines_txt = lexLines i lines_txt <&> fmap show

camelKeyWords :: Set String
camelKeyWords = Data.Set.fromList [
    -- datatypes
        "Self",
    -- reservations
        "Super"
    ]

snakeKeyWords :: Set String
snakeKeyWords = Data.Set.fromList [
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
        "and",
        "or",
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
        "pattern",
    -- control flow
        "let",
        "do",
        "with",
        "match",
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
statementContinuationKeyWords = Data.Set.fromList [
        "elif",
        "else",
        "catch",
        "with",
        "nobreak"
    ]

symbolicKeyWords :: Set String
symbolicKeyWords = Data.Set.fromList [
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
specialOperators = Data.Set.fromList [
    -- numberic
        "+",
        "-",
        "*",
        "/",
        "++",
        "--"
    ]

decoratorKeyWords :: Set String
decoratorKeyWords = Data.Set.fromList [
        "unit", -- for units on numbers, such as in `3 + 4i`
        "assign" -- could be a neat way to assign `todo` things to certain groups / people
    ]

data Token
    = Keyword String
    | SpecialOperator String
    | Comma
    | Semicolon
    | OpenRound
    | CloseRound
    | OpenSquare
    | CloseSquare
    | OpenCurly
    | CloseCurly
    | CurlyItem
    | CurlyItemKeyword String
    | Snake String
    | Camel String
    | Symbol String
    | StringLiteral String
    | CharLiteral Char
    | Natural Nat
    | NaturalWithUnit Nat String
    | Documentation String
    | Malformed PoisonID
    deriving Show