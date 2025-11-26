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
    debugLexLine
) where

import Data.Set (Set, fromList, member)
import Data.Function ((&))
import Data.Char (isUpper, isLower, isSpace, isAlphaNum)
import Frontend (EditorInfo (..))
import GHC.TypeLits (Nat)
import Control.Arrow ((>>>))
import Data.Functor ((<&>), ($>))
import Control.Monad (join)
import Reporting (PoisonID, CompilerExcept, raise, raiseInit)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..), fromList, toList)
import Data.Maybe (mapMaybe)

type Matcher t a = t -> Maybe (MatchAccept t a)

mapMatcher :: (a -> b) -> Matcher t a -> Matcher t b
mapMatcher f = (>>> fmap (fmap f))

expectThen :: (t -> Bool) -> Matcher t a -> Matcher t a
expectThen p matcher t = if p t
    then Just $ Continue Nothing $ pure matcher
    else Nothing

expectFinally :: (t -> Bool) -> a -> Matcher t a
expectFinally p x t = if p t
    then Just $ Finish x
    else Nothing

data MatchAccept t a
    = Finish a
    | Continue (Maybe a) (NonEmpty (Matcher t a))

instance Functor (MatchAccept t) where
    fmap f (Finish x) = Finish $ f x
    fmap f (Continue mx matchers) = Continue (f <$> mx) $ mapMatcher f <$> matchers

lexWith :: [Matcher t a] -> [t] -> [Maybe a]
lexWith _ [] = []
lexWith matchers (t : ts) = case matchers & mapMaybe ($ t) of
        [] -> Nothing : lexWith matchers ts
        acc : _ -> maxMunch acc ts
    where
        maxMunch (Finish x) ts' = Just x : lexWith matchers ts'
        maxMunch (Continue mx matchers') (t' : ts') = case matchers' & toList & mapMaybe ($ t') of
            [] -> mx : lexWith matchers (t' : ts')
            acc : _ -> maxMunch acc ts'
        maxMunch (Continue mx _) [] = [mx]

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
whiteSpaceMatcher ' '  = Just $ Continue (Just WhiteSpace) $ pure whiteSpaceMatcher
whiteSpaceMatcher '\t' = Just $ Continue (Just WhiteSpace) $ pure whiteSpaceMatcher
whiteSpaceMatcher '\n' = Just $ Continue (Just WhiteSpace) $ pure whiteSpaceMatcher
whiteSpaceMatcher '\r' = Just $ Continue (Just WhiteSpace) $ pure whiteSpaceMatcher
whiteSpaceMatcher _    = Nothing

stringMatcher :: Matcher Char PreToken
stringMatcher = expectThen (== '\"') $ mapMatcher StringLit stringBodyMatcher
    where 
        stringBodyMatcher :: Matcher Char String
        stringBodyMatcher '\"' = Just $ Finish ""
        stringBodyMatcher '\\' = Just $ Continue Nothing $ pure $ \case
            '\\' -> Just $ Continue Nothing $ pure $ mapMatcher ('\\' :) stringBodyMatcher
            't'  -> Just $ Continue Nothing $ pure $ mapMatcher ('\t' :) stringBodyMatcher
            'n'  -> Just $ Continue Nothing $ pure $ mapMatcher ('\n' :) stringBodyMatcher
            'r'  -> Just $ Continue Nothing $ pure $ mapMatcher ('\r' :) stringBodyMatcher
            _    -> Nothing
        stringBodyMatcher c = Just $ Continue Nothing $ pure $ mapMatcher (c :) stringBodyMatcher

charMatcher :: Matcher Char PreToken
charMatcher = expectThen (== '\'') $ \case
    '\\' -> Just $ Continue Nothing $ pure (\case
        '\\' -> Just $ Continue Nothing $ pure $ expectFinally (== '\'') (CharLit '\\')
        't'  -> Just $ Continue Nothing $ pure $ expectFinally (== '\'') (CharLit '\t')
        'n'  -> Just $ Continue Nothing $ pure $ expectFinally (== '\'') (CharLit '\n')
        'r'  -> Just $ Continue Nothing $ pure $ expectFinally (== '\'') (CharLit '\r')
        _ -> Nothing
        )
    c    -> Just $ Continue Nothing $ pure $ expectFinally (== '\'') (CharLit c)

numMatcher :: Matcher Char PreToken
numMatcher = mapMatcher NumLit $ numContinueMatcher 0
    where
        next n n' = (10 * n) + n'
        continueMatching n n' = Just $ Continue (Just $ next n n') $ pure $ numContinueMatcher (next n n')

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
        numContinueMatcher _ _   = Nothing

symbolicCharecters :: Set Char
symbolicCharecters = Data.Set.fromList "!$%^&*-+=:@~|<>?./"

symbolMatcher :: Matcher Char PreToken
symbolMatcher = mapMatcher SymbolName symbolMatcherInner
    where
        symbolMatcherInner :: Matcher Char String
        symbolMatcherInner c
            | symbolicCharecters & member c = Just $ Continue (Just [c]) $ pure $ mapMatcher (c :) symbolMatcherInner
            | otherwise                     = Nothing

snakeMatcher :: Matcher Char PreToken
snakeMatcher = mapMatcher SnakeName snakeMatcherInner
    where
        snakeMatcherInner :: Matcher Char String
        snakeMatcherInner '_' = Just $ Continue (Just "_") $ pure $ mapMatcher ('_' :) snakeMatcherInner
        snakeMatcherInner c
            | isLower c = Just $ Continue (Just [c]) $ pure $ mapMatcher (c :) snakeMatcherInner
            | otherwise = Nothing

pascalMatcher :: Matcher Char PreToken
pascalMatcher c
    | isUpper c = Just $ Continue (Just $ PascalName [c]) $ pure $ mapMatcher ((c :) >>> PascalName) pascalBodyMatcher
    | otherwise = Nothing
    where
        pascalBodyMatcher :: Matcher Char String
        pascalBodyMatcher c
            | isAlphaNum c = Just $ Continue (Just [c]) $ pure $ mapMatcher (c :) pascalBodyMatcher
            | otherwise    = Nothing

-- general purpose matcher thing for `#` syntax
hashMatcher :: Matcher Char PreToken
hashMatcher = expectThen (== '#') $ \case
    ' ' -> Just $ Continue (Just Comment) $ pure $ mapMatcher (const Comment) matchComment
    '|' -> Just $ Continue (Just $ DocComment "") $ pure $ expectThen (== ' ') $ mapMatcher DocComment matchComment
    '[' -> Just $ Finish HashDecorator
    _   -> Nothing
    where
        matchComment :: Matcher Char String
        matchComment '\n' = Nothing
        matchComment c    = Just $ Continue (Just [c]) $ pure $ mapMatcher (c :) matchComment

syntaxCharecters :: Set Char
syntaxCharecters = Data.Set.fromList "()[]{}"

syntaxMatcher :: Matcher Char PreToken
syntaxMatcher c
    | syntaxCharecters & member c = Just $ Finish (SyntaxChar c)
    | otherwise                   = Nothing

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

lexLine :: String -> CompilerExcept [Maybe PreToken]
lexLine line = do
    let (start, body) = break (\c -> c /= ' ' || c /= '\t') line
    mapM pure (Just (Indentation start) : lexWith lexer body)


debugLexLine :: String -> CompilerExcept String
debugLexLine = lexLine >>> fmap show

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
        "/"
    ]

decoratorKeyWords :: Set String
decoratorKeyWords = Data.Set.fromList [
        "unit", -- for units on numbers, such as in `3 + 4i`
        "assign" -- could be a neat way to assign `todo` things to certain groups / people
    ]

data Token
    = Keyword String
    | SpecialOperator String
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
    | Erronious PoisonID
    deriving Show