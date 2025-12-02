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
    TokenIssue,
    Token,
    processLines
) where

import Data.Set (Set, fromList, member)
import Data.Function ((&))
import Data.Char (isUpper, isLower, isSpace, isAlphaNum)
import Frontend (EditorInfo (..), widthOf)
import GHC.TypeLits (Nat)
import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import Control.Monad (join)
import Reporting (PoisonID, CompilerExcept, raise, raiseInit, internalFailure, InternalCompilerError (..))
import Data.Maybe (mapMaybe)
import Frontend.Spanned (Spanned (..), Span (..), TextPos (..), startPoint, endPoint)
import Data.List (isPrefixOf)

data MatchFail
    = UnexpectedEOS TextPos
    | ExpectedFound Span Expectation Char
    | UnrecognisedInput Span Char

type Expectation = String

type Matcher a = Char -> Either Expectation (MatchAccept a)

mapMatcher :: (a -> b) -> Matcher a -> Matcher b
mapMatcher f = (>>> fmap (fmap f))

expectThen :: Char -> Matcher a -> Matcher a
expectThen t matcher t' = if t == t'
    then Right $ Continue Nothing matcher
    else Left $ show t

expectFinally :: Char -> a -> Matcher a
expectFinally t x t' = if t == t'
    then Right $ Finish x
    else Left $ show t

data MatchAccept a
    = Finish a
    | Continue (Maybe a) (Matcher a)

instance Functor MatchAccept where
    fmap f (Finish x) = Finish $ f x
    fmap f (Continue mx matcher) = Continue (f <$> mx) $ mapMatcher f matcher

-- legacy, kept for reference
-- lexWith :: [Matcher t a] -> [t] -> [Either (MatchFail t) a]
-- lexWith _ [] = []
-- lexWith matchers (t : ts) = case matchers & mapMaybe (($ t) >>> either (const Nothing) Just) of
--         [] -> Left (UnrecognisedInput t) : lexWith matchers ts
--         acc : _ -> maxMunch acc ts
--     where
--         maxMunch (Finish x) ts' = Right x : lexWith matchers ts'
--         maxMunch (Continue mx matcher) (t' : ts') = case matcher t' of
--             Left ex -> maybe (Left $ ExpectedFound ex t') Right mx : lexWith matchers (t' : ts')
--             Right acc -> maxMunch acc ts'
--         maxMunch (Continue mx _) [] = [maybe (Left UnexpectedEOS) Right mx]

lexLineWith :: EditorInfo -> Nat -> Nat -> [Matcher a] -> [Char] -> [Spanned (Either MatchFail a)]
lexLineWith _ _ _ _ [] = []
lexLineWith i line_num starting_char matchers (t : ts) =
    let next_char = starting_char + (i & widthOf t) in case matchers & mapMaybe (($ t) >>> either (const Nothing) Just) of
        [] -> between starting_char next_char :@ Left (UnrecognisedInput (between starting_char next_char) t)
            : lexLineWith i line_num next_char matchers ts
        acc : _ -> maxMunch starting_char next_char acc ts
    where
        between c c' = Range (TextPos line_num c) (TextPos line_num c')
        maxMunch c c' (Finish x) ts' = between c c' :@ Right x : lexLineWith i line_num c' matchers ts'
        maxMunch c c' (Continue mx matcher) (t' : ts') =
            let next_c' = c' + (i & widthOf t') in case matcher t' of
                Left ex -> between c c' :@ maybe (Left $ ExpectedFound (between c' next_c') ex t') Right mx
                    : lexLineWith i line_num c' matchers (t' : ts')
                Right acc -> maxMunch c next_c' acc ts'
        maxMunch c c' (Continue mx _) [] = [between c c' :@ maybe (Left $ UnexpectedEOS (TextPos line_num c')) Right mx]

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

whiteSpaceMatcher :: Matcher PreToken
whiteSpaceMatcher ' '  = Right $ Continue (Just WhiteSpace) whiteSpaceMatcher
whiteSpaceMatcher '\t' = Right $ Continue (Just WhiteSpace) whiteSpaceMatcher
whiteSpaceMatcher '\n' = Right $ Continue (Just WhiteSpace) whiteSpaceMatcher
whiteSpaceMatcher '\r' = Right $ Continue (Just WhiteSpace) whiteSpaceMatcher
whiteSpaceMatcher _    = Left "white space charecter"

stringMatcher :: Matcher PreToken
stringMatcher = expectThen '\"' $ mapMatcher StringLit stringBodyMatcher
    where
        stringBodyMatcher :: Matcher String
        stringBodyMatcher '\"' = Right $ Finish ""
        stringBodyMatcher '\\' = Right $ Continue Nothing $ \case
            '\\' -> Right $ Continue Nothing $ mapMatcher ('\\' :) stringBodyMatcher
            't'  -> Right $ Continue Nothing $ mapMatcher ('\t' :) stringBodyMatcher
            'n'  -> Right $ Continue Nothing $ mapMatcher ('\n' :) stringBodyMatcher
            'r'  -> Right $ Continue Nothing $ mapMatcher ('\r' :) stringBodyMatcher
            _    -> Left "escaped charecter"
        stringBodyMatcher c = Right $ Continue Nothing $ mapMatcher (c :) stringBodyMatcher

charMatcher :: Matcher PreToken
charMatcher = expectThen '\'' $ \case
    '\\' -> Right $ Continue Nothing (\case
        '\\' -> Right $ Continue Nothing $ expectFinally '\'' (CharLit '\\')
        't'  -> Right $ Continue Nothing $ expectFinally '\'' (CharLit '\t')
        'n'  -> Right $ Continue Nothing $ expectFinally '\'' (CharLit '\n')
        'r'  -> Right $ Continue Nothing $ expectFinally '\'' (CharLit '\r')
        _ -> Left "escaped charecter"
        )
    c    -> Right $ Continue Nothing $ expectFinally '\'' (CharLit c)

numMatcher :: Matcher PreToken
numMatcher = mapMatcher NumLit $ numContinueMatcher 0
    where
        next n n' = (10 * n) + n'
        continueMatching n n' = Right $ Continue (Just $ next n n') $ numContinueMatcher (next n n')

        numContinueMatcher :: Nat -> Matcher Nat
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

symbolMatcher :: Matcher PreToken
symbolMatcher = mapMatcher SymbolName symbolMatcherInner
    where
        symbolMatcherInner :: Matcher String
        symbolMatcherInner c
            | symbolicCharecters & member c = Right $ Continue (Just [c]) $ mapMatcher (c :) symbolMatcherInner
            | otherwise                     = Left "symbolic charecter (one of \"!$%^&*-+=:@~|<>?./\")"

snakeMatcher :: Matcher PreToken
snakeMatcher = mapMatcher SnakeName snakeMatcherInner
    where
        snakeMatcherInner :: Matcher String
        snakeMatcherInner '_' = Right $ Continue (Just "_") $ mapMatcher ('_' :) snakeMatcherInner
        snakeMatcherInner c
            | isLower c = Right $ Continue (Just [c]) $ mapMatcher (c :) snakeMatcherInner
            | otherwise = Left "lowercase charecter or underscore"

pascalMatcher :: Matcher PreToken
pascalMatcher c
    | isUpper c = Right $ Continue (Just $ PascalName [c]) $ mapMatcher ((c :) >>> PascalName) pascalBodyMatcher
    | otherwise = Left "uppercase charecter"
    where
        pascalBodyMatcher :: Matcher String
        pascalBodyMatcher c
            | isAlphaNum c = Right $ Continue (Just [c]) $ mapMatcher (c :) pascalBodyMatcher
            | otherwise    = Left "alphanumberic charecter"

-- general purpose matcher thing for `#` syntax
hashMatcher :: Matcher PreToken
hashMatcher = expectThen '#' $ \case
    ' ' -> Right $ Continue (Just Comment) $ mapMatcher (const Comment) matchComment
    '|' -> Right $ Continue (Just $ DocComment "") $ expectThen ' ' $ mapMatcher DocComment matchComment
    '[' -> Right $ Finish HashDecorator
    _   -> Left "one of ' ', '|', or '['"
    where
        matchComment :: Matcher String
        matchComment '\n' = Left "no newlines"
        matchComment c    = Right $ Continue (Just [c]) $ mapMatcher (c :) matchComment

syntaxCharecters :: Set Char
syntaxCharecters = Data.Set.fromList ",;()[]{}"

syntaxMatcher :: Matcher PreToken
syntaxMatcher c
    | syntaxCharecters & member c = Right $ Finish (SyntaxChar c)
    | otherwise                   = Left "one of \",;()[]{}\""

lexer :: [Matcher PreToken]
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

reportToken :: Either MatchFail a -> CompilerExcept (Either PoisonID a)
reportToken (Left (UnexpectedEOS at)) = raiseInit (show at ++ " unexpected end of line") <&> Left
reportToken (Left (ExpectedFound at ex t)) = raiseInit (show at ++ " expected " ++ ex ++ ", found " ++ show t) <&> Left
reportToken (Left (UnrecognisedInput at t)) = raiseInit (show at ++ " charecter " ++ show t ++ " is not recognised as the start of any token") <&> Left
reportToken (Right x) = pure $ Right x

reportSpannedToken :: Spanned (Either MatchFail a) -> CompilerExcept (Spanned (Either PoisonID a))
reportSpannedToken (s :@ tok) = (s :@) <$> reportToken tok

lexLine :: EditorInfo -> Nat -> String -> CompilerExcept [Spanned (Either PoisonID PreToken)]
lexLine i line_number line_txt = do
    let (line_start, line_body) = break (\c -> c /= ' ' && c /= '\t') line_txt
    let start_width = sum [i & widthOf t | t <- line_start]
    let start_span = Range (TextPos line_number 0) (TextPos line_number start_width)
    ts <- mapM reportSpannedToken $ lexLineWith i line_number start_width lexer line_body
    case ts of
        -- empty lines and lines that are just comments do not contribute to indentation tracking
        [] -> pure []
        [_ :@ (Right Comment)] -> pure []
        -- track indentation
        _ -> pure $ start_span :@ Right (Indentation line_start) : ts

lexLines :: EditorInfo -> [String] -> CompilerExcept [Spanned (Either PoisonID PreToken)]
lexLines i lines_txt = sequence [ lexLine i n line_txt | (n, line_txt) <- zip [0..] lines_txt] <&> join

debugLexLines :: EditorInfo -> [String] -> CompilerExcept [String]
debugLexLines i lines_txt = lexLines i lines_txt <&> fmap show

pascalKeyWords :: Set String
pascalKeyWords = Data.Set.fromList [
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

data TokenIssue
    = Malformed
    | BadIndentation
    | BadIndentationOnClosingCurly
    | UnbalancedClosingCurly
    | InsufficientSpacing
    | InvalidUseOfKeyword
    | IncompleteDecorator
    | InvalidDecorator
    deriving Show

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
    | CurlyItemContinuationKeyword String
    | Decorator String
    | Snake String
    | Pascal String
    | Symbol String
    | StringLiteral String
    | CharLiteral Char
    | Natural Nat
    | NaturalWithUnit Nat String
    | Documentation String
    | Error TokenIssue PoisonID
    deriving Show

data IndentationLevel
    = Above
    | Matching
    | Invalid String

-- we could ignore stretched inline blocks, which would translate to skipping the `Nothing` cases, and in doing so parse more things.
-- 
checkIndentation :: [Maybe String] -> String -> IndentationLevel
checkIndentation (Nothing : _) _ = Invalid $ "expected indentation is not well defined for malformed blocks"
    ++ "\n(if you are trying to define a multi-line block ensure the opening curly bracket is immediately followed by a new line)"
checkIndentation (Just indentation : _) candidate
    | indentation `isPrefixOf` candidate = if length indentation == length candidate
        then Matching
        else Above
    | otherwise = Invalid "does not start with indentation matching the start of the block"
checkIndentation [] "" = Matching
checkIndentation [] _ = Above


-- | ## Composes pre-tokens into tokens
compose :: [Maybe String] -> [Spanned (Either PoisonID PreToken)] -> CompilerExcept [Spanned Token]
-- white space elim
compose indents (_ :@ Right WhiteSpace : ts) = compose indents ts
compose indents (s :@ Right (SyntaxChar c) : _ :@ Right WhiteSpace : ts) = compose indents (s :@ Right (SyntaxChar c) : ts)
-- start multi-line block
compose indents (s :@ Right (SyntaxChar '{') : s' :@ Right (Indentation indent) : ts) = (\ts' ->
        s :@ OpenCurly : endPoint s' :@ CurlyItem : ts'
    ) <$> compose (Just indent : indents) ts
-- empty block
compose indents (s :@ Right (SyntaxChar '{') : s' :@ Right (SyntaxChar '}') : ts) = (\ts' ->
        s :@ OpenCurly : s' :@ CloseCurly : ts'
    ) <$> compose indents ts
compose indents (s :@ Right (SyntaxChar '}') : s' :@ Right (Indentation indent) : s'' :@ Right (SyntaxChar '}') : ts) = case checkIndentation indents indent of
    Above -> (\ts' ->
            s :@ OpenCurly : s'' :@ CloseCurly : ts'
        ) <$> compose indents ts
    Matching -> (\ts' ->
            s :@ OpenCurly : s'' :@ CloseCurly : ts'
        ) <$> compose indents ts
    Invalid err -> raiseInit (show s' ++ " " ++ err)
        >>= \poison_id -> (\ts' ->
            s :@ OpenCurly : s'' :@ Error BadIndentationOnClosingCurly poison_id : ts'
        ) <$> compose indents ts
-- start inline block
compose indents (s :@ Right (SyntaxChar '{') : ts) = (s :@ OpenCurly :) <$> compose (Nothing : indents) ts
-- close a block
compose (_ : indents) (s :@ Right (Indentation indent) : s' :@ Right (SyntaxChar '}') : ts) = case checkIndentation indents indent of
    Above -> (s' :@ CloseCurly :) <$> compose indents ts
    Matching -> (s' :@ CloseCurly :) <$> compose indents ts
    Invalid err -> raiseInit (show s ++ " " ++ err)
        >>= \poison_id -> (s' :@ Error BadIndentationOnClosingCurly poison_id :) <$> compose indents ts
compose (_ : indents) (s :@ Right (SyntaxChar '}') : ts) = (s :@ CloseCurly :) <$> compose indents ts
compose [] (s :@ Right (SyntaxChar '}') : ts) = raiseInit (show s ++ " unbalanced curly bracket")
    >>= \poison_id -> (s :@ Error UnbalancedClosingCurly poison_id :) <$> compose [] ts
-- doc comments must be properly indented
compose indents (s :@ Right (Indentation indent) : s' :@ Right (DocComment com) : ts) = case checkIndentation indents indent of
    Above -> compose indents ts
    Matching -> (s' :@ Documentation com :) <$> compose indents ts
    Invalid err -> raiseInit (show s ++ " " ++ err)
        >>= \poison_id -> (s :@ Error BadIndentation poison_id :) <$> compose indents ts
compose indents (_ :@ Right (DocComment _) : ts) = compose indents ts
-- block item
compose indents (s :@ Right (Indentation indent) : s' :@ Right (SnakeName name) : ts) = case checkIndentation indents indent of
    Above -> compose indents (s' :@ Right (SnakeName name) : ts)
    Matching -> if statementContinuationKeyWords & member name
        then ((s <> s') :@ CurlyItemContinuationKeyword name :) <$> compose indents ts
        else (s :@ CurlyItem :) <$> compose indents (s' :@ Right (SnakeName name) : ts)
    Invalid err -> raiseInit (show s ++ " " ++ err)
        >>= \poison_id -> (s :@ Error BadIndentation poison_id :) <$> compose indents (s' :@ Right (SnakeName name) : ts)
compose indents (s :@ Right (Indentation indent) : ts) = case checkIndentation indents indent of
    Above -> compose indents ts
    Matching -> (s :@ CurlyItem :) <$> compose indents ts
    Invalid err -> raiseInit (show s ++ " " ++ err)
        >>= \poison_id -> (s :@ Error BadIndentation poison_id :) <$> compose indents ts
-- names and keywords
compose indents (s :@ Right (SnakeName _) : s' :@ Right (PascalName _) : ts) =
    raiseInit (show (s <> s') ++ " snake and pascal names must be seperated by white space")
    >>= \poison_id -> ((s <> s') :@ Error InsufficientSpacing poison_id :) <$> compose indents ts
compose indents (s :@ Right (PascalName _) : s' :@ Right (SnakeName _) : ts) =
    raiseInit (show (s <> s') ++ " pascal and snake names must be seperated by white space")
    >>= \poison_id -> ((s <> s') :@ Error InsufficientSpacing poison_id :) <$> compose indents ts
compose indents (s :@ Right (SnakeName name) : ts)
    | snakeKeyWords & member name = (s :@ Keyword name :) <$> compose indents ts
    | otherwise                   = (s :@ Snake name   :) <$> compose indents ts
compose indents (s :@ Right (PascalName name) : ts)
    | pascalKeyWords & member name = (s :@ Keyword name :) <$> compose indents ts
    | otherwise                    = (s :@ Pascal name  :) <$> compose indents ts
compose indents (s :@ Right (SymbolName name) : ts)
    | specialOperators & member name = (s :@ SpecialOperator name :) <$> compose indents ts
    | symbolicKeyWords & member name = (s :@ Keyword name         :) <$> compose indents ts
    | otherwise                      = (s :@ Symbol name          :) <$> compose indents ts
compose indents (s :@ Right HashDecorator : s' :@ Right (SnakeName name) : ts)
    | decoratorKeyWords & member name = ((s <> s') :@ Decorator name :) <$> compose indents ts
    | otherwise                       = raiseInit (show s' ++ " invalid decorator")
        >>= \poison_id -> ((s <> s') :@ Error InvalidDecorator poison_id :) <$> compose indents ts
compose indents (s :@ Right HashDecorator : ts) = raiseInit (show s ++ " incomplete decorator")
    >>= \poison_id -> (s :@ Error IncompleteDecorator poison_id :) <$> compose indents ts 
-- literals
compose indents (s :@ Right (NumLit n) : s' :@ Right (SnakeName name) : ts)
    | snakeKeyWords & member name = raiseInit (show (s <> s') ++ " keyword cannot be a unit")
        >>= \poison_id -> ((s <> s') :@ Error InvalidUseOfKeyword poison_id :) <$> compose indents ts
    | otherwise = ((s <> s') :@ NaturalWithUnit n name :) <$> compose indents ts
compose indents (s :@ Right (NumLit n) : ts) = (s :@ Natural n :) <$> compose indents ts
compose indents (s :@ Right (StringLit str) : ts) = (s :@ StringLiteral str :) <$> compose indents ts
compose indents (s :@ Right (CharLit ch) : ts) = (s :@ CharLiteral ch :) <$> compose indents ts
-- syntax
compose indents (s :@ Right (SyntaxChar ',') : ts) = (s :@ Comma       :) <$> compose indents ts
compose indents (s :@ Right (SyntaxChar ';') : ts) = (s :@ Semicolon   :) <$> compose indents ts
compose indents (s :@ Right (SyntaxChar '(') : ts) = (s :@ OpenRound   :) <$> compose indents ts
compose indents (s :@ Right (SyntaxChar ')') : ts) = (s :@ CloseRound  :) <$> compose indents ts
compose indents (s :@ Right (SyntaxChar '[') : ts) = (s :@ OpenSquare  :) <$> compose indents ts
compose indents (s :@ Right (SyntaxChar ']') : ts) = (s :@ CloseSquare :) <$> compose indents ts
compose _ (s :@ Right (SyntaxChar c)   : _ ) = internalFailure $ LexerIdentifiedIncorrectSyntax s c
-- discards
compose indents (_ :@ Right Comment : ts) = compose indents ts
-- eventually we might want to convert unclosed open curlys into zero width error tokens at the end
compose _ [] = pure []
-- error propogation
compose indents (s :@ Left poison_id : ts) = (s :@ Error Malformed poison_id :) <$> compose indents ts

processLines :: EditorInfo -> [String] -> CompilerExcept [Spanned Token]
processLines i ls = lexLines i ls >>= compose []