{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Frontend.Parser (
    Parser,
    ParseError,
    parse,
    WithBindings (..),
    Matcher (..),
    MatchGaurd (..),
    Map (..),
    Pattern (..),
    InductiveFlow (..),
    Expr (..),
    Action (..),
    SwitchGaurd (..),
    BlockStatement (..),
    curlyItem,
    expr
) where
import Data.Kind (Type)

import Frontend.Lexer (Token (..))
import Frontend.Spanned (Spanned (..), TextPos, spanEnd)
import Control.Arrow ((>>>))
import Data.Function (on, (&))
import Data.List.NonEmpty (NonEmpty (..), last, init, toList)
import Utils ((<+>), (&>), AlternatingList (..), AlternatingListSep (..), posts, associateLeft)
import Data.Bifunctor (Bifunctor (..))
import Reporting (PoisonID, CompilerExcept, raise, raiseInit)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Foldable1 (foldl1')

type ParseExpectation = String

data Parser :: Type -> Type where
    Step  :: ParseExpectation -> (Token -> Maybe a)      -> Parser a
    Pure  :: a                                           -> Parser a
    Alt   :: Parser a        -> Parser a                 -> Parser a
    Then  :: Parser (a -> b) -> Parser a                 -> Parser b
    Recov :: Parser (Either PoisonID a -> b) -> Parser a -> Parser b

instance Functor Parser where
    fmap f (Step ex select) = Step ex $ select >>> fmap f
    fmap f (Pure x)         = Pure $ f x
    fmap f (Alt px py)      = (Alt `on` fmap f) px py
    fmap f (Then pf px)     = Then ((>>> f) <$> pf) px
    fmap f (Recov pr px)    = Recov ((>>> f) <$> pr) px

instance Applicative Parser where
    pure = Pure
    (<*>) = Then

data Munch a
    = Already (CompilerExcept a)
    | Accepted (CompilerExcept (Parser a))

munch :: Spanned Token -> Parser a -> Either (NonEmpty ParseExpectation) (Munch a)
munch (_ :@ tok) (Step ex select) = case select tok of
    Nothing -> Left $ ex :| []
    Just x -> Right $ Accepted $ pure $ Pure x
munch _ (Pure x) = Right $ Already $ pure x
munch stok (Alt px py) = munch stok px <+> munch stok py
munch stok (Then pf px) = pf & munch stok >>= \case
    Already f    -> px & munch stok <&> \case
        Already x    -> Already $ f <*> x
        Accepted px' -> Accepted $ (<$>) <$> f <*> px'
    Accepted pf' -> Right (Accepted $ pf' <&> (`Then` px))
munch (s :@ tok) (Recov pr px) = pr & munch (s :@ tok) <&> \case
    Already recov -> Already $ case px & close (spanEnd s) of
        Left exs -> do
            poison_id <- raiseInit $ show $ UnexpectedEOS (spanEnd s) exs
            (Left poison_id &) <$> recov
        Right x  -> recov <*> (Right <$> x)
    Accepted pr' -> Accepted $ case px & munch (s :@ tok) of
        Left exs             -> do
            poison_id <- raiseInit $ show $ ExpectedFound (s :@ tok) exs
            fmap (Left poison_id &) <$> pr'
        Right (Already _)    -> do
            poison_id <- raiseInit $ show $ Unexpected (s :@ tok)
            fmap (Left poison_id &) <$> pr'
        Right (Accepted px') -> Recov <$> pr' <*> px'

close :: TextPos -> Parser a -> Either (NonEmpty ParseExpectation) (CompilerExcept a)
close _ (Step ex _) = Left $ ex :| []
close _    (Pure x) = Right $ pure x
close final   (Alt px py) = close final px <+> close final py
close final  (Then pf px) = (<*>) <$> close final pf <*> close final px
close final (Recov pr px) = close final pr <&> \recov -> case px & close final of
    Left exs -> do
        poison_id <- raiseInit $ show $ UnexpectedEOS final exs
        (Left poison_id &) <$> recov
    Right x -> x >>= \x' -> (Right x' &) <$> recov

data ParseError
    = UnexpectedEOS TextPos (NonEmpty ParseExpectation)
    | ExpectedFound (Spanned Token) (NonEmpty ParseExpectation)
    | Unexpected (Spanned Token)

expectation :: NonEmpty ParseExpectation -> String
expectation (ex :| []) = ex
expectation exs = (Data.List.NonEmpty.init exs >>= \ex -> ex ++ ", ") ++ "or " ++ Data.List.NonEmpty.last exs

instance Show ParseError where
    show (UnexpectedEOS pos exs) = "Expected " ++ expectation exs ++ " at " ++ show pos
    show (ExpectedFound (s :@ tok) exs) = "Expected " ++ expectation exs ++ " found " ++ show tok ++ " at " ++ show s
    show (Unexpected (s :@ tok)) = "Unexpected token " ++ show tok ++ " at " ++ show s

parse :: TextPos -> [Spanned Token] -> Parser a -> CompilerExcept (Either ParseError a)
parse final []                px = sequenceA $ first (UnexpectedEOS final) $ close final px
parse final (s :@ tok : toks) px = case px & munch (s :@ tok) of
    Left exs -> return $ Left (ExpectedFound (s :@ tok) exs)
    Right (Already _) -> return $ Left (Unexpected (s :@ tok))
    Right (Accepted px') -> px' >>= parse final toks

equal :: Token -> Parser ()
equal tok = Step (show tok) $ \tok' -> if tok' == tok
    then Just ()
    else Nothing

opt :: Parser a -> Parser (Maybe a)
opt px = Alt (Just <$> px) (pure Nothing)

optDefault :: a -> Parser a -> Parser a
optDefault x px = fromMaybe x <$> opt px

mostSeperated :: Parser s -> Parser a -> Parser (AlternatingList s a)
mostSeperated py px = px &> Alt (flip (:-) <$> ((:+) <$> py <*> mostSeperated py px)) (pure End)

mostPostsSeperated :: Parser s -> Parser a -> Parser (NonEmpty a)
mostPostsSeperated py px = posts <$> mostSeperated py px

alt :: NonEmpty (Parser a) -> Parser a
alt (px :| []) = px
alt (px :| px' : pxs) = Alt px (alt $ px' :| pxs)

most :: Parser a -> Parser [a]
most px = optDefault [] $ (:) <$> px <*> most px

most1 :: Parser a -> Parser (NonEmpty a)
most1 px = (:|) <$> px <*> most px

-- pass
keyword kw = equal (Keyword kw)

openCurly = equal OpenCurly

curlyItem = Step "matching indentation" $ \case
    CurlyItem -> Just ()
    _ -> Nothing

simpleSkipItem = most (Step "" $ \case
        CurlyItem -> Nothing
        OpenCurly -> Nothing
        CloseCurly -> Nothing
        _ -> Just ()
    )

simpleSkipBlock = most (Step "" $ \case
        OpenCurly -> Nothing
        CloseCurly -> Nothing
        _ -> Just ()
    )

skipItem :: Parser ()
skipItem = () <$ mostPostsSeperated skipBlock simpleSkipItem

skipBlock :: Parser ()
skipBlock = openCurly *> mostPostsSeperated skipBlock simpleSkipBlock *> closeCurly

recovCurlyItem :: Parser a -> Parser (Either PoisonID a)
recovCurlyItem px = curlyItem *> Recov (id <$ skipItem) px

curlyKeyword kw = equal (CurlyItemKeyword kw)

optCurlyKeyword kw = Step (show kw) $ \case
    CurlyItemKeyword kw' -> if kw == kw'
        then Just ()
        else Nothing
    Keyword kw' -> if kw == kw'
        then Just ()
        else Nothing
    _ -> Nothing

closeCurly = equal CloseCurly

openRound = equal OpenRound

closeRound = equal CloseRound

openSquare = equal OpenSquare

closeSquare = equal CloseSquare

-- small_name
snake = Step "snake case identifier" $ \case
    Snake iden -> Just iden
    _ -> Nothing

-- LargeName
pascal = Step "pascal case identifier" $ \case
    Pascal iden -> Just iden
    _ -> Nothing

-- <$>
symbol = Step "symbolic identifier" $ \case
    Symbol iden -> Just iden
    _ -> Nothing

anyOperator = Step "operator" $ \case
    Symbol iden        -> Just iden
    SpecialOperator op -> Just op
    _ -> Nothing

additiveOperator = Step "additive operator" $ \case
    SpecialOperator "+" -> Just "+"
    SpecialOperator "-" -> Just "-"
    _ -> Nothing

multiplicativeOperator = Step "multiplicative operator" $ \case
    SpecialOperator "*" -> Just "*"
    SpecialOperator "/" -> Just "/"
    _ -> Nothing

logicalOperator = Step "logical operator" $ \case
    SpecialOperator "==" -> Just "=="
    SpecialOperator "!=" -> Just "!="
    _ -> Nothing

-- @place
label = keyword "@" *> snake

newtype WithBindings = WithBindings {
    bindings :: [(String, Expr)]
} deriving Show

newtype Matcher a = Matcher (NonEmpty (a, Maybe WithBindings))
    deriving Show

data MatchGaurd = MatchGaurd (Matcher (NonEmpty Pattern)) Action
    deriving Show

data Map
    = Args (NonEmpty String) Expr
    | Gaurded (NonEmpty MatchGaurd)
    deriving Show

data Pattern
    = Wild
    | BindIdenConst String
    | BindIdenMut String
    | CurryCtor String [Pattern]
    | InfixCtor String Pattern Pattern
    deriving Show

data InductiveFlow
    = For String Expr Expr Action
    | Loop Expr
    | While Expr Expr Action
    | WhileIs (Matcher Pattern) Expr Expr Action
    deriving Show

data Expr
    = UseIden String
    | Ctor String
    | Member String Expr
    | Call Expr Expr
    | PartialCall Expr Expr -- skip the first arg
    | Block [Either PoisonID BlockStatement]
    | Array [Expr]
    | If Expr Action Action
    | IfIs (Matcher Pattern) Expr Action Action
    | Match (NonEmpty Expr) [MatchGaurd]
    | TryCatch Expr Map
    | Flow (Maybe String) InductiveFlow
    | Lambda Map
    | Unwrap Expr
    | Unit
    | Undefined -- ...
    deriving Show

data Action
    = Run Expr
    | Pass
    | Break (Maybe String) Expr
    | Continue (Maybe String)
    | Return Expr
    | Goto String [Expr]
    | Throw Expr
    deriving Show

data SwitchGaurd = SwitchGaurd (Matcher (String, [Pattern])) Action
    deriving Show

data BlockStatement
    = Act Action
    | Switch String [Expr] (NonEmpty SwitchGaurd)
    | Let (Matcher Pattern) Expr
    deriving Show

compactPattern :: Parser Pattern
compactPattern = alt $ (Wild <$ keyword "_") :| [
        openRound *> symbolicPattern <* closeRound,
        BindIdenConst <$> snake,
        keyword "mut" *> (BindIdenMut <$> snake)
    ]

curryPattern :: Parser Pattern
curryPattern = Alt compactPattern $ CurryCtor <$> pascal <*> most compactPattern

symbolicPattern :: Parser Pattern
symbolicPattern = associateLeft (flip InfixCtor) <$> mostSeperated symbol curryPattern

multiSymbolicPattern :: Parser (NonEmpty Pattern)
multiSymbolicPattern = mostPostsSeperated (keyword ";") symbolicPattern

withBindings :: Parser WithBindings
withBindings = keyword "with"
    *> openCurly
    *> (WithBindings <$> most (
        (,)
        <$> (curlyItem *> snake)
        <*> (keyword "=" *> expr)
    ))
    <* closeCurly

matcher :: Parser a -> Parser (Matcher a)
matcher px = fmap Matcher $ mostPostsSeperated (keyword "or") $ (,) <$> px <*> opt withBindings

patt :: Parser (Matcher Pattern)
patt = matcher symbolicPattern

multiPatt :: Parser (Matcher (NonEmpty Pattern))
multiPatt = matcher multiSymbolicPattern

compactExpr :: Parser Expr
compactExpr = alt $ (Undefined <$ keyword "...") :| [
        UseIden <$> snake,
        Ctor <$> pascal,
        openCurly *> (Block <$> most (recovCurlyItem blockStmt)) <* closeCurly,
        openRound *> optDefault Unit expr <* closeRound,
        openSquare *> Alt (Array [] <$ closeSquare) ((toList >>> Array) <$> mostPostsSeperated (keyword ",") expr <* closeSquare)
    ]

call :: Parser (Expr -> Expr)
call = openRound *> (foldl1' (>>>) <$> mostPostsSeperated (keyword ";") (flip Call <$> expr)) <* closeRound

chainLink :: Parser (Expr -> Expr)
chainLink = alt $ (Unwrap <$ keyword "?") :| [
        (>>>)
        <$> (keyword "." *> fmap Member snake)
        <*> optDefault id call,
        (>>>)
        <$> (keyword "?." *> fmap (Unwrap >>>) (Member <$> snake))
        <*> optDefault id call
    ]

chainExpr :: Bool -> Parser Expr
chainExpr is_start = (if is_start then Alt (flow False) else id) $ alt $ lambda :| [
        foldl (&) <$> compactExpr <*> most chainLink
    ]

curryExpr is_start = foldl1 Call <$> most1 (chainExpr is_start)

infixOperatorExpr :: Parser String -> (Bool -> Parser Expr) -> (Bool -> Parser Expr)
infixOperatorExpr po px is_start =
    foldl (&)
    <$> px is_start
    <*> most (
        (\o n p -> Call (Call (UseIden o) p) n)
        <$> po
        <*> (if is_start then Alt (flow False) else id) (px False)
    )

mulExpr = infixOperatorExpr multiplicativeOperator curryExpr

addExpr = infixOperatorExpr additiveOperator mulExpr

infExpr = infixOperatorExpr symbol addExpr

logicExpr = infixOperatorExpr logicalOperator infExpr

expr :: Parser Expr
expr = alt $ flow False :| [
        anyOperator &> optDefault UseIden (curryExpr True <&> \c o -> Call (UseIden o) c),
        logicExpr True &> optDefault id (keyword "$" *> (flip Call <$> expr))
    ]

doAction :: Parser Action
doAction = Alt (keyword "do" *> (Run <$> expr)) jump

action :: Parser Action
action = Alt (Run <$> expr) jump

jump = alt $ (Pass <$ keyword "pass") :| [
        keyword "break" *> (Break <$> opt label <*> optDefault Unit expr),
        keyword "continue" *> (Continue <$> opt label),
        keyword "return" *> (Return <$> expr),
        keyword "goto" *> (Goto <$> label <*> most (chainExpr True)),
        keyword "throw" *> (Throw <$> expr)
    ]

fatArr :: Parser Map
fatArr = Alt (Args <$> most1 snake <*> (keyword "=>" *> expr)) (Gaurded <$> most1 matchGaurd)

lambda = Lambda <$> (keyword "fn" *> fatArr)

ifCond False =
    (
        keyword "if" *>
            expr &>
            (maybe If IfIs <$> opt (keyword "is" *> patt))
    ) <*>
    doAction <*>
    optDefault Pass (Alt
        (keyword "else" *> action)
        (Run <$> elifCond False)
    )
ifCond True =
    (
        keyword "if" *>
            expr &>
            (maybe If IfIs <$> opt (keyword "is" *> patt))
    ) <*>
    doAction <*>
    optDefault Pass (Alt
        (optCurlyKeyword "else" *> action)
        (Run <$> alt (elifCond False :| [elifCond True]))
    )

elifCond stmt =
    (
        (if stmt then curlyKeyword else keyword) "elif" *>
        expr &>
        (maybe If IfIs <$> opt (keyword "is" *> patt))
    ) <*>
    doAction <*>
    optDefault Pass (Alt
        ((if stmt then curlyKeyword else keyword) "else" *> action)
        (Run <$> elifCond stmt)
    )

tryCatch stmt = TryCatch
    <$> (keyword "try" *> expr)
    <*> ((if stmt then curlyKeyword else keyword) "catch" *> fatArr)

matchGaurd :: Parser MatchGaurd
matchGaurd = MatchGaurd
    <$> (keyword "|" *> multiPatt)
    <*> (keyword "=>" *> action)

matchWith stmt = Match
    <$> (keyword "match" *> mostPostsSeperated (keyword ";") expr)
    <*> ((if stmt then optCurlyKeyword else keyword) "with" *> most matchGaurd)

flow stmt = alt $ (Flow <$> opt label <*> loopingFlow stmt) :| [
        ifCond stmt,
        tryCatch stmt,
        matchWith stmt
    ]
forIn stmt = For
    <$> (keyword "for" *> snake)
    <*> (keyword "in" *> expr)
    <*> (keyword "do" *> expr)
    <*> optDefault Pass ((if stmt then optCurlyKeyword else keyword) "nobreak" *> action)

loop = Loop <$> (keyword "loop" *> expr)

while stmt = (keyword "while" *> expr &> (maybe While WhileIs <$> opt (keyword "is" *> patt)))
    <*> (keyword "do" *> expr)
    <*> optDefault Pass ((if stmt then optCurlyKeyword else keyword) "nobreak" *> action)

loopingFlow stmt = alt $ forIn stmt :| [
        loop,
        while stmt
    ]

labelPattern :: Parser (String, [Pattern])
labelPattern = (,) <$> label <*> most compactPattern

switchGaurd :: Parser SwitchGaurd
switchGaurd = SwitchGaurd <$> (keyword "|" *> matcher labelPattern) <*> (keyword "=>" *> action)

switchWith = Switch <$> (keyword "switch" *> label) <*> most (chainExpr True) <*> (optCurlyKeyword "with" *> most1 switchGaurd)

letStmt = Let <$> (keyword "let" *> patt) <*> Alt (keyword "=" *> expr) (keyword "?=" *> fmap Unwrap expr)

blockStmt = alt $ ((Run >>> Act) <$> flow True) :| [
        switchWith,
        letStmt,
        Act <$> action
    ]