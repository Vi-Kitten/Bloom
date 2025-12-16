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
import Utils ((<+>), (&>), AlternatingList (..), AlternatingListSep (..), posts, associateLeft, associateRight)
import Data.Bifunctor (Bifunctor (..))
import Reporting (PoisonID, CompilerExcept, raiseInit)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Foldable1 (foldl1')
import Data.List.NonEmpty.Extra (cons)
import Control.Monad (join)
import GHC.TypeLits (Nat)

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
expectation (ex :| [ex']) = ex ++ " or " ++ ex'
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

optDefaultIf :: Bool -> a -> Parser a -> Parser a
optDefaultIf cond = if cond
    then optDefault
    else const id

mostSeperated :: Parser s -> Parser a -> Parser (AlternatingList s a)
mostSeperated py px = px &> Alt (flip (:-) <$> ((:+) <$> py <*> mostSeperated py px)) (pure End)

mostPostsSeperated :: Parser s -> Parser a -> Parser (NonEmpty a)
mostPostsSeperated py px = posts <$> mostSeperated py px

mostInfixLeft :: Parser (a -> a -> a) -> Parser a -> Parser a
mostInfixLeft py px = associateLeft (\x o y -> o x y) <$> mostSeperated py px

mostInfixRight :: Parser (a -> a -> a) -> Parser a -> Parser a
mostInfixRight py px = associateRight (\x o y -> o x y) <$> mostSeperated py px

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

recovCurly :: Parser a -> Parser [Either PoisonID a]
recovCurly px = openCurly *> most (recovCurlyItem px) <* closeCurly

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

comma = equal Comma

semicolon = equal Semicolon

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

comparisonOperator = Step "logical operator" $ \case
    SpecialOperator "==" -> Just "=="
    SpecialOperator "!=" -> Just "!="
    _ -> Nothing

-- @place
label = keyword "@" *> snake

data PatternBinding = PatternBinding String Expr
    deriving Show

newtype WithBindings = WithBindings {
    bindings :: [PatternBinding]
} deriving Show

data MatcherCase a = MatcherCase a (Maybe WithBindings)
    deriving Show

newtype Matcher a = Matcher (NonEmpty (MatcherCase a))
    deriving Show

data MatchGaurd = MatchGaurd (Matcher (NonEmpty Pattern)) Action
    deriving Show

data Map
    = Args (NonEmpty Arg) Expr
    | Gaurded (NonEmpty MatchGaurd)
    deriving Show

data SimpleLiteral
    = StringLit String
    | CharLit Char
    | NumberLit Nat
    deriving Show

data RefFlavour
    = Mut
    | Pin
    | Ref
    deriving Show

data Pattern
    = Wild
    | BindIden RefFlavour String
    | CurryCtor String [Pattern]
    | InfixCtor String Pattern Pattern
    | DestructureTuple (NonEmpty Pattern)
    | DestructureArray [Pattern]
    | ExpectLiteral SimpleLiteral
    | ThenBindIden RefFlavour Pattern String
    deriving Show

data Condition
    = TruthCheck Expr
    | PartialMatch Expr (Matcher Pattern)
    deriving Show

data InductiveFlow
    = For String Expr Expr Action
    | Loop Expr
    | While Condition Expr Action
    deriving Show

data Expr
    = UseIden String
    | Ctor String
    | Member String Expr
    | Call Expr Expr
    | PartialCall Expr Expr -- skip the first arg
    | Block [Either PoisonID BlockStatement]
    | If Condition Action Action
    | Match (NonEmpty Expr) [MatchGaurd]
    | TryCatch Expr Map
    | Flow (Maybe String) InductiveFlow
    | Lambda Map
    | Unwrap Expr
    | Unit
    | MakeTuple (NonEmpty Expr)
    | MakeArray [Expr]
    | ConstructLiteral SimpleLiteral
    | Borrow RefFlavour (NonEmpty String)
    | ThenRun Expr Expr
    | AnonInterface (NonEmpty BloomType) [Either PoisonID ImplMethod]
    | Coerce Expr BloomType
    | TypedHole -- ...
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
    | Let (Matcher Pattern) (Maybe BloomType) Expr (Maybe Action)
    deriving Show

flavour :: Parser RefFlavour
flavour = optDefault Ref mutableFlavour

mutableFlavour :: Parser RefFlavour
mutableFlavour = Alt (Mut <$ keyword "mut") (Pin <$ keyword "pin")

simpleLiteral :: Parser SimpleLiteral
simpleLiteral = alt $
    Step "string" ( \case
        StringLiteral str -> Just $ StringLit str
        _ -> Nothing
    ) :| [ Step "charecter" $ \case
        CharLiteral charecter -> Just $ CharLit charecter
        _ -> Nothing
    , Step "number" $ \case
        Natural n -> Just $ NumberLit n
        _ -> Nothing
    ]

compactPattern :: Parser Pattern
compactPattern = alt $ (Wild <$ keyword "_") :| [
        openRound *> freePattern <* closeRound,
        BindIden Ref <$> snake,
        openSquare *> fmap DestructureArray (most symbolicPattern) <* closeSquare,
        ExpectLiteral <$> simpleLiteral
    ]

curryPattern :: Parser Pattern
curryPattern = alt $ (
        BindIden <$> mutableFlavour <*> snake
    ) :| [
        compactPattern
    ,
        CurryCtor <$> pascal <*> most compactPattern
    ]

symbolicPattern :: Parser Pattern
symbolicPattern = mostInfixLeft (InfixCtor <$> symbol) curryPattern

freePattern :: Parser Pattern
freePattern = symbolicPattern &> optDefault id (Alt
        (fmap (\xs x -> DestructureTuple $ cons x xs) $ most1 $ comma *> symbolicPattern)
        (keyword "then" *> ((ThenBindIden >>> flip) <$> flavour <*> snake))
    )

multiTuplePattern :: Parser (NonEmpty Pattern)
multiTuplePattern = mostPostsSeperated semicolon freePattern

withBindings :: Parser WithBindings
withBindings = keyword "with"
    *> openCurly
    *> (WithBindings <$> most (
        PatternBinding
        <$> (curlyItem *> snake)
        <*> (keyword "=" *> expr)
    ))
    <* closeCurly

matcher :: Parser a -> Parser (Matcher a)
matcher px = fmap Matcher $ mostPostsSeperated (keyword "or") $ MatcherCase <$> px <*> opt withBindings

patt :: Parser (Matcher Pattern)
patt = matcher freePattern

multiPatt :: Parser (Matcher (NonEmpty Pattern))
multiPatt = matcher multiTuplePattern

refFlavour :: Parser RefFlavour
refFlavour = optDefault Ref (Alt (Mut <$ keyword "mut") (Pin <$ keyword "pin"))

borrow :: Parser Expr
borrow = Borrow
    <$> (keyword "&" *> refFlavour)
    <*> mostPostsSeperated (keyword ".") snake

compactExpr :: Parser Expr
compactExpr = alt $ (TypedHole <$ keyword "...") :| [
        UseIden <$> snake,
        Ctor <$> pascal,
        Block <$> recovCurly blockStmt,
        openRound *> optDefault Unit expr <* closeRound,
        openSquare *> Alt (MakeArray [] <$ closeSquare) ((toList >>> MakeArray) <$> mostPostsSeperated comma expr <* closeSquare),
        ConstructLiteral <$> simpleLiteral
    ]

call :: Parser (Expr -> Expr)
call = openRound *> (foldl1' (>>>) <$> mostPostsSeperated semicolon (flip Call <$> expr)) <* closeRound

chainLink :: Parser (Expr -> Expr)
chainLink = alt $ (Unwrap <$ keyword "?") :| [
        (>>>)
        <$> (keyword "." *> fmap Member snake)
        <*> optDefault id call,
        (>>>)
        <$> (keyword "?." *> fmap (Unwrap >>>) (Member <$> snake))
        <*> optDefault id call
    ]

inlineFlow :: Parser Expr
inlineFlow = alt $ flow False :| [lambda, anonInterface]

chainExpr :: Parser Expr
chainExpr = foldl (&) <$> compactExpr <*> most chainLink

flowChainExprs :: Parser (NonEmpty Expr)
flowChainExprs = Alt ((:| []) <$> inlineFlow) $ most1 chainExpr &> optDefault id (((:| []) >>> flip (<>)) <$> inlineFlow)

optFlowChainExprs :: Parser [Expr]
optFlowChainExprs = optDefault [] (toList <$> flowChainExprs)

curryExpr :: Bool -> Parser Expr
curryExpr is_start = Alt borrow $ foldl1 Call <$>
    (if is_start
        then Alt ((:| []) <$> inlineFlow) $ most1 chainExpr &> optDefault id (((:| []) >>> flip (<>)) <$> inlineFlow)
        else most1 chainExpr
    )

infixOperatorExpr :: Parser String -> (Bool -> Parser Expr) -> (Bool -> Parser Expr)
infixOperatorExpr po px is_start =
    foldl (&)
    <$> px is_start
    <*> infixOperators
    where
        infixOperators = optDefault [] $ fmap (UseIden >>> Call) po &> optDefaultIf is_start pure (Alt
                ((\fl o -> [o >>> flip Call fl]) <$> inlineFlow)
                ((\r xs o -> (o >>> flip Call r) : xs) <$> px False <*> infixOperators)
            )

mulExpr :: Bool -> Parser Expr
mulExpr = infixOperatorExpr multiplicativeOperator curryExpr

addExpr :: Bool -> Parser Expr
addExpr = infixOperatorExpr additiveOperator mulExpr

infixExpr :: Bool -> Parser Expr
infixExpr = infixOperatorExpr symbol addExpr

logicExpr :: Bool -> Parser Expr
logicExpr is_start = infixExpr is_start &> optDefault id ((\o r l -> Call (Call (UseIden o) l) r) <$> comparisonOperator <*> infixExpr False)

expr :: Parser Expr
expr = Alt (
        anyOperator &> optDefault UseIden (curryExpr True <&> \c o -> PartialCall (UseIden o) c)
    ) (
        logicExpr True &> optDefault id (alt $ (
                comma *> ((\xs x -> MakeTuple (cons x xs)) <$> mostPostsSeperated comma (logicExpr False))
            ) :| [
                keyword "$" *> (flip Call <$> expr)
            ,
                keyword "then" *> (flip ThenRun <$> expr)
            ,
                keyword "as" *> (flip Coerce <$> regularType)
            ]
        )
    )

doAction :: Parser Action
doAction = Alt (keyword "do" *> (Run <$> expr)) jump

action :: Parser Action
action = Alt (Run <$> expr) jump

jump :: Parser Action
jump = alt $ (Pass <$ keyword "pass") :| [
        keyword "break" *> (Break <$> opt label <*> optDefault Unit expr),
        keyword "continue" *> (Continue <$> opt label),
        keyword "return" *> (Return <$> expr),
        keyword "goto" *> (Goto <$> label <*> optFlowChainExprs),
        keyword "throw" *> (Throw <$> expr)
    ]

fatArr :: Parser Map
fatArr = Alt (Args <$> args1 <*> (keyword "=>" *> expr)) (Gaurded <$> most1 matchGaurd)

lambda :: Parser Expr
lambda = Lambda <$> (keyword "fn" *> fatArr)

anonInterface :: Parser Expr
anonInterface = AnonInterface <$> (keyword "new" *> mostPostsSeperated (keyword "and") regularType) <*> recovCurly implMethod

condition :: Parser Condition
condition = expr &> optDefault TruthCheck (keyword "is" *> patt <&> flip PartialMatch)

ifCond :: Bool -> Parser Expr
ifCond False =
    (
        keyword "if" *>
        (If <$> condition)
    ) <*>
    doAction <*>
    optDefault Pass (Alt
        (keyword "else" *> action)
        (Run <$> elifCond False)
    )
ifCond True =
    (
        keyword "if" *>
        (If <$> condition)
    ) <*>
    doAction <*>
    optDefault Pass (Alt
        (optCurlyKeyword "else" *> action)
        (Run <$> alt (elifCond False :| [elifCond True]))
    )

elifCond :: Bool -> Parser Expr
elifCond stmt =
    (
        (if stmt then curlyKeyword else keyword) "elif" *>
        (If <$> condition)
    ) <*>
    doAction <*>
    optDefault Pass (Alt
        ((if stmt then curlyKeyword else keyword) "else" *> action)
        (Run <$> elifCond stmt)
    )

tryCatch :: Bool -> Parser Expr
tryCatch stmt = TryCatch
    <$> (keyword "try" *> expr)
    <*> ((if stmt then curlyKeyword else keyword) "catch" *> fatArr)

matchGaurd :: Parser MatchGaurd
matchGaurd = MatchGaurd
    <$> (keyword "|" *> multiPatt)
    <*> (keyword "=>" *> action)

matchWith :: Bool -> Parser Expr
matchWith stmt = Match
    <$> (keyword "match" *> mostPostsSeperated semicolon expr)
    <*> ((if stmt then optCurlyKeyword else keyword) "with" *> most matchGaurd)

flow :: Bool -> Parser Expr
flow stmt = alt $ (Flow <$> opt label <*> loopingFlow stmt) :| [
        ifCond stmt,
        tryCatch stmt,
        matchWith stmt
    ]

forIn :: Bool -> Parser InductiveFlow
forIn stmt = For
    <$> (keyword "for" *> snake)
    <*> (keyword "in" *> expr)
    <*> (keyword "do" *> expr)
    <*> optDefault Pass ((if stmt then optCurlyKeyword else keyword) "nobreak" *> action)

loop :: Parser InductiveFlow
loop = Loop <$> (keyword "loop" *> expr)

while :: Bool -> Parser InductiveFlow
while stmt = (keyword "while" *> fmap While condition)
    <*> (keyword "do" *> expr)
    <*> optDefault Pass ((if stmt then optCurlyKeyword else keyword) "nobreak" *> action)

loopingFlow :: Bool -> Parser InductiveFlow
loopingFlow stmt = alt $ forIn stmt :| [
        loop,
        while stmt
    ]

labelPattern :: Parser (String, [Pattern])
labelPattern = (,) <$> label <*> most compactPattern

switchGaurd :: Parser SwitchGaurd
switchGaurd = SwitchGaurd <$> (keyword "|" *> matcher labelPattern) <*> (keyword "=>" *> action)

switchWith :: Parser BlockStatement
switchWith = Switch <$> (keyword "switch" *> label) <*> optFlowChainExprs <*> (optCurlyKeyword "with" *> most1 switchGaurd)

letStmt :: Parser BlockStatement
letStmt = Let
    <$> (keyword "let" *> patt)
    <*> opt typed
    <*> Alt (keyword "=" *> expr) (keyword "?=" *> fmap Unwrap expr)
    <*> opt (optCurlyKeyword "else" *> action)

blockStmt = alt $ ((Run >>> Act) <$> flow True) :| [
        switchWith,
        letStmt,
        Act <$> action
    ]

data BloomType
    = Named String [BloomType]
    | TupleType (NonEmpty BloomType)
    | Reference RefFlavour BloomType
    | OnlyPure BloomType
    | UnPin BloomType
    | Forall QuantifierBody BloomType
    | Exists QuantifierBody BloomType
    | UnitType
    | ChiralProduct BloomType BloomType
    deriving Show

data Variance
    = MixedVariance
    | Covariant
    | Contravariant
    deriving Show

data Arg
    = SimpleArg String
    | PatternArg Pattern (Maybe BloomType)
    deriving Show

variance :: Parser Variance
variance = optDefault MixedVariance $ Alt (Covariant <$ keyword "out") (Contravariant <$ keyword "in")

compactType :: Parser BloomType
compactType = Alt
    (Named <$> pascal <*> optDefault [] (fmap toList $ openSquare *> most1 compactType <* closeSquare))
    (openRound *> optDefault UnitType freeType <* closeRound)

functionArrow :: Parser (BloomType -> BloomType -> BloomType)
functionArrow = fmap (\name arg ret -> Named name [arg, ret]) $ alt $
    (
        "->" <$ keyword "->"
    ) :| [
        "~>" <$ keyword "~>"
    ,
        "-+" <$ keyword "-+"
    ,
        "-*" <$ keyword "-*"
    ]

typePrefix :: Parser (BloomType -> BloomType)
typePrefix = alt $ (keyword "&" *> refFlavour <&> Reference) :| [
        OnlyPure <$ keyword "pure"
    ,
        UnPin <$ keyword "unpin"
    ]

elaboratedType :: Parser BloomType
elaboratedType = flip (foldr ($)) <$> most typePrefix <*> compactType

functionType :: Parser BloomType
functionType = mostInfixRight functionArrow elaboratedType

typeQuantifier :: Parser (BloomType -> BloomType)
typeQuantifier = Alt
    (keyword "for" *> quantifierBody <&> Forall)
    (keyword "dyn" *> quantifierBody <&> Exists)

regularType :: Parser BloomType
regularType = flip (foldr ($)) <$> most typeQuantifier <*> functionType

freeType :: Parser BloomType
freeType = regularType &> optDefault id (Alt
        ((\xs x -> TupleType $ x :| xs) <$> most (comma *> regularType))
        ((\xs x -> foldr1 ChiralProduct $ x :| xs ) <$> most (keyword "then" *> regularType))
    )

typed :: Parser BloomType
typed = keyword ":" *> regularType

-- singleArg = Alt (SimpleArg <$> snake) $ openRound *> (PatternArg <$> tuplePattern <*> opt typed) <* closeRound

multiArg :: Parser (NonEmpty Arg)
multiArg = Alt ((BindIden Ref >>> flip PatternArg Nothing >>> (:| [])) <$> snake) $
    openRound *> mostPostsSeperated semicolon (PatternArg <$> freePattern <*> opt typed) <* closeRound

args1 :: Parser (NonEmpty Arg)
args1 = join <$> most1 multiArg

args :: Parser [Arg]
args = join <$> most (toList <$> multiArg)

data KindDomain = KindDomain Variance BloomKind
    deriving Show

data KindArgument = KindArgument Variance BloomKind String
    deriving Show

data QuantifierArgument = QuantifierArgument BloomKind String
    deriving Show

data BloomKind
    = TypeGen [KindDomain]
    deriving Show

compactKind :: Parser BloomKind
compactKind = fmap TypeGen $ keyword "type" *> optKindPack kindDomain

kindDomain :: Parser KindDomain
kindDomain = KindDomain <$> variance <*> compactKind

kindArgument :: Parser KindArgument
kindArgument = KindArgument <$> variance <*> optDefault (TypeGen []) compactKind <*> pascal

quantifierArgument :: Parser QuantifierArgument
quantifierArgument = QuantifierArgument <$> optDefault (TypeGen []) compactKind <*> pascal

kindPack :: Parser a -> Parser (NonEmpty a)
kindPack px = openSquare *> mostPostsSeperated comma px <* closeSquare

optKindPack :: Parser a -> Parser [a]
optKindPack px = optDefault [] (toList <$> kindPack px)

data QuantifierConstraint
    = IsInterface String BloomType
    deriving Show

data QuantifierBody = QuantifierBody (NonEmpty QuantifierArgument) [QuantifierConstraint]
    deriving Show

quantifierConstraint :: Parser QuantifierConstraint
quantifierConstraint = IsInterface <$> pascal <*> (keyword "is" *> regularType)

typeConstraints :: Parser (NonEmpty QuantifierConstraint)
typeConstraints = openSquare *> mostPostsSeperated comma quantifierConstraint <* closeSquare

optTypeLevelIf :: Parser [QuantifierConstraint]
optTypeLevelIf = optDefault [] $ toList <$> (keyword "if" *> typeConstraints)

quantifierBody :: Parser QuantifierBody
quantifierBody = openSquare *> (QuantifierBody
        <$> mostPostsSeperated comma quantifierArgument
        <*> optDefault [] (toList <$> mostPostsSeperated comma quantifierConstraint)
    ) <* closeSquare

data MethodFlavour
    = CallByRef RefFlavour
    | CallByMove
    deriving Show

data ImplMethod
    = ImplMethod String [Pattern] Expr
    deriving Show

data ImplBlock = ImplBlock BloomType [QuantifierConstraint] [Either PoisonID ImplMethod]
    deriving Show

-- type statement
data ADTStatement
    = DefineMethod MethodFlavour String (Maybe QuantifierBody) [Arg] (Maybe BloomType) (Maybe Expr)
    | InterfaceImpl (Maybe RefFlavour) ImplBlock
    deriving Show

data CaseArg
    = Field (Maybe String) BloomType
    | Super (Maybe String) BloomType
    | ImplWith (NonEmpty BloomType) (Maybe String) BloomType
    deriving Show

data StaticStatement
    = Define String (Maybe QuantifierBody) [Arg] (Maybe BloomType) (Maybe Expr)
    | Struct String [KindArgument] [CaseArg] [Either PoisonID ADTStatement]
    | InfixStruct String [KindArgument] CaseArg CaseArg [Either PoisonID ADTStatement]
    deriving Show

defName :: Parser String
defName = Alt snake $ openRound *> symbol <* closeRound

define :: Parser StaticStatement
define = Define
    <$> (keyword "def" *> defName)
    <*> opt quantifierBody
    <*> args
    <*> opt typed
    <*> opt (keyword "=" *> expr)

caseArg :: Parser CaseArg
caseArg = Alt (Field Nothing <$> compactType) $ openRound *> alt (
        (Field <$> fmap Just snake <*> typed)
    :| [
        keyword "super" *> (Super <$> opt (snake <* keyword ":") <*> regularType)
    ,
        keyword "impl" *> (ImplWith <$> mostPostsSeperated (keyword "and") regularType <*> opt (snake <* keyword ":") <*> regularType)
    ]) <* closeRound

struct :: Parser StaticStatement
struct = (keyword "struct" *>) $ Alt
    (Struct <$> snake <*> optKindPack kindArgument <*> most caseArg <*> recovCurly structBodyStatement)
    (InfixStruct <$> symbol <*> optKindPack kindArgument <*> caseArg <*> caseArg <*> recovCurly structBodyStatement)

methodFlavour :: Parser MethodFlavour
methodFlavour = optDefault CallByMove $ keyword "ref" *> refFlavour <&> CallByRef

structBodyStatement :: Parser ADTStatement
structBodyStatement = defineMethod

defineMethod :: Parser ADTStatement
defineMethod = DefineMethod
    <$> methodFlavour
    <*> (keyword "def" *> defName)
    <*> opt quantifierBody
    <*> args
    <*> opt typed
    <*> opt (keyword "=" *> expr)

implMethod :: Parser ImplMethod
implMethod = ImplMethod <$> (keyword "." *> snake) <*> most compactPattern <*> (keyword "=" *> expr)

implBlock :: Parser ImplBlock
implBlock = ImplBlock <$> (keyword "impl" *> regularType) <*> optTypeLevelIf <*> recovCurly implMethod

dataImplInterface :: Parser ADTStatement
dataImplInterface = InterfaceImpl <$> opt (keyword "ref" *> refFlavour) <*> implBlock