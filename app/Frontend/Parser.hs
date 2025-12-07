{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
module Frontend.Parser (
    Parser,
    ParseError,
    parse,
    exampleParser
) where
import Data.Kind (Type)

import Frontend.Lexer (Token (..))
import Frontend.Spanned (Spanned (..))
import Control.Arrow ((>>>))
import Data.Function (on, (&))
import Data.List.NonEmpty (NonEmpty (..), last, init)
import Utils ((<+>))
import Data.Bifunctor (Bifunctor (..))
import Reporting (PoisonID, CompilerExcept, raise, raiseInit)
import Data.Functor ((<&>))

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
    | Continue (CompilerExcept (Parser a))

munch :: Spanned Token -> Parser a -> Either (NonEmpty ParseExpectation) (Munch a)
munch (_ :@ tok) (Step ex select) = case select tok of
    Nothing -> Left $ ex :| []
    Just x -> Right $ Continue $ pure $ Pure x
munch _ (Pure x) = Right $ Already $ pure x
munch stok (Alt px py) = munch stok px <+> munch stok py
munch stok (Then pf px) = pf & munch stok >>= \case
    Already f    -> px & munch stok <&> \case
        Already x    -> Already $ f <*> x
        Continue px' -> Continue $ (<$>) <$> f <*> px'
    Continue pf' -> Right (Continue $ pf' <&> (`Then` px))
munch (s :@ tok) (Recov pr px) = pr & munch (s :@ tok) <&> \case
    Already recov -> Already $ case px & close of
        Left exs -> do
            poison_id <- raiseInit "WIP MESSAGE"
            (Left poison_id &) <$> recov
        Right x  -> recov <*> (Right <$> x)
    Continue pr' -> Continue $ case px & munch (s :@ tok) of
        Left exs             -> do
            poison_id <- raiseInit "WIP MESSAGE"
            fmap (Left poison_id &) <$> pr'
        Right (Already _)    -> do
            poison_id <- raiseInit "WIP MESSAGE"
            fmap (Left poison_id &) <$> pr'
        Right (Continue px') -> Recov <$> pr' <*> px'

close :: Parser a -> Either (NonEmpty ParseExpectation) (CompilerExcept a)
close (Step ex _) = Left $ ex :| []
close (Pure x) = Right $ pure x
close (Alt px py) = close px <+> close py
close (Then pf px) = (<*>) <$> close pf <*> close px
close (Recov pr px) = close pr <&> \recov -> case px & close of
    Left exs -> do
        poison_id <- raiseInit "WIP MESSAGE"
        (Left poison_id &) <$> recov
    Right x -> x >>= \x' -> (Right x' &) <$> recov

data ParseError
    = UnexpectedEOF (NonEmpty ParseExpectation)
    | ExpectedFound (Spanned Token) (NonEmpty ParseExpectation)
    | Unexpected (Spanned Token)

expectation :: NonEmpty ParseExpectation -> String
expectation (ex :| []) = ex
expectation exs = (Data.List.NonEmpty.init exs >>= \ex -> ex ++ ", ") ++ "or " ++ Data.List.NonEmpty.last exs 

instance Show ParseError where
    show (UnexpectedEOF exs) = "Expected " ++ expectation exs
    show (ExpectedFound (s :@ tok) exs) = "Expected " ++ expectation exs ++ " found " ++ show tok ++ " at " ++ show s
    show (Unexpected (s :@ tok)) = "Unexpected token " ++ show tok ++ " at " ++ show s

parse :: [Spanned Token] -> Parser a -> CompilerExcept (Either ParseError a)
parse []                px = sequenceA $ first UnexpectedEOF $ close px
parse (s :@ tok : toks) px = case px & munch (s :@ tok) of
    Left exs -> return $ Left (ExpectedFound (s :@ tok) exs)
    Right (Already _) -> return $ Left (Unexpected (s :@ tok))
    Right (Continue px') -> px' >>= parse toks

equal :: Token -> Parser ()
equal tok = Step (show tok) $ \tok' -> if tok' == tok
    then Just ()
    else Nothing

-- pass
keyword kw = equal (Keyword kw)

openCurly = equal OpenCurly

curlyItem = Step "matching indentation" $ \case
    CurlyItem -> Just ()
    _ -> Nothing

curlyKeyword kw = equal (CurlyItemKeyword kw)

closeCurly = equal CloseCurly

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

-- @place
label = keyword "@" *> snake

def = curlyItem *> keyword "def"

exampleParser = curlyItem *> label