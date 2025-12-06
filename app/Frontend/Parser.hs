{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
module Frontend.Parser () where
import Data.Kind (Type)

import Frontend.Lexer (Token)
import Frontend.Spanned (Spanned (..))
import Control.Arrow ((>>>))
import Data.Function (on, (&))
import GHC.Base (NonEmpty (..))
import Utils ((<+>))
import Data.Bifunctor (Bifunctor(..))

type ParseExpectation = String

data Parser :: Type -> Type where
    Step :: ParseExpectation -> (Token -> Maybe a) -> Parser a
    Pure :: a                                      -> Parser a
    Alt  :: Parser a        -> Parser a            -> Parser a
    Then :: Parser (a -> b) -> Parser a            -> Parser b
    -- Recov :: Parser (Either PoisonId a -> b) -> Parser a -> Parser b

instance Functor Parser where
    fmap f (Step ex select) = Step ex $ select >>> fmap f
    fmap f (Pure x)      = Pure $ f x
    fmap f (Alt px py)   = (Alt `on` fmap f) px py
    fmap f (Then pf px)  = Then ((>>> f) <$> pf) px

instance Applicative Parser where
    pure = Pure
    (<*>) = Then

data Munch a
    = Already a
    | Accept a
    | Continue (Parser a)

munch :: Token -> Parser a -> Either (NonEmpty ParseExpectation) (Munch a)
munch tok (Step ex select) = case select tok of
    Nothing -> Left $ ex :| []
    Just x -> Right $ Accept x
munch _ (Pure x) = Right $ Already x
munch tok (Alt px py) = munch tok px <+> munch tok py
munch tok (Then pf px) = pf & munch tok >>= \case
    Already f    -> f <$> px & munch tok
    Accept  f    -> Right (Continue $ f <$> px)
    Continue pf' -> Right (Continue $ Then pf' px)

close :: Parser a -> Either (NonEmpty ParseExpectation) a
close (Step ex _) = Left $ ex :| []
close (Pure x) = Right x
close (Alt px py) = close px <+> close py
close (Then pf px) = close pf <*> close px

data ParseError
    = UnexpectedEOF (NonEmpty ParseExpectation)
    | ExpectedFound (Spanned Token) (NonEmpty ParseExpectation)
    | Unexpected (Spanned Token)

parse :: [Spanned Token] -> Parser a -> Either ParseError a
parse []                px = first UnexpectedEOF $ close px
parse (s :@ tok : toks) px = first (ExpectedFound $ s :@ tok) (px & munch tok) >>= \case
    Already _    -> Left $ Unexpected (s :@ tok)
    Accept x     -> parse toks (Pure x)
    Continue px' -> parse toks px'