module Parser.Spanned (
    TextPos (..),
    Span (..),
    Spanned (..),
    coalesceWith,
    coalesce
) where

import GHC.TypeLits (Nat)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty (..))

data TextPos = TextPos {
    line :: Nat,
    char :: Nat
}
    deriving (Eq, Show)

instance Ord TextPos where
    compare x y = case (compare `on` line) x y of
        LT -> LT
        EQ -> (compare `on` char) x y
        GT -> GT

data Span = Span {
    start :: TextPos,
    end :: TextPos
}
    deriving (Eq, Show)

instance Semigroup Span where
    x <> y = Span {
        start = (min `on` start) x y,
        end = (max `on` end) x y
    }

data Spanned a = (:@) Span a
    deriving Show

instance Functor Spanned where
    fmap f (s :@ x) = s :@ f x

coalesceWith :: Span -> [Spanned a] -> Spanned [a]
coalesceWith s [] = s :@ []
coalesceWith s (s' :@ x : xs) = (x :) <$> coalesceWith (s <> s') xs

coalesce :: NonEmpty (Spanned a) -> Spanned (NonEmpty a)
coalesce (s :@ x :| xs) = (x :|) <$> coalesceWith s xs