module Parser.Spanned (
    TextPos (..),
    Span (..),
    Spanned (..)
) where

import GHC.TypeLits (Nat)
import Data.Function (on)

data TextPos = TextPos { line :: Nat, char :: Nat }
    deriving Eq

instance Ord TextPos where
    compare x y = case (compare `on` line) x y of
        LT -> LT
        EQ -> (compare `on` char) x y
        GT -> GT

data Span = Span { start :: TextPos, end :: TextPos }

instance Semigroup Span where
    x <> y = Span {
        start = (min `on` start) x y,
        end = (max `on` end) x y
    }

data Spanned a = (:@) Span a