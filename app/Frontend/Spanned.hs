module Frontend.Spanned (
    TextPos (..),
    Span (..),
    Spanned (..),
    coalesceWith,
    coalesce,
    spanStart,
    spanEnd,
    startPoint,
    endPoint
) where

import GHC.TypeLits (Nat)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty (..))

data TextPos = TextPos {
    line :: Nat,
    char :: Nat
}
    deriving Eq

instance Show TextPos where
    show (TextPos l c) = "(" ++ show l ++ ":" ++ show c ++ ")"

instance Ord TextPos where
    compare x y = case (compare `on` line) x y of
        LT -> LT
        EQ -> (compare `on` char) x y
        GT -> GT

data Span
    = Range TextPos TextPos
    | Point TextPos
    deriving Eq

instance Show Span where
    show (Range (TextPos l c) (TextPos l' c')) = "(" ++ show l ++ ":" ++ show c ++ " - " ++ show l' ++ ":" ++ show c' ++ ")"
    show (Point pos) = show pos

spanStart :: Span -> TextPos
spanStart (Range s _) = s
spanStart (Point p) = p

startPoint :: Span -> Span
startPoint s = Point (spanStart s)

spanEnd :: Span -> TextPos
spanEnd (Range _ e) = e
spanEnd (Point p) = p

endPoint :: Span -> Span
endPoint s = Point (spanEnd s)

instance Semigroup Span where
    Point p <> Point q
        | p == q    = Point p
        | otherwise = Range (min p q) (max p q)
    x <> y = Range ((min `on` spanStart) x y) ((max `on` spanEnd) x y)

data Spanned a = (:@) Span a

instance Show a => Show (Spanned a) where
    show (s :@ x) = show s ++ " " ++ show x

instance Functor Spanned where
    fmap f (s :@ x) = s :@ f x

instance Semigroup a => Semigroup (Spanned a) where
    s :@ x <> s' :@ y = (s <> s') :@ (x <> y)

coalesceWith :: Span -> [Spanned a] -> Spanned [a]
coalesceWith s [] = s :@ []
coalesceWith s (s' :@ x : xs) = (x :) <$> coalesceWith (s <> s') xs

coalesce :: NonEmpty (Spanned a) -> Spanned (NonEmpty a)
coalesce (s :@ x :| xs) = (x :|) <$> coalesceWith s xs