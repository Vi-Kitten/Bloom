{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE ViewPatterns #-}

module Utils (
    associateLeft,
    associateRight,
    associateLeft',
    associateRight',
    AlternatingListSep (..),
    AlternatingList (..),
    (+-),
    Or (..),
    pattern OrLeft,
    pattern OrRight,
    runSeperately,
    maybeLeft,
    maybeRight,
    choose,
    chooseLeft,
    chooseRight,
    fromExclusie
) where

import Data.Bifunctor (Bifunctor (..))
import Data.Functor ((<&>))

infixr 5 :+

data AlternatingListSep s a = (:+) s (AlternatingList s a)

infixr 5 :-

data AlternatingList s a
    = End a
    | (:-) a (AlternatingListSep s a)

infixr 4 +-

(+-) :: AlternatingList s a -> AlternatingListSep s a -> AlternatingList s a
End x         +- yxs = x :- yxs
x :- y :+ xys +- yxs = x :- y :+ (xys +- yxs)

bindAlternatingList :: AlternatingList s a -> (a -> AlternatingList s b) -> AlternatingList s b
bindAlternatingList (End x)         f = f x
bindAlternatingList (x :- y :+ xys) f = f x +- y :+ bindAlternatingList xys f

associateLeft' :: (a -> b) -> (s -> a -> b -> b) -> AlternatingList s a -> b
associateLeft' rgh _           (End a) = rgh a
associateLeft' rgh sep (x :- y :+ xys) = sep y x (associateLeft' rgh sep xys)

associateRight' :: (a -> b) -> (s -> b -> a -> b) -> AlternatingList s a -> b
associateRight' lft _           (End a) = lft a
associateRight' lft sep (x :- y :+ xys) = associateRight' (sep y (lft x)) sep xys

associateLeft :: (s -> a -> a -> a) -> AlternatingList s a -> a
associateLeft = associateLeft' id

associateRight :: (s -> a -> a -> a) -> AlternatingList s a -> a
associateRight = associateRight' id

instance Functor (AlternatingListSep s) where
    fmap f (y :+ xys) = y :+ fmap f xys

instance Functor (AlternatingList s) where
    fmap f (x :- yxs) = f x :- fmap f yxs
    fmap f (End x)    = End $ f x

instance Bifunctor AlternatingListSep where
    bimap f g (y :+ xys) = f y :+ bimap f g xys

instance Bifunctor AlternatingList where
    bimap f g (x :- yxs) = g x :- bimap f g yxs
    bimap _ g (End x)    = End $ g x

instance Applicative (AlternatingList s) where
    pure = End
    fys <*> xys = bindAlternatingList fys $ \f -> xys <&> f 

instance Monad (AlternatingList s) where
    (>>=) = bindAlternatingList

-- | An inclusive or type.
data Or a b
    = Both a b
    | JustLeft a
    | JustRight b

instance Functor (Or a) where
    fmap f (Both x y)    = Both x $ f y
    fmap _ (JustLeft x)  = JustLeft x
    fmap f (JustRight y) = JustRight $ f y

instance Bifunctor Or where
    bimap f g (Both x y)    = Both (f x) (g y)
    bimap f _ (JustLeft x)  = JustLeft  $ f x
    bimap _ g (JustRight y) = JustRight $ g y

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
    Both x y    <> Both x' y'   = Both (x <> x') (y <> y')
    Both x y    <> JustLeft x'  = Both (x <> x') y
    Both x y    <> JustRight y' = Both x (y <> y')
    JustLeft x  <> Both x' y'   = Both (x <> x') y'
    JustLeft x  <> JustLeft x'  = JustLeft (x <> x')
    JustLeft x  <> JustRight y' = Both x y'
    JustRight y <> Both x' y'   = Both x' (y <> y')
    JustRight y <> JustLeft x'  = Both x' y
    JustRight y <> JustRight y' = JustRight (y <> y') 

-- | Treat the `Or` as a pair of `Maybe`.
runSeperately :: (Maybe a -> Maybe b -> c) -> Or a b -> c
runSeperately f (Both x y) = f (Just x) (Just y)
runSeperately f (JustLeft x) = f (Just x) Nothing
runSeperately f (JustRight y) = f Nothing (Just y)

maybeLeft :: Or a b -> Maybe a
maybeLeft = runSeperately $ \x _ -> x

maybeRight :: Or a b -> Maybe b
maybeRight = runSeperately $ \_ y -> y

-- | Match any pattern that defines left, canonical pattern is `JustLeft`.
pattern OrLeft :: a -> Or a b
pattern OrLeft x <- (maybeLeft -> Just x) where
    OrLeft x = JustLeft x

-- | Match any pattern that defines right, canonical pattern is `JustRight`.
pattern OrRight :: b -> Or a b
pattern OrRight y <- (maybeRight -> Just y) where
    OrRight x = JustRight x

{-# COMPLETE OrLeft, JustRight #-}
{-# COMPLETE JustLeft, OrRight #-}

{-# COMPLETE OrLeft, OrRight #-}

-- | Decide how to resolve the `Both` pattern.
choose :: (a -> b -> Either a b) -> Or a b -> Either a b
choose f (Both x y) = f x y
choose _ (JustLeft x) = Left x
choose _ (JustRight y) = Right y

chooseLeft :: Or a b -> Either a b
chooseLeft = choose $ \x _ -> Left x

chooseRight :: Or a b -> Either a b
chooseRight = choose $ \_ y -> Right y

fromExclusie :: Either a b -> Or a b
fromExclusie (Left x) = JustLeft x
fromExclusie (Right y) = JustRight y