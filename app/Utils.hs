{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE ViewPatterns #-}

module Utils (
    len,
    (.&),
    (..&),
    (...&),
    (&>),
    associateLeft,
    associateRight,
    associateLeft',
    associateRight',
    AlternatingListSep (..),
    AlternatingList (..),
    (+-),
    fences,
    posts,
    (<:>),
    Or (..),
    pattern OrLeft,
    pattern OrRight,
    runSeperately,
    maybeLeft,
    maybeRight,
    choose,
    chooseLeft,
    chooseRight,
    fromExclusie,
    orElse,
    (<+>),
    (<%>),
    both,
    mapBoth,
    collectLeft,
    collectRight,
    collect
) where

import Data.Bifunctor (Bifunctor (..))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty (..), cons, nonEmpty)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Function ((&))
import GHC.Natural (Natural)

len :: (Foldable f) => f a -> Natural
len = foldr (const (+ 1)) 0

infix 1 .&
infix 1 ..&
infix 1 ...&

(.&)   :: (a, b)       -> (a -> b -> c)           -> c
(..&)  :: (a, b, c)    -> (a -> b -> c -> d)      -> d
(...&) :: (a, b, c, d) -> (a -> b -> c -> d -> e) -> e

(.&)   (x, y) f       = f x y
(..&)  (x, y, z) f    = f x y z
(...&) (x, y, z, w) f = f x y z w

infixl 4 &>

(&>) :: Applicative f => f a -> f (a -> b) -> f b
(&>) fx ff = (&) <$> fx <*> ff

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

associateLeft' :: (a -> b) -> (b -> s -> a -> b) -> AlternatingList s a -> b
associateLeft' lft _           (End x) = lft x
associateLeft' lft sep (x :- y :+ xys) =
    let sx = lft x in
        associateLeft' (sep sx y) sep xys

associateRight' :: (a -> b) -> (a -> s -> b -> b) -> AlternatingList s a -> b
associateRight' rgh _           (End x) = rgh x
associateRight' rgh sep (x :- y :+ xys) = sep x y $ associateRight' rgh sep xys

associateLeft :: (a -> s -> a -> a) -> AlternatingList s a -> a
associateLeft = associateLeft' id

associateRight :: (a -> s -> a -> a) -> AlternatingList s a -> a
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

fences :: AlternatingList a b -> [a]
fences (_ :- y :+ xys) = y : fences xys
fences (End _) = []

posts :: AlternatingList a b -> NonEmpty b
posts (x :- _ :+ xys) = cons x (posts xys)
posts (End x) = x :| []

infixr 5 <:>

(<:>) :: Functor f => a -> f [a] -> f [a]
(<:>) x fxs = (x :) <$> fxs

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
chooseLeft (JustRight y) = Right y
chooseLeft (OrLeft x) = Left x

chooseRight :: Or a b -> Either a b
chooseRight (JustLeft x) = Left x
chooseRight (OrRight y) = Right y

fromExclusie :: Either a b -> Or a b
fromExclusie (Left x) = JustLeft x
fromExclusie (Right y) = JustRight y

orElse :: Or a b -> Or a b -> Or a b
orElse (Both x y) _ = Both x y
orElse (JustLeft x) (JustLeft _) = JustLeft x
orElse (JustLeft x) (OrRight y)  = Both x y
orElse (JustRight y) (JustRight _) = JustRight y
orElse (JustRight y) (OrLeft x)    = Both x y

both :: a -> b -> Or a b -> (a, b)
both x y = mapBoth (fromMaybe x) (fromMaybe y)

mapBoth :: (Maybe a -> c) -> (Maybe b -> d) -> Or a b -> (c, d)
mapBoth f g (Both x y)    = (f $ Just x, g $ Just y)
mapBoth f g (JustLeft x)  = (f $ Just x, g Nothing )
mapBoth f g (JustRight y) = (f Nothing , g $ Just y)

infixr 6 <+>

(<+>) :: Semigroup e => Either e a -> Either e a -> Either e a
(<+>) (Right x) _        = Right x
(<+>) _ (Right y)        = Right y
(<+>) (Left e) (Left e') = Left $ e <> e'

infixl 4 <%>

(<%>) :: Semigroup e => Either e (a -> b) -> Either e a -> Either e b
(<%>) (Right f) (Right x) = Right $ f x
(<%>) (Left e) (Left e')  = Left  $ e <> e'
(<%>) (Left e) _ = Left e
(<%>) _ (Left e) = Left e

collectLeft :: [Either a b] -> Either [a] (NonEmpty b)
collectLeft [] = Left []
collectLeft (Right y : es) = Right $ case collectLeft es of
    Left _   -> y :| []
    Right ys -> cons y ys
collectLeft (Left x : es) = first (x :) $ collectLeft es

collectRight :: [Either a b] -> Either (NonEmpty a) [b]
collectRight [] = Right []
collectRight (Left x : es) = Left $ case collectRight es of
    Left xs -> cons x xs
    Right _ -> x :| []
collectRight (Right y : es) =  second (y :) $ collectRight es

collect :: NonEmpty (Or a b) -> Or (NonEmpty a) (NonEmpty b)
collect (o :| os) =
    let xs = mapMaybe maybeLeft  os in
    let ys = mapMaybe maybeRight os in
    case o of
        Both x y -> Both (x :| xs) (y :| ys)
        JustLeft x -> maybe
            (JustLeft (x :| xs))
            (Both $ x :| xs)
            $ nonEmpty ys
        JustRight y -> maybe
            (JustRight (y :| ys))
            (flip Both $ y :| ys)
            $ nonEmpty xs