{-# LANGUAGE TupleSections #-}
module Parser.Combinators (
    AlternatingListSep (..),
    AlternatingList (..),
    associateLeft',
    associateRight',
    associateLeft,
    associateRight,
    (+-),
    opt,
    leastUntil',
    leastUntil,
    least,
    least1Until',
    least1Until,
    least1,
    leastAltUntil',
    leastAltUntil,
    leastAlt,
    mostAltUntil',
    mostAltUntil,
    mostAlt
) where

import Data.Bifunctor (Bifunctor (..))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty (..))
import Parser (ParserT)
import Control.Applicative (Alternative(..))

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

opt :: Functor m => ParserT t e m a -> ParserT t e m (Maybe a)
opt px = (px <&> Just) <|> pure Nothing

leastUntil' :: Functor m => ParserT t e m b -> ParserT t e m a -> ParserT t e m ([a], b)
leastUntil' pe px = fmap ([],) pe
    <|> do
        x <- px
        (xs, end) <- leastUntil' pe px
        return (x : xs, end)

leastUntil :: Functor m => ParserT t e m b -> ParserT t e m a -> ParserT t e m [a]
leastUntil pe px = leastUntil' pe px <&> fst

least :: Functor m => ParserT t e m a -> ParserT t e m [a]
least = leastUntil $ pure ()

least1Until' :: Functor m => ParserT t e m b -> ParserT t e m a -> ParserT t e m (NonEmpty a, b)
least1Until' pe px = do
    x <- px
    (xs, end) <- leastUntil' pe px
    return (x :| xs, end)

least1Until :: Functor m => ParserT t e m b -> ParserT t e m a -> ParserT t e m (NonEmpty a)
least1Until pe px = least1Until' pe px <&> fst

least1 :: Functor m => ParserT t e m a -> ParserT t e m (NonEmpty a)
least1 = least1Until $ pure ()

leastAltUntil' :: Functor m => ParserT t e m s -> ParserT t e m b -> ParserT t e m a -> ParserT t e m (AlternatingList s a, b)
leastAltUntil' py pe px = do
    x <- px
    branch <- (pe <&> Left) <|> (py <&> Right)
    case branch of
        Left end -> return (End x, end)
        Right y -> do
            (xys, end) <- mostAltUntil' py pe px
            return (x :- y :+ xys, end)

leastAltUntil :: Functor m => ParserT t e m s -> ParserT t e m b -> ParserT t e m a -> ParserT t e m (AlternatingList s a)
leastAltUntil py pe px = leastAltUntil' py pe px <&> fst

leastAlt :: Functor m => ParserT t e m s -> ParserT t e m a -> ParserT t e m (AlternatingList s a)
leastAlt py = leastAltUntil py $ pure ()

mostAltUntil' :: Functor m => ParserT t e m s -> ParserT t e m b -> ParserT t e m a -> ParserT t e m (AlternatingList s a, b)
mostAltUntil' py pe px = do
    x <- px
    branch <- (py <&> Left) <|> (pe <&> Right)
    case branch of
        Left y -> do
            (xys, end) <- mostAltUntil' py pe px
            return (x :- y :+ xys, end)
        Right end -> return (End x, end)

mostAltUntil :: Functor m => ParserT t e m s -> ParserT t e m b -> ParserT t e m a -> ParserT t e m (AlternatingList s a)
mostAltUntil py pe px = mostAltUntil' py pe px <&> fst

mostAlt :: Functor m => ParserT t e m s -> ParserT t e m a -> ParserT t e m (AlternatingList s a)
mostAlt py = mostAltUntil py $ pure ()