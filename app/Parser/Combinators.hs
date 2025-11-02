{-# LANGUAGE TupleSections #-}
module Parser.Combinators (
    expectPred,
    expectPredSpanned,
    expectEqual,
    expectEqualSpanned,
    expectNotequal,
    expectNotequalSpanned,
    expectAny,
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

import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty (..))
import Parser (ParserT, expect)
import Control.Applicative (Alternative (..))
import Parser.Spanned (Spanned (..))
import Utils(AlternatingList (..), AlternatingListSep (..))    

expectPred :: (a -> Bool) -> ParserT a () m a
expectPred p = expect $ \c -> if p c
    then Right c
    else Left ()

expectPredSpanned :: (a -> Bool) -> ParserT (Spanned a) () m (Spanned a)
expectPredSpanned p = expect $ \(s :@ c) -> if p c
    then Right $ s :@ c
    else Left ()

expectEqual :: Eq a => a -> ParserT a () m a
expectEqual c = expect $ \c' -> if c == c'
    then Right c'
    else Left ()

expectEqualSpanned :: Eq a => a -> ParserT (Spanned a) () m (Spanned a)
expectEqualSpanned c = expect $ \(s :@ c') -> if c == c'
    then Right $ s :@ c'
    else Left ()

expectNotequal :: Eq a => a -> ParserT a () m a
expectNotequal c = expect $ \c' -> if c /= c'
    then Right c'
    else Left ()

expectNotequalSpanned :: Eq a => a -> ParserT (Spanned a) () m (Spanned a)
expectNotequalSpanned c = expect $ \(s :@ c') -> if c /= c'
    then Right $ s :@ c'
    else Left ()

expectAny :: ParserT t e m t
expectAny = expect Right

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