{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
module Parser (
    ParserT,
    Parser,
    ParseError,
    StrictParseError,
    foldParserT,
    foldParserTStrict,
    runParserT,
    runParserTStrict,
    runParser,
    runParserStrict,
    withToken,
    withError,
    trackTokens,
    expect
) where

import Control.Monad.Except (ExceptT)
import Control.Monad.State (MonadTrans (..))
import Data.Functor ((<&>))
import Data.Function (on)
import Control.Applicative (Alternative (..))
import Data.Kind (Type)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, maybeToExceptT)
import Control.Arrow ((>>>))
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Trans.Except (runExceptT, throwE)
import Control.Monad.State.Lazy (StateT(..))
import Data.Bifunctor (Bifunctor(..))
import Control.Monad.Morph (MFunctor (..))

data ParserT t e (m :: Type -> Type) a
    = Fail
    | Then (t -> Either e (ParserT t e m a))
    | Alt  (ParserT t e m a) (ParserT t e m a)
    | Pure a
    | Free (m (ParserT t e m a))

bindParser :: Functor m => ParserT t e m a -> (a -> ParserT t e m b) -> ParserT t e m b
bindParser Fail _ = Fail
bindParser (Then continue) f = Then $ continue >>> fmap (`bindParser` f)
bindParser (Alt px py) f = Alt (bindParser px f) (bindParser py f)
bindParser (Pure x) f = f x
bindParser (Free fpx) f = Free $ fpx <&> (`bindParser` f)

instance Functor m => Functor (ParserT t e m) where
    fmap f = (`bindParser` (f >>> Pure))

instance Functor m => Applicative (ParserT t e m) where
    pure = Pure
    pf <*> px = bindParser pf $ \f -> px <&> \x -> f x

instance Functor m => Alternative (ParserT t e m) where
    empty = Fail
    (<|>) = Alt

instance Functor m => Monad (ParserT t e m) where
    (>>=) = bindParser

instance MonadTrans (ParserT t e) where
    lift = fmap Pure >>> Free

instance MFunctor (ParserT t e) where
    hoist _ Fail = Fail
    hoist f (Then continue) = Then $ continue >>> fmap (hoist f)
    hoist f (Alt px py) = (Alt `on` hoist f) px py
    hoist _ (Pure x) = Pure x
    hoist f (Free mpx) = Free $ f $ mpx <&> hoist f

data StepResult t e f a
    = Complete a
    | Continue (ParserT t e f a)
    | Raised [e]

tryStep :: Monad m => ParserT t e m a -> t -> m (StepResult t e m a)
tryStep Fail _ = return $ Raised []
tryStep (Then next) tok = return $ case next tok of
    Left err -> Raised [err]
    Right px -> Continue px
tryStep (Alt px py) tok = do
    rx <- tryStep px tok
    case rx of
        Complete x   -> return $ Complete x
        Continue px' -> return $ Continue px'
        Raised errs  -> do
            ry <- tryStep py tok
            case ry of
                Complete y   -> return $ Complete y
                Continue py' -> return $ Continue py'
                Raised errs' -> return $ Raised (errs ++ errs')
tryStep (Pure x) _ = return $ Complete x
tryStep (Free mpx) tok = mpx >>= (`tryStep` tok)

tryEnd :: Monad m => ParserT t e m a -> MaybeT m a
tryEnd Fail = hoistMaybe Nothing
tryEnd (Then _) = hoistMaybe Nothing
tryEnd (Alt px py) = tryEnd px <|> tryEnd py
tryEnd (Pure x) = hoistMaybe $ Just x
tryEnd (Free mpx) = lift mpx >>= tryEnd

data ParseError t e
    = ExpectedFound [e] t
    | UnexpectedEOS

foldParserT :: Monad m => ParserT t e m a -> StateT s (MaybeT m) t -> StateT s m (Either (ParseError t e) a)
foldParserT px next = StateT $ \s -> do
    maybe_ts <- runMaybeT $ runStateT next s
    case maybe_ts of
        Nothing        -> runMaybeT (tryEnd px) <&> (maybe (Left UnexpectedEOS) Right >>> (,s))
        Just (tok, s') -> do
            rx <- tryStep px tok
            case rx of
                Complete x   -> return (Right x, s')
                Continue px' -> runStateT (foldParserT px' next) s'
                Raised es    -> return (Left $ ExpectedFound es tok, s)

runParserT :: Monad m => ParserT t e m a -> [t] -> ExceptT (ParseError t e) m a
runParserT px [] = maybeToExceptT UnexpectedEOS $ tryEnd px
runParserT px (tok : toks) = do
    rx <- lift $ tryStep px tok
    case rx of
        Complete x   -> return x
        Continue px' -> runParserT px' toks
        Raised es    -> throwE $ ExpectedFound es tok

data StrictParseError t e a
    = Failure (ParseError t e)
    | PrematureEnd a

pattern FailureExpectedFound :: [e] -> t -> StrictParseError t e a
pattern FailureExpectedFound es tok = Failure (ExpectedFound es tok)

pattern FailureUnexpectedEOS :: StrictParseError t e a
pattern FailureUnexpectedEOS = Failure UnexpectedEOS

{-# COMPLETE FailureExpectedFound, FailureUnexpectedEOS, PrematureEnd #-}

foldParserTStrict :: Monad m => ParserT t e m a -> StateT s (MaybeT m) t -> StateT s m (Either (StrictParseError t e a) a)
foldParserTStrict px next = StateT $ \s -> do
    maybe_ts <- runMaybeT $ runStateT next s
    case maybe_ts of
        Nothing        -> runMaybeT (tryEnd px) <&> (maybe (Left FailureUnexpectedEOS) Right >>> (,s))
        Just (tok, s') -> do
            rx <- tryStep px tok
            case rx of
                Complete x   -> return (Left $ PrematureEnd x, s')
                Continue px' -> runStateT (foldParserTStrict px' next) s'
                Raised es    -> return (Left $ FailureExpectedFound es tok, s)

runParserTStrict :: Monad m => ParserT t e m a -> [t] -> ExceptT (StrictParseError t e a) m a
runParserTStrict px [] = maybeToExceptT FailureUnexpectedEOS $ tryEnd px
runParserTStrict px (tok : toks) = do
    rx <- lift $ tryStep px tok
    case rx of
        Complete x -> throwE $ PrematureEnd x
        Continue px' -> runParserTStrict px' toks
        Raised es -> throwE $ FailureExpectedFound es tok

withToken :: Functor m => (t' -> t) -> ParserT t e m a -> ParserT t' e m a
withToken _ Fail = Fail
withToken f (Then continue) = Then $ f >>> continue >>> fmap (withToken f)
withToken f (Alt px py) = (Alt `on` withToken f) px py
withToken _ (Pure x) = Pure x
withToken f (Free mpx) = Free $ mpx <&> withToken f

withError :: Functor m => (e -> e') -> ParserT t e m a -> ParserT t e' m a
withError _ Fail            = Fail
withError f (Then continue) = Then $ continue >>> bimap f (withError f)
withError f (Alt px py)     = (Alt `on` withError f) px py
withError _ (Pure x)        = Pure x
withError f (Free mpx)      = Free $ mpx <&> withError f

trackTokens :: Functor m => ParserT t e m a -> ParserT t e m ([t], a)
trackTokens Fail            = Fail
trackTokens (Then continue) = Then $ \tok -> continue tok <&> (trackTokens >>> fmap (first (tok :)))
trackTokens (Alt px py)     = (Alt `on` trackTokens) px py
trackTokens (Pure x)        = Pure ([], x)
trackTokens (Free mpx)      = Free $ mpx <&> trackTokens

expect :: (t -> Either e a) -> ParserT t e m a
expect f = Then $ f >>> fmap Pure

type Parser t e a = ParserT t e Identity a

runParser :: Parser t e a -> [t] -> Either (ParseError t e) a
runParser px ts = runExceptT >>> runIdentity $ runParserT px ts

runParserStrict :: Parser t e a -> [t] -> Either (StrictParseError t e a) a
runParserStrict px ts = runExceptT >>> runIdentity $ runParserTStrict px ts