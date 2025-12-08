module Reporting (
    InternalCompilerError (..),
    CompilationError,
    PoisonID,
    CompilerExcept,
    PoisonEvent,
    raise,
    raiseInit,
    internalFailure,
    runReporter
) where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Morph (MonadTrans(..))
import Control.Monad.Except (MonadError(..))
import Control.Monad.Writer.Lazy (runWriter)
import Control.Arrow ((>>>))
import Frontend.Spanned (Span)
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Writer (Writer, tell)
import GHC.TypeLits (Nat)
import Data.Function ((&))
import Data.List.NonEmpty (nonEmpty, NonEmpty (..))

type PoisonID = Nat

type CompilationError = String

data PoisonEvent = PoisonEvent {
    causes :: [PoisonID],
    poison :: PoisonID,
    reason :: CompilationError
}

instance Show PoisonEvent where
    show pe = case pe & causes & nonEmpty of
        Nothing -> "#" ++ show (pe & poison) ++ " " ++ (pe & reason)
        Just (c :| cs) -> ("#" ++ show c) ++ (cs >>= \c' -> ", #" ++ show c') ++ " => #" ++ show (pe & poison) ++ " " ++ (pe & reason)

type PoisonService = StateT PoisonID (Writer [PoisonEvent])

reportErr :: CompilationError -> [PoisonID] -> PoisonService PoisonID
reportErr err causes' = do
    n <- get
    put (n + 1)
    lift $ tell [PoisonEvent causes' n err]
    return n

reportInitErr :: CompilationError -> PoisonService PoisonID
reportInitErr err = reportErr err []

data InternalCompilerError
    = InferenceKeyError
    | KindTrackingError
    | LexerIdentifiedIncorrectSyntax Span Char
    | StatementKeyWordsAreNotSnakeKeyWords
    deriving Show

type CompilerExcept = ExceptT InternalCompilerError PoisonService

raise :: CompilationError -> [PoisonID] -> CompilerExcept PoisonID
raise e ps = lift $ reportErr e ps

raiseInit :: CompilationError -> CompilerExcept PoisonID
raiseInit e = lift $ reportInitErr e

internalFailure :: InternalCompilerError -> CompilerExcept a
internalFailure = throwError

runReporter :: CompilerExcept a -> (Either InternalCompilerError a, [PoisonEvent])
runReporter = runExceptT >>> flip runStateT 0 >>> fmap fst >>> runWriter