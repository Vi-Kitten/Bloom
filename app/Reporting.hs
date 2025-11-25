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

import Reporting.Poison (CompilationError, PoisonService, PoisonEvent, PoisonID, reportErr, reportInitErr, runService)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Morph (MonadTrans(..))
import Control.Monad.Except (MonadError(..))
import Control.Monad.Writer.Lazy (runWriter)
import Control.Arrow ((>>>))

data InternalCompilerError
    = InferenceKeyError
    | KindTrackingError
    deriving Show

type CompilerExcept = ExceptT InternalCompilerError PoisonService

raise :: CompilationError -> [PoisonID] -> CompilerExcept PoisonID
raise e ps = lift $ reportErr e ps

raiseInit :: CompilationError -> CompilerExcept PoisonID
raiseInit e = lift $ reportInitErr e

internalFailure :: InternalCompilerError -> CompilerExcept a
internalFailure = throwError

runReporter :: CompilerExcept a -> (Either InternalCompilerError a, [PoisonEvent])
runReporter = runExceptT >>> runService >>> runWriter