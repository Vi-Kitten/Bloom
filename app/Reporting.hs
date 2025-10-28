module Reporting (
    InternalCompilerError (..),
    CompilationError,
    PoisonID,
    CompilerExcept,
    raise,
    raiseInit,
    internalFailure
) where

import Reporting.Poison (CompilationError, PoisonService, PoisonID, reportErr, reportInitErr)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Morph (MonadTrans(..))
import Control.Monad.Except (MonadError(..))

data InternalCompilerError
    = InferenceKeyError
    | KindTrackingError

type CompilerExcept = ExceptT InternalCompilerError PoisonService

raise :: CompilationError -> [PoisonID] -> CompilerExcept PoisonID
raise e ps = lift $ reportErr e ps

raiseInit :: CompilationError -> CompilerExcept PoisonID
raiseInit e = lift $ reportInitErr e

internalFailure :: InternalCompilerError -> CompilerExcept a
internalFailure = throwError