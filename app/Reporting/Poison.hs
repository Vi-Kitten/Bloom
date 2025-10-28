module Reporting.Poison (
    PoisonID,
    CompilationError,
    PoisonService,
    reportErr,
    reportInitErr,
    runService
) where
import GHC.TypeLits (Nat)
import Control.Monad.Writer.Lazy (Writer, tell)
import Control.Monad.Trans.State.Lazy (StateT, get, put, runStateT)
import Control.Monad.Morph (MonadTrans(..))
import Control.Arrow ((>>>))

type PoisonID = Nat

type CompilationError = String

type PoisonService = StateT PoisonID (Writer [(CompilationError, PoisonID, [PoisonID])])

reportErr :: CompilationError -> [PoisonID] -> PoisonService PoisonID
reportErr err causes = do
    n <- get
    put (n + 1)
    lift $ tell [(err, n, causes)]
    return n

reportInitErr :: CompilationError -> PoisonService PoisonID
reportInitErr err = reportErr err []

runService :: PoisonService a -> Writer [(CompilationError, PoisonID, [PoisonID])] a
runService = flip runStateT 0 >>> fmap fst