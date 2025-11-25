module Reporting.Poison (
    PoisonID,
    CompilationError,
    PoisonService,
    PoisonEvent,
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

data PoisonEvent = PoisonEvent {
    causes :: [PoisonID],
    poison :: PoisonID,
    reason :: CompilationError
} deriving Show

type PoisonService = StateT PoisonID (Writer [PoisonEvent])

reportErr :: CompilationError -> [PoisonID] -> PoisonService PoisonID
reportErr err causes' = do
    n <- get
    put (n + 1)
    lift $ tell [PoisonEvent causes' n err]
    return n

reportInitErr :: CompilationError -> PoisonService PoisonID
reportInitErr err = reportErr err []

runService :: PoisonService a -> Writer [PoisonEvent] a
runService = flip runStateT 0 >>> fmap fst