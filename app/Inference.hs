{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Inference (

) where

import Data.List.NonEmpty (NonEmpty (..))
import Utils (Or (..))
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Function ((&), on)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad ((>=>))
import Control.Arrow ((>>>))

-- | Potentially temporary type alias for contradicitons.
type Contradiciton = String

-- | A reason given by a rewrite for opting into a non-decision.
type AbstinanceReason = String

-- implication should be monoid with <> meaning logical and
-- TODO: I need to stick an `Either Contradiction` somewhere and I don't know where

data Introduction m k
    = Introduction {
        -- | Knowledge gained by constructing the constraint.
        implication :: k,
        -- | The constraints to be introduced to the solving system.
        constriants :: [Constraint m k]
    }
    | Contradictory Contradiciton

instance Semigroup k => Semigroup (Introduction m k) where
    i <> i' = Introduction {
        implication = ((<>) `on` implication) i i',
        constriants = ((<>) `on` constriants) i i'
    }

instance Monoid k => Monoid (Introduction m k) where
    mempty = Introduction {
        implication = mempty,
        constriants = mempty
    }

data Constraint m k = Constraint {
    -- | Ways we can reduce the constraint, if solving is successful.
    reductions :: NonEmpty (Reduction m k)
}

data ReductionFailure
    -- | Reattempt when solver has more knowledge.
    = InsufficientInformation
    -- | Leverage partial decidability to avoid non-halting behaviour within the solver.
    | AbstainDecision (NonEmpty AbstinanceReason)

instance Semigroup ReductionFailure where
    InsufficientInformation <> InsufficientInformation = InsufficientInformation
    InsufficientInformation <> AbstainDecision _ = InsufficientInformation
    AbstainDecision _ <> InsufficientInformation = InsufficientInformation
    AbstainDecision ab <> AbstainDecision ab' = AbstainDecision (ab <> ab')

data Reduction m k
    -- | Under a condition evidenced by success,
    -- the list of constraints taken as intersection,
    -- should be **isomorphic** to the constraint the reduction is from.
    = Attempt (k -> ExceptT ReductionFailure m (Introduction m k))

-- TODO: Needs work, lots of work, bleh
data AbstainedConstraint m k = UndecidedConstraint {
    reasons :: NonEmpty AbstinanceReason,
    constraint :: Constraint m k
}

data SolverState m k = SolverState {
    -- | Knowledge we have derived.
    knowledge :: k,
    -- | What we want to prove.
    live_constraints :: [Constraint m k],
    -- potentially provide a reason for blocking that can be used for elaboration
    -- | What we will have to prove later.
    blocked_constraints :: [Constraint m k],
    -- | Constraints that the solver has abstained from further consideration.
    abstained_constraints :: [AbstainedConstraint m k]
}

-- TODO: eventually remove `m` from SolverDecision type and use elaboration type thing from reductions instead
-- potentially involving constructing a directed graph and seperating into strongly connected components

-- TODO: consider if `k` should be present regardless of decision

data SolverDecision m k
    -- | Solver has failed to prove or disprove satisfiability of constraints.
    = Undecided k (Or (NonEmpty (Constraint m k)) (NonEmpty (AbstainedConstraint m k)))
    -- | Satisfiability of constraints proven false by contradiction.
    | ProvenFalse k (NonEmpty Contradiciton)
    -- | All constraints are satisfied.
    -- The extent of constructivity may be found by processing the generated knowledge.
    | ProvenTrue k

initSolver :: (Monoid k) => SolverState m k
initSolver = SolverState {
    knowledge = mempty,
    live_constraints = [],
    blocked_constraints = [],
    abstained_constraints = []
}

introduceTo :: (Monad m, Semigroup k) => SolverState m k -> Introduction m k -> ExceptT (SolverDecision m k) m (SolverState m k)
introduceTo s Introduction { implication = k, constriants = constraints } = return $ s {
    knowledge = (s & knowledge) <> k,
    live_constraints = (s & live_constraints) <> (s & blocked_constraints) <> constraints,
    blocked_constraints = []
}
-- TODO: rewrite this to not be an instant fail
introduceTo s (Contradictory c) = throwError $ ProvenFalse (s & knowledge) (c :| [])

tryReduce :: Reduction m k -> k -> ExceptT ReductionFailure m (Introduction m k)
tryReduce (Attempt f) = f

tryReduceAny :: (Monad m) => NonEmpty (Reduction m k) -> k -> ExceptT ReductionFailure m (Introduction m k)
tryReduceAny (r :| []) k = tryReduce r k
tryReduceAny (r :| r' : rs) k =
    catchError (tryReduce r k) $ \e ->
    catchError (tryReduceAny (r' :| rs) k) $ \e' ->
    throwError (e <> e')

step :: (Monad m, Monoid k) => SolverState m k -> ExceptT (SolverDecision m k) m (SolverState m k)
step (SolverState k [] [] []) = ExceptT $ return $ Left (ProvenTrue k) -- written without `throwError` for semantic reasons (its not an error)
step (SolverState k [] (bc:bcs) []) = throwError $ Undecided k $ JustLeft (bc :| bcs)
step (SolverState k [] [] (ab:abs)) = throwError $ Undecided k $ JustRight (ab :| abs)
step (SolverState k [] (bc:bcs) (ab:abs)) = throwError $ Undecided k $ Both (bc :| bcs) (ab :| abs)
step s@SolverState { live_constraints = (c@(Constraint rs):cs) } = do
    res <- lift $ runExceptT $
        s & knowledge & tryReduceAny rs
    case res of
        -- constraint is blocked by insufficient information
        Left InsufficientInformation -> return $ s {
            live_constraints = cs, -- one fewer
            blocked_constraints = c : (s & blocked_constraints) -- one more
        }
        -- the solver has abstained from further consideration of the constraint
        Left (AbstainDecision rs) -> return $ s {
            live_constraints = cs, -- one fewer
            abstained_constraints = UndecidedConstraint rs c : (s & abstained_constraints) -- one more
        }
        Right intro -> intro & introduceTo s {
            live_constraints = cs -- remove reduced before introducing reduction
        }

normalise :: (Monad m, Monoid k) => SolverState m k -> m (SolverDecision m k)
normalise s = runExceptT (step s) >>= either pure normalise

solve :: (Monad m, Monoid k) => Introduction m k -> m (SolverDecision m k)
solve = introduceTo initSolver >>> runExceptT >=> either pure normalise