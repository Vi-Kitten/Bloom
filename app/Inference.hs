{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE RankNTypes #-}
module Inference (
    LatticeComparison (..),
    TypeTrait (..),
    SolvingContext (..),
    Constraint (..)
) where

-- THE SIMPLE APPROACH TO CONSTRAINT SOLVING TAKEN HERE IS NOT FINAL, A LOT WILL BE REWRITTEN, BUT THE CORE OF IT *SHOULD* SCALE!

import Typing (BloomKind (..), Variance)
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Reporting (CompilationError, InternalCompilerError (..), PoisonID, raise, raiseInit, CompilerExcept, internalFailure)
import Data.Function (on, (&))
import Data.Map (Map, lookup, delete, insert, adjust)
import Control.Monad.Trans.State.Lazy (StateT, get, put, modify)
import Control.Monad.Morph (MonadTrans(..))
import Data.Functor ((<&>), ($>))
import Data.Set (Set)
import Data.Bifunctor (Bifunctor(..))

data LatticeComparison
    = Equivilent
    | SubType
    | SuperType
    | Unrelated

data TypeInferenceOperand k t
    = KnownType t
    | Unknown k

data TypeInferenceTerm k t
    = Specification (TypeInferenceOperand k t) [TypeInferenceTerm k t]

replaceUnknown :: Eq k => k -> TypeInferenceTerm k t -> TypeInferenceTerm k t -> TypeInferenceTerm k t
replaceUnknown key sx                          (Specification (KnownType ty) params ) = Specification (KnownType ty) $ params <&> replaceUnknown key sx
replaceUnknown key sx@(Specification x params) (Specification (Unknown key') params') = if key == key'
    then Specification x              $ params <> (params' <&> replaceUnknown key sx)
    else Specification (Unknown key') $ params' <&> replaceUnknown key sx

-- for now these will be entirely nonconstructive
data TypeTrait
    = Pure  -- ~Self then Self
    | Drop  -- ~Self
    | Dup   -- Self -> (Self, Self)
    | Chain -- Self -> (Self then Self)

-- provides type-lattice information and existance lookup (for now lookup is not constructive)
data SolvingContext t = SolvingContext {
    compareTypes :: t -> t -> LatticeComparison,
    factorTrait :: forall k. TypeTrait -> (t, [TypeInferenceTerm k t]) -> Maybe [Constraint k t]
}

data SolverState k t = SolverState {
    activeConstraints :: [Constraint k t],
    -- rejectedConstraints :: [Constraint k t],
    blockingInferences :: Map k [Constraint k t],
    completedInferences :: Map k (TypeInferenceTerm k t)
    -- constructive shenanigans (traits) will also go here, eventually
}

-- `t` is assumed to be a type expression without rewrites
-- which is to say, upon prividing it terms, it will not normalise into something else.
data Constraint k t
    = TypeEquality (TypeInferenceTerm k t) (TypeInferenceTerm k t)
    -- | TypeSubtypeOf (TypeInferenceTerm k t) (TypeInferenceTerm k t)
    | ExistanceOf TypeTrait (TypeInferenceTerm k t)

data SolvedType t
    = Simple t
    | Specified (SolvedType t) (SolvedType t)

data ConstraintResult k t = ConstraintResult {
    types :: Map k (Either PoisonID (SolvedType t))
}

-- the whole point of this entire headache of a module
-- solveConstraints :: Ord k => SolvingContext t -> Set k -> [Constraint k t] -> CompilerExcept (ConstraintResult k t)
-- solveConstraints = _

type SolveEffect k t = ReaderT (SolvingContext t) (StateT (SolverState k t) CompilerExcept)

nextConstraint :: SolveEffect k t (Maybe (Constraint k t))
nextConstraint = lift $ get >>= \state -> case state & activeConstraints of
    [] -> return Nothing
    c : cs -> do
        put $ state { activeConstraints = cs }
        return $ Just c

scheduleConstraints :: [Constraint k t] -> SolveEffect k t ()
scheduleConstraints cs = lift $ modify $ \state -> state {
        activeConstraints = (state & activeConstraints) ++ cs
    }

replaceUnknownInConstraint :: Eq k => k -> TypeInferenceTerm k t -> Constraint k t -> Constraint k t
replaceUnknownInConstraint k v (TypeEquality  x y) = (TypeEquality  `on` replaceUnknown k v) x y
-- replaceUnknownInConstraint k v (TypeSubtypeOf x y) = (TypeSubtypeOf `on` replaceUnknown k v) x y
replaceUnknownInConstraint k v (ExistanceOf trt x) = ExistanceOf trt $ replaceUnknown k v x

-- check for cyclic definitions before this so we don't get keyerrors and bad logic
inferAs :: Ord k => k -> TypeInferenceTerm k t -> SolveEffect k t ()
inferAs k v = lift $ do
    state <- get
    case state & blockingInferences & Data.Map.lookup k of
            Nothing -> lift $ internalFailure InferenceKeyError
            Just releasedInferences -> put state {
                    activeConstraints = ((state & activeConstraints) ++ releasedInferences) <&> replaceUnknownInConstraint k v,
                    blockingInferences = state & blockingInferences & Data.Map.delete k <&> fmap (replaceUnknownInConstraint k v),
                    completedInferences = (state & completedInferences <&> replaceUnknown k v) & Data.Map.insert k v
                }

deferUntil :: Ord k => k -> Constraint k t -> SolveEffect k t ()
deferUntil k c = lift $ modify $ \state -> state {
        blockingInferences = state & blockingInferences & Data.Map.adjust (c:) k
    }

constrainEqualParameters :: [TypeInferenceTerm k t] -> [TypeInferenceTerm k t] -> SolveEffect k t ()
constrainEqualParameters [] [] = pure ()
constrainEqualParameters (c:cs) (c':cs') = constrainEqualParameters cs cs' *> scheduleConstraints [TypeEquality c c']
constrainEqualParameters _ _ = lift $ lift $ internalFailure KindTrackingError

-- returns poison IDs from unsatisfiable constraints
reduceConstraint :: Ord k => Constraint k t -> SolveEffect k t [PoisonID]
reduceConstraint c@(ExistanceOf _ (Specification (Unknown key) _)) = deferUntil key c $> []
reduceConstraint (ExistanceOf trt (Specification (KnownType ty) params)) = ask >>= \ctx -> case (ctx & factorTrait) trt (ty, params) of
    Just cs -> scheduleConstraints cs $> []
    Nothing -> lift $ lift $ raiseInit "[[dummy unsatisfiable constraint message]]" <&> (:[])
reduceConstraint (TypeEquality (Specification (Unknown key) []) x) = inferAs key x $> []
reduceConstraint (TypeEquality x (Specification (Unknown key) [])) = inferAs key x $> []
reduceConstraint c@(TypeEquality (Specification (Unknown key) (_:_)) (Specification (KnownType _) _)) = deferUntil key c $> []
reduceConstraint c@(TypeEquality (Specification (KnownType _) _) (Specification (Unknown key) (_:_))) = deferUntil key c $> []
reduceConstraint c@(TypeEquality (Specification (Unknown key') params) (Specification (Unknown key) params')) = if key == key'
    then constrainEqualParameters params params' $> []
    else deferUntil key c $> []
reduceConstraint (TypeEquality (Specification (KnownType ty) params) (Specification (KnownType ty') params')) = ask >>= \ctx -> case (ctx & compareTypes) ty ty' of 
    Equivilent -> constrainEqualParameters params params' $> []
    _ -> lift $ lift $ raiseInit "[[dummy unsatisfiable constraint message]]" <&> (:[])
-- reduceConstraint (TypeSubtypeOf )
-- reduceConstraint _ = _