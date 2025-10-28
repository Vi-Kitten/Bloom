module Typing (
    BloomKind (..),
    FundimentalType (..),
    Variance (..)
) where

import Data.Functor.Contravariant (Contravariant)

data Variance
    = Mixed
    | Covariant
    | Contravariant
    deriving Eq

data BloomKind
    = Type
    | HigherKinded BloomKind [(Variance, BloomKind)]

data FundimentalType
    = Unit
    | Void -- for[a] a
    | Full -- dyn[a] a
    | Fn -- (->)
    | FnMut -- (~>)
    | FnOnce -- (-+)
    | FnMust -- (-*)