module HLearn.Internal.Types where

{- Define some generic types that can be used for all algorithms. -}

import qualified Data.Array.Repa as R

-- point is always evaluated.
newtype Point dim a = Point
  { unPoint :: R.Array R.U dim a
  }
  deriving (Show, Eq)

data PointPair dim a = PointPair (Point dim a) (Point dim a)

newtype Distance = Distance {unDistance :: Double}

