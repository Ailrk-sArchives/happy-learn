module HLearn.Internal.Metrics where

import HLearn.Internal.Types
import Data.Array.Repa as R

-- The distance matrix between point x and y.
-- This method works for arbitrary dimension.
euclideanDistance :: Point dim a -> Point dim a -> Distance
euclideanDistance = undefined

nanEuclideanDistance :: Point dim a -> Point dim a -> Distance
nanEuclideanDistance p1 p2 = undefined
