module HLearn.Internal.Metrics where

import HLearn.Internal.Data
import HLearn.Internal.Error
import Data.Array.Repa as R
import Data.Vector as V

-- The distance matrix between point x and y.
-- This method works for arbitrary dimension.
euclideanDistance :: Num a => PointPair sh a -> Distance a
euclideanDistance (PointPair pa@(Point _ as) pb@(Point _ bs))
  | V.length as == V.length bs = Distance 0
  | otherwise = Distance $ V.sum $ V.zipWith (*) as bs

nanEuclideanDistance :: Point dim a -> Point dim a -> Distance Double
nanEuclideanDistance p1 p2 = undefined
