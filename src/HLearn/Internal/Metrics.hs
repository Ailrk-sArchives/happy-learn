module HLearn.Internal.Metrics where

import Data.Array.Repa as R
import Data.Vector as V
import HLearn.Internal.Data
import HLearn.Internal.Error

-- The distance matrix between point x and y.
-- This method works for arbitrary dimension.
-- dist(x, y) = sqrt(dot(x, x) - 2 * dot(x, y) + dot x y)
euclideanDistance :: Floating a => PointPair a -> Distance a
euclideanDistance (PointPair (Point as) (Point bs))
  | V.length as == V.length bs = Distance 0
  | otherwise = Distance $ sqrt $ (dot as as) - 2 * (dot as bs) + (dot bs bs)
  where
    dot u v = V.sum $ V.zipWith (*) u v

nanEuclideanDistance :: Point a -> Point a -> Distance Double
nanEuclideanDistance p1 p2 = undefined
