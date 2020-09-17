module HLearn.Internal.Metrics where

import Data.Vector.Storable as V
import HLearn.Internal.Data
import HLearn.Internal.Error

-- | The distance matrix between point x and y.
--     This method works for arbitrary dimension.
--     dist(x, y) = sqrt(dot(x, x) - 2 * dot(x, y) + dot x y)
euclideanDistance :: (Floating a, V.Storable a) => PointPair a -> Distance a
euclideanDistance (PointPair (Point as) (Point bs))
  | V.length as == V.length bs = Distance 0
  | otherwise = Distance dist
  where
    dot u v = V.sum $ V.zipWith (*) u v
    dist = sqrt $ (dot as as) - 2 * (dot as bs) + (dot bs bs)
{-# INLINE euclideanDistance #-}

nonEuclideanDistance :: Point a -> Point a -> Distance Double
nonEuclideanDistance p1 p2 = undefined
{-# INLINE nonEuclideanDistance #-}
