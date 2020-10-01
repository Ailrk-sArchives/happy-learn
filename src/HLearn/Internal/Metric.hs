module HLearn.Internal.Metric where


import           HLearn.Internal.Data
import           Data.Vector.Generic           as G

-- | to Generate distance metric functiosn
distanceMetric :: (Double -> Double -> Double) -> Point -> Point -> Double
distanceMetric f (Point _ v1) (Point _ v2) = G.sum $ G.zipWith f v1 v2

sqDistance :: Point -> Point -> Double
sqDistance = distanceMetric (\a b -> (a - b) ^ 2)

euclideanDistance :: Point -> Point -> Double
euclideanDistance = distanceMetric (\a b -> sqrt $ (a ^ 2 - b ^ 2))

manhattanDistance :: Point -> Point -> Double
manhattanDistance = distanceMetric (\a b -> abs $ a - b)

maximumDistance :: Point -> Point -> Double
maximumDistance (Point _ v1) (Point _ v2) =
  G.maximum $ G.zipWith (\a b -> abs $ a - b) v1 v2
