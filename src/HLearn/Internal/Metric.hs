module HLearn.Internal.Metric where


import           HLearn.Internal.Data
import           Data.Vector.Generic           as G

distanceMetric :: (Double -> Double -> Double) -> Point -> Point -> Double
distanceMetric f (Point _ v1) (Point _ v2) = G.sum $ G.zipWith f v1 v2

sqDistance :: Point -> Point -> Double
sqDistance = distanceMetric (\x1 x2 -> (x1 - x2) ^ 2)

euclideanDistance :: Point -> Point -> Double
euclideanDistance = distanceMetric (\x1 x2 -> sqrt $ (x1 ^ 2 - x2 ^ 2))
