module HLearn.Internal.Metric where


import           HLearn.Internal.Data
import           Data.Vector.Generic           as G

sqDistance :: Point -> Point -> Double
sqDistance (Point _ v1) (Point _ v2) =
  G.sum $ G.zipWith (\x1 x2 -> (x1 - x2) ^ 2) v1 v2
