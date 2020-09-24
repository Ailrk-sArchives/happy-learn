module HLearn.Cluster.Hierachical where

import           HLearn.Internal.Data

-- bottom up approach
data AgglomerativeConfig = AgglomerativeConfig
  { agglNum :: Int
  , agglDistThreashold :: Double
  , agglAffinity :: Point -> Point -> Double
  }


