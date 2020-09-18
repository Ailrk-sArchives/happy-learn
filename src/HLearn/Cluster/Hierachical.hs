module HLearn.Cluster.Hierachical where

import HLearn.Internal.Data

data AgglomerativeConfig = AgglomerativeConfig
  { agglomNclusters :: Int,
    agglomDistanceThreashold :: Double,
    agglomAffinity :: Point Double -> Point Double -> Double
  }
