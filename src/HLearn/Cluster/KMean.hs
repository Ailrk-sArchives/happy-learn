{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module HLearn.Cluster.KMean
  ( kmeansLloyd,
    kmeansElkan,
    KmeanConfig (..),
  )
where

import Control.Monad.Identity
import qualified Data.Array.Repa as R
import qualified Data.Vector as VV
import qualified Data.Vector.Unboxed as V
import HLearn.Internal.Data
import HLearn.Cluster.Data
import qualified HLearn.Internal.Data as I

-- Kmean choose centroids that minize the sun of squares within cluster,
-- AKA intertia.
-- goal is to find μⱼ s.t
--     n
--     ∑ min(|| xᵢ - μⱼ ||²), μⱼ ∈ ℂ
--    i=0

type Clusters a = VV.Vector (Cluster a)

type PointSums a = VV.Vector (PointSum a)

data KmeanConfig a = KmeanConfig
  { nclusters :: Int,
    clusters :: Clusters a,
    points :: I.NonEmptyPointList a
  }

-- | kmean with Lloyd algorithm.
kmeansLloyd :: (V.Unbox a, Fractional a, Eq a) => KmeanConfig a -> IO (Clusters a)
kmeansLloyd c@(KmeanConfig nclusters clusters points) = loop 0 clusters
  where
    loop n clusters = do
      let clusters' = step c
      if clusters' == clusters
        then return clusters
        else loop (n + 1) clusters'

-- | Kmean with Elkan method.
kmeansElkan :: KmeanConfig a -> IO (Clusters a)
kmeansElkan (KmeanConfig nclusters clusters points) = undefined

-- ---------------------------------------------------------------------
-- Internal definitions

-- | One step of kmean
step :: (V.Unbox a, Fractional a) => KmeanConfig a -> Clusters a
step config = nextClusters (assign config)

-- | Assign closest data points to the centroid
--     each cluster corresponds to a point sum.
assign :: (V.Unbox a, Fractional a) => KmeanConfig a -> PointSums a
assign (KmeanConfig nclusters clusters points) = undefined
  where
    pointArray = compactPoints points
    distance = undefined

nextClusters :: (V.Unbox a) => PointSums a -> Clusters a
nextClusters ps =
  VV.fromList
    [ pointSumToCluser i ps
      | (i, ps@(PointSum count _)) <- [0 ..] `zip` (VV.toList ps),
        count > 0
    ]
