{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module HLearn.Cluster.KMean
  ( kmeansLloyd,
    kmeansElkan,
    KmeanConfig (..),
  )
where

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Unboxed as RR
import HLearn.Cluster.Data
import qualified HLearn.Internal.Data as I

-- Kmean choose centroids that minize the sun of squares within cluster,
-- AKA intertia.
--     n
--     ∑ min(|| xᵢ - μⱼ ||²), μⱼ ∈ ℂ
--    i=0

type ClusterArray sh a = R.Array R.U sh (Cluster a)

type PointSumArray sh a = R.Array R.U sh (PointSum a)

data KmeanConfig sh a = KmeanConfig
  { nclusters :: Int,
    points :: I.NonEmptyPointList a,
    clusteres :: ClusterArray sh a
  }

-- | kmean with Lloyd algorithm.
kmeansLloyd :: R.Shape sh => KmeanConfig sh a -> IO (ClusterArray sh a)
kmeansLloyd (KmeanConfig nclusters points clusters) = undefined

-- | Kmean with Elkan method.
kmeansElkan :: R.Shape sh => KmeanConfig sh a -> IO (ClusterArray sh a)
kmeansElkan (KmeanConfig nclusters points clusters) = undefined

-- ---------------------------------------------------------------------
-- Internal definitions

-- | One step of kmean
step :: (R.Shape sh) => KmeanConfig sh a -> ClusterArray sh a
step (KmeanConfig nclusters clusters points) = undefined

-- | Assign
assign :: (R.Shape sh) => KmeanConfig sh a -> PointSumArray sh a
assign (KmeanConfig nclusters clusters points) = undefined

nextClusters :: (RR.Unbox a, R.Shape sh) => PointSumArray sh a -> ClusterArray sh a
nextClusters ps = undefined
