{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module HLearn.Cluster.KMean
  ( kmeansLloyd,
    KmeanConfig (..),
  )
where

import Control.Monad.Identity
import qualified Data.Array.Repa as R
import Data.Function (on)
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import HLearn.Cluster.Data
import HLearn.Internal.Data
import HLearn.Internal.Metrics as M
import qualified HLearn.Internal.Data as I

-- Kmean choose centroids that minize the sun of squares within cluster,
-- AKA intertia.
-- goal is to find μⱼ s.t
--     n
--     ∑ min(|| xᵢ - μⱼ ||²), μⱼ ∈ C
--    i=0

type Clusters a = V.Vector (Cluster a)

type PointSums a = V.Vector (PointSum a)

data KmeanConfig a = KmeanConfig
  { nclusters :: Int,
    clusters :: Clusters a,
    points :: I.NonEmptyPointList a
  }

-- | kmean with Lloyd algorithm.
kmeansLloyd :: KmeanConfig Double -> IO (Clusters Double)
kmeansLloyd c@(KmeanConfig nclusters clusters points) = loop 0 clusters
  where
    loop n clusters = do
      let clusters' = step c
      if clusters' == clusters
        then return clusters
        else loop (n + 1) clusters'

-- ---------------------------------------------------------------------
-- Internal definitions

-- | One step of kmean
step :: KmeanConfig Double -> Clusters Double
step config = nextClusters (assign config)

-- | Assign closest data points to the centroid
--     each cluster corresponds to a point sum.
assign :: KmeanConfig Double -> PointSums Double
assign (KmeanConfig nclusters clusters points) = V.create $ do
  let dim = clusterDim $ V.head clusters
  vec <- MV.replicate nclusters (PointSum 0 (I.zeroPoint dim))
  let
    add p = do
      let c = nearest p
          cid = clusterId c
      ps <- MV.read vec cid
      MV.write vec cid $! addPointSum ps p

  return vec
  where
    pointArray = compactPoints points
    nearest p =
      L.minimumBy
        (compare `on` ((M.euclideanDistance p) . clusterCentroid))
        (V.toList clusters)


-- fold triple of point into cluster id
-- nearest ::
--   (UV.Unbox a, Floating a) => R.Array R.U R.DIM2 a -> R.Array R.U R.DIM1 a

-- TODO
-- I want to be able to access inner most dimension at once.
-- so I can calculate the euclideanDistance base on that.
--
-- nearest =
--   R.foldP
--     ( \a b ->
--         let dist = M.euclideanDistance (Point a) (Point b)
--          in undefined
--     )
--     0

-- nearest p =
--   fst $
--     L.minimumBy
--       (compare `on` snd)
--       [(c, M.euclideanDistance p (clusterCentroid c)) | c <- V.toList clusters]

nextClusters :: PointSums Double -> Clusters Double
nextClusters ps =
  V.fromList
    [ pointSumToCluser i ps
      | (i, ps@(PointSum count _)) <- [0 ..] `zip` (V.toList ps),
        count > 0
    ]
