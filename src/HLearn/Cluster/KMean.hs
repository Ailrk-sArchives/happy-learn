{-# LANGUAGE PartialTypeSignatures #-}

module HLearn.Cluster.KMean
  ( kmeansLloyd,
    kmeansElkan,
  )
where

import qualified Data.Array.Repa as R
import HLearn.Cluster.Data
import qualified HLearn.Internal.Data as I

-- Kmean choose centroids that minize the sun of squares within cluster,
-- AKA intertia.
--     n
--     ∑ min(|| xᵢ - μⱼ ||²), μⱼ ∈ ℂ
--    i=0

type ClusterArray sh a = R.Array R.U sh (Cluster a)

type PointSumArray sh a = R.Array R.U sh (PointSum a)

-- | kmean with Lloyd algorithm.
kmeansLloyd ::
  R.Shape sh =>
  Int ->
  I.NonEmptyPointList a ->
  ClusterArray sh a ->
  IO (ClusterArray sh a)
kmeansLloyd nclusters points clusters = undefined

-- | Kmean with Elkan method.
kmeansElkan ::
  R.Shape sh =>
  Int ->
  I.NonEmptyPointList a ->
  ClusterArray sh a ->
  IO (ClusterArray sh a)
kmeansElkan = undefined

-- | Assign
assign ::
  (R.Shape sh) =>
  Int ->
  ClusterArray sh a ->
  I.NonEmptyPointList a ->
  PointSumArray sh a
assign nclusters clusters points = undefined
