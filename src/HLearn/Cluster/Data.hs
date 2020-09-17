{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module HLearn.Cluster.Data where

import qualified Data.Array.Repa as R
import Data.List (foldl')
import qualified Data.Vector as V
import HLearn.Cluster.Error
import HLearn.Internal.Error
import qualified HLearn.Internal.Data as I

-- | Cluster of a group of point.
data Cluster a = Cluster
  { clusterId :: {-# UNPACK #-} !Int,
    clusterCentroid :: {-# UNPACK #-} !(I.Point a)
  }
  deriving (Show, Eq)

-- | The accumulated point position.
--     PointSum stores the result of the sum of points without building up
--     any extra data structure.
data PointSum a where
  PointSum ::
    Fractional a =>
    {-# UNPACK #-} !Int ->
    {-# UNPACK #-} !(I.Point a) ->
    PointSum a

-- | Add a point to PointSum without length check
addPointSumUnsafe :: (Fractional a) => PointSum a -> I.Point a -> PointSum a
addPointSumUnsafe (PointSum count (I.Point pss)) (I.Point ps) =
  let zs = V.zipWith (+) pss ps in PointSum (count + 1) (I.Point $ zs)
{-# INLINE addPointSumUnsafe #-}

-- | Add a point to PointSum with length check
addPointSum ::
  (Fractional a) => PointSum a -> I.Point a -> Either InternalError (PointSum a)
addPointSum ps@(PointSum _ (I.Point xs)) p@(I.Point ys)
  | differentShape = Left $ DataError "cannot add to point sum with different shape"
  | otherwise = Right $ addPointSumUnsafe ps p
  where
    differentShape = length xs /= length ys
{-# INLINE addPointSum #-}

-- | Make cluster from
makeCluster ::
  (Ord a, Fractional a) => Int -> Int -> I.NonEmptyPointList a -> Cluster a
makeCluster rank cid (I.NonEmptyPointList points) =
  Cluster
    { clusterId = cid,
      clusterCentroid =
        I.Point {I.unPoint = (\a -> a / fromIntegral count) <$> vs}
    }
  where
    I.Point vs = foldl' add' (I.zeroPoint rank) points
    count = length points
    add' (I.Point va) (I.Point vb) = I.Point {I.unPoint = V.zipWith (+) va vb}

-- | Convert PointSum to Cluster.
pointSumToCluser :: (Fractional a) => Int -> PointSum a -> Cluster a
pointSumToCluser id (PointSum count xs) =
  Cluster
    { clusterId = id,
      clusterCentroid = I.Point $ (\a -> a / fromIntegral count) <$> I.unPoint xs
    }
{-# INLINE pointSumToCluser #-}
