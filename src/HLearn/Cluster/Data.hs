{-# LANGUAGE GADTs #-}

module HLearn.Cluster.Data where

import qualified Data.Array.Repa as R
import Data.List (foldl')
import qualified Data.Vector as V
import HLearn.Cluster.Error
import HLearn.Internal.Error
import qualified HLearn.Internal.Data as I

data Cluster a = Cluster
  { clusterId :: {-# UNPACK #-} !Int,
    clusterCentroid :: {-# UNPACK #-} !(I.Point a)
  }

data PointSum a where
  PointSum ::
    Fractional a =>
    {-# UNPACK #-} !Int ->
    {-# UNPACK #-} !(I.Point a) ->
    PointSum a

addPointSumUnsafe :: (Fractional a) => PointSum a -> I.Point a -> PointSum a
addPointSumUnsafe (PointSum count (I.Point pss)) (I.Point ps) =
  let zs = V.zipWith (+) pss ps in PointSum (count + 1) (I.Point $ zs)

addPointSum :: (Fractional a) => PointSum a -> I.Point a -> Either InternalError (PointSum a)
addPointSum ps@(PointSum _ (I.Point xs)) p@(I.Point ys)
  | length xs /= length ys = Left $ DataError "cannot add to point sum with different shape "
  | otherwise = Right $ addPointSumUnsafe ps p

makeCluster ::
  (R.Shape sh, Ord a, Fractional a) =>
  sh ->
  Int ->
  I.NonEmptyPointArray a ->
  Cluster a
makeCluster sh cid (I.NonEmptyPointArray points) =
  Cluster
    { clusterId = cid,
      clusterCentroid =
        I.Point {I.unPoint = (\a -> a / fromIntegral count) <$> vs}
    }
  where
    I.Point vs = foldl' add' (I.zeroPoint sh) points
    count = length points
    add' (I.Point va) (I.Point vb) =
      I.Point {I.unPoint = V.zipWith (+) va vb}

pointSumToCluser :: (Fractional a) => Int -> PointSum a -> Cluster a
pointSumToCluser id (PointSum count xs) =
  Cluster
    { clusterId = id,
      clusterCentroid = I.Point $ (\a -> a / fromIntegral count) <$> I.unPoint xs
    }
