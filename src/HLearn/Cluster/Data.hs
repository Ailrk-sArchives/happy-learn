module HLearn.Cluster.Data where

import qualified Data.Array.Repa as R
import Data.List (foldl')
import qualified Data.Vector as V
import qualified HLearn.Internal.Data as I
import HLearn.Cluster.Error

data Cluster sh a = Cluster
  { clusterId :: {-# UNPACK #-} !Int,
    clusterCentroid :: {-# UNPACK #-} !(I.Point sh a)
  }

makeCluster ::
  (R.Shape sh, Ord a, Fractional a) =>
  sh ->
  Int ->
  I.PointArray sh a ->
  Cluster sh a
makeCluster sh cid (I.PointArray points) =
  Cluster
    { clusterId = cid,
      clusterCentroid =
        I.Point
          { I.unDim = sh,
            I.unPoint = (\a -> a / fromIntegral count) <$> vs
          }
    }
  where
    I.Point sh' vs = foldl' add' (I.zeroPoint sh) points
    count = length points
    add' (I.Point _ va) (I.Point _ vb) =
      I.Point
        { I.unDim = sh,
          I.unPoint = V.zipWith (+) va vb
        }
