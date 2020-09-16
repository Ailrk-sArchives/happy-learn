module HLearn.Cluster.Data where

import qualified HLearn.Internal.Data as I
import qualified Data.Array.Repa as R

data Cluster sh a = Cluster
  { clusterId :: {-# UNPACK #-} !Int,
    clusterCentroid :: {-# UNPACK #-} !(I.Point sh a)
  }

makeCluster ::
  (R.Shape sh) => sh -> Int -> R.Array R.U sh (I.Point sh a) -> Cluster sh a
makeCluster cid points =
  Cluster
    { clusterId = cid,
      clusterCentroid = undefined
    }
  where
    count = I.mapPoint

