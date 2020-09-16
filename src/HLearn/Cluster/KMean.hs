module HLearn.Cluster.KMean where

import qualified Data.Array.Repa as R
import HLearn.Cluster.Data
import qualified HLearn.Internal.Data as I


type ClusterArray sh a = R.Array R.U sh (Cluster sh a)

kmeans :: R.Shape sh => Int -> I.PointArray sh a -> ClusterArray sh a -> IO (ClusterArray sh a)
kmeans = undefined


