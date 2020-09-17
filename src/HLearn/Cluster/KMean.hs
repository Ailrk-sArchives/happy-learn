module HLearn.Cluster.KMean where

import qualified Data.Array.Repa as R
import HLearn.Cluster.Data
import qualified HLearn.Internal.Data as I


type ClusterArray sh a = R.Array R.U sh (Cluster a)
type PointSumArray sh a = R.Array R.U sh (PointSum a)

kmeans :: R.Shape sh => Int -> I.NonEmptyPointArray a -> ClusterArray sh a -> IO (ClusterArray sh a)
kmeans = undefined

assign :: (R.Shape sh) => Int -> ClusterArray sh a -> I.NonEmptyPointArray a -> PointSumArray sh a
assign = undefined
