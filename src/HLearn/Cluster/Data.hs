{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module HLearn.Cluster.Data where

import           HLearn.Internal.Data
import           HLearn.Internal.Metric
import           Data.Vector.Generic           as G
import           Control.DeepSeq
import qualified Data.Vector                   as Vec
import qualified Data.Vector.Mutable           as MVec
import           Control.Monad.State

data Cluster = Cluster { clusterId :: !Int
                       , clusterCent :: !Point} deriving (Eq, Show)

data PointSum = PointSum { psumCount :: !Int
                         , psumPoint :: !Point
                         } deriving (Eq, Show)

instance NFData PointSum where
  rnf (PointSum count (Point dim xs)) = ()

addToPointSum :: PointSum -> Point -> PointSum
addToPointSum (PointSum count (Point _ ps)) p@(Point _ xs) =
  PointSum (count + 1) (p { unPoint = G.zipWith (+) xs ps })

pointSumToCluster :: Int -> PointSum -> Cluster
pointSumToCluster cid (PointSum count p) = Cluster
  { clusterId   = cid
  , clusterCent = p { unPoint = G.map (/ (fromIntegral count)) $ unPoint p }
  }

-- | indicate if two clusters are close enough that we can
-- treet them as the same cluster
sameCluster :: Cluster -> Cluster -> Bool
sameCluster (Cluster a _) (Cluster b _) | a /= b = False
sameCluster (Cluster _ as) (Cluster _ bs)        = (sqDistance as bs) < 0.001

-- | filter away clustser has no point close to it.
newCluster :: (MonadState s m) => Vec.Vector PointSum -> m [Cluster]
newCluster vec =
  return
    $ [ pointSumToCluster i ps
      | (i, ps@(PointSum count _)) <- [0 ..] `Prelude.zip` (Vec.toList vec)
      , count > 0
      ]

