{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HLearn.Cluster.KMedois where


import           Data.List
import           Data.Function                  ( on )
import qualified Data.Vector                   as Vec
import qualified Data.Vector.Mutable           as MVec
import           HLearn.Cluster.Error
import           HLearn.Cluster.Data
import           HLearn.Internal.Data
import           HLearn.Internal.Metric
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import qualified Data.Vector                   as Vec
import qualified Data.Vector.Mutable           as MVec

limit = 100

data KMedoisState = KMedoisState { kmeddim :: Int
                                 , kmedsnum :: Int
                                 , kmedclusters :: [Cluster]
                                 , kmedpoints :: [Point]
                                 }


type KMedois' a = ExceptT ClusterError (State KMedoisState) a

newtype KMedois a = KMean { unKmean :: KMedois' a }
  deriving (Functor, Applicative, Monad, MonadState KMedoisState, MonadError ClusterError)


kmedois :: KMedois [Cluster]
kmedois = loop 0
 where
  loop :: Int -> KMedois [Cluster]
  loop n = do
    s@(KMedoisState _ ncluster clusters _) <- get
    when (ncluster > limit) $ do
      liftEither $ Left (ClusterInitError "too many clusters")
    clusters' <- step
    return clusters'
    if and $ zipWith sameCluster clusters' clusters
      then return clusters
      else put (s { kmedclusters = clusters' }) >> loop (n + 1)

step :: KMedois [Cluster]
step = assign >>= newCluster

assign :: KMedois (Vec.Vector PointSum)
assign = do
  KMedoisState dim ncluster clusters points <- get
  let nearestCluster p =
        fst $ minimumBy (compare `on` snd) [ mkpair c p | c <- clusters ]
  return
    $ (Vec.create $ do
        vec <- MVec.replicate ncluster (PointSum 0 $ zeroPoint dim)
        let addpoint p = do
              let c   = nearestCluster p
                  cid = clusterId c
              ps <- MVec.read vec cid
              MVec.write vec cid $! addToPointSum ps p
        mapM_ addpoint points
        return vec
      )
  where mkpair c p = (c, sqDistance (clusterCent c) p)