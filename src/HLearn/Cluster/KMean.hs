{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HLearn.Cluster.KMean where

import           Data.List
import           Data.Function                  ( on )
import           Control.Monad.State
import           System.IO
import           HLearn.Internal.Data
import           HLearn.Cluster.Data
import           HLearn.Cluster.Error
import           HLearn.Internal.Metric
import qualified Data.Vector                   as Vec
import qualified Data.Vector.Mutable           as MVec
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           System.Random
import           Debug.Trace

limit = 100

data KMeanConfig = KMeanConfig { kconfigdim :: Int
                               , kconfignum :: Int
                               , kconfigpoints :: [Point]
                               , kconfigbound :: [(Double, Double)]
                               }

data KMeanState = KMeanState { ksdim :: Int
                             , ksnum :: Int  -- number of clusters
                             , ksclusters :: [Cluster]
                             , kspoints :: [Point]
                             }

type KMean' a = ExceptT ClusterError (State KMeanState) a

newtype KMean a = KMean { unKmean :: KMean' a }
  deriving (Functor, Applicative, Monad, MonadState KMeanState, MonadError ClusterError)

runKmeanLloy :: KMeanConfig -> IO (Either ClusterError [Cluster])
runKmeanLloy (KMeanConfig dim ncluster points bounds) = do
  cs <- initClusters
  let s = initState cs
  return $ flip evalState s $ runExceptT (unKmean kmeanLloy)
 where
  initClusters = do
    ps <- replicateM ncluster $ randomPoint bounds
    return [ Cluster idx p | (idx, p) <- [0 ..] `zip` ps ]
  initState cl = KMeanState { ksdim      = dim
                            , ksnum      = ncluster
                            , ksclusters = cl
                            , kspoints   = points
                            }

kmeanLloy :: KMean [Cluster]
kmeanLloy = loop 0
 where
  loop :: Int -> KMean [Cluster]
  loop n = do
    s@(KMeanState _ ncluster clusters _) <- get
    when (ncluster > limit) $ do
      liftEither $ Left (ClusterInitError "too many clusters")
    clusters' <- step
    return clusters'
    if and $ zipWith sameCluster clusters' clusters
      then return clusters
      else put (s { ksclusters = clusters' }) >> loop (n + 1)

step :: KMean [Cluster]
step = assign >>= newCluster

assign :: KMean (Vec.Vector PointSum)
assign = do
  KMeanState dim ncluster clusters points <- get
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
