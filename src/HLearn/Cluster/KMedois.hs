{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HLearn.Cluster.KMedois where


import           Data.List
import           Data.Function                  ( on )
import           Control.Monad.ST
import           Data.STRef
import qualified Data.Vector.Unboxed           as U
import qualified Data.Vector                   as Vec
import qualified Data.Vector.Mutable           as MVec
import           HLearn.Cluster.Error
import           HLearn.Cluster.Data
import           HLearn.Internal.Data
import           HLearn.Internal.Metric
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           System.Random

--  record the best medoid and o in each cluster.
data PreSwaped = PreSwaped { medoidId :: !Int
                           , oId :: !Int
                           }

limit = 100
data KMedoisConfig = KMedoisConfig { kmedconfigdim :: Int
                                   , kmedconfignum :: Int
                                   , kmedconfigpoints :: [Point]
                                   , kmedconfigbound :: [(Double, Double)]
                                   }

data KMedoisState = KMedoisState { kmeddim :: Int
                                 , kmednum :: Int
                                 , kmedclusters :: [Cluster]
                                 , kmedpoints :: [Point]
                                 }

type KMedois' a = ExceptT ClusterError (State KMedoisState) a

newtype KMedois a = KMean { unKmean :: KMedois' a }
  deriving (Functor, Applicative, Monad, MonadState KMedoisState, MonadError ClusterError)

runKmedois :: KMedoisConfig -> IO (Either ClusterError [Cluster])
runKmedois (KMedoisConfig dim ncluster points bounds) = do
  cs <- initClusters
  let s = initState cs
  return $ flip evalState s $ runExceptT (unKmean kmedois)
 where
  initClusters = do
    idxs <- replicateM ncluster $ randomRIO (0, length points)
    return [ Cluster idx (points !! idx) | idx <- idxs ]  -- randomly choose
  initState cl = KMedoisState { kmeddim      = dim
                              , kmednum      = ncluster
                              , kmedclusters = cl
                              , kmedpoints   = points
                              }

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
    if undefined -- TODO
      then return clusters
      else put (s { kmedclusters = clusters' }) >> loop (n + 1)

step :: KMedois [Cluster]
step = assign >>= newCluster

-- for each medoids m and for each non medoids o
-- try swap m and o, compute the cost, keep the swap of the best cost.
-- then perform best swap of mbest and obest.
assign :: KMedois (Vec.Vector PreSwaped)
assign = do
  s@(KMedoisState dim ncluster clusters points) <- get
  return
    $ (Vec.create $ do
        vec <- MVec.replicate ncluster (PreSwaped 0 0)
        let localBestSwap :: Cluster -> KMedoisState -> PreSwaped
            localBestSwap (Cluster ci _) s@(KMedoisState _ _ cs ps) =
              fst $ minimumBy
                (compare `on` snd)
                [ (PreSwaped ci pi, cost $ swap ci pi s)
                | pi <- [0 .. length ps]
                ]

        let addBestSwap c@(Cluster cid _) = do
              let bs = localBestSwap c s
              ps <- MVec.read vec cid
              MVec.write vec cid $! bs

        mapM_ addBestSwap clusters
        return vec
      )

-- | swap a point and a cluster
swap :: Int -> Int -> KMedoisState -> KMedoisState
swap ci pi s@(KMedoisState dim _ cs ps) = runST $ do
  csPointRef <- newSTRef $ clusterCent $ cs !! ci
  psPointRef <- newSTRef $ ps !! pi
  temp <- readSTRef csPointRef
  writeSTRef csPointRef (ps !! pi)
  writeSTRef psPointRef temp
  return s

cost :: KMedoisState -> Double
cost (KMedoisState _ _ clusters points) =
  sum [ euclideanDistance p c | (Cluster _ c) <- clusters, p <- points ]

-- | filter away clustser has no point close to it.
newCluster :: Vec.Vector PreSwaped -> KMedois [Cluster]
newCluster vec = return $ []
