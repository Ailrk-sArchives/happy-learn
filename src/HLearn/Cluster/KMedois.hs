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
import qualified Lens.Micro.Platform           as L
import           Lens.Micro.Platform     hiding ( assign )

-- | record the best medoid and o in each cluster.
data PreSwaped = PreSwaped { preswapCi :: !Int
                           , preswapPi :: !Int
                           -- points associated with swapped cluster cneter
                           , preswapPoints :: ![Point]
                           }

limit = 100
data KMedoisConfig = KMedoisConfig { kmedconfigDim :: Int
                                   , kmedconfigNum :: Int
                                   , kmedconfigPoints :: [Point]
                                   , kmedconfigBound :: [(Double, Double)]
                                   }

data KMedoisState = KMedoisState { kmedDim :: Int
                                 , kmedNum :: Int
                                 , kmedClusters :: [Cluster]
                                 , kmedPoints :: [Point]
                                 , kmedCostImproved :: Bool
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
    return
      [ Cluster { _clusterId     = idx
                , _clusterCent   = (points !! idx)
                , _clusterPoints = []
                }
      | idx <- idxs
      ]  -- randomly choose
  initState cl = KMedoisState dim ncluster cl points False

kmedois :: KMedois [Cluster]
kmedois = loop 0
 where
  loop :: Int -> KMedois [Cluster]
  loop n = do
    s@(KMedoisState _ ncluster clusters _ costImproved) <- get
    when (ncluster > limit) $ do
      liftEither $ Left (ClusterInitError "too many clusters")
    clusters' <- step
    return clusters'
    if costImproved
      then return clusters
      else put (s { kmedClusters = clusters' }) >> loop (n + 1)

step :: KMedois [Cluster]
step = assign >>= newCluster


-- for each medoids m and for each non medoids o
-- try swap m and o, compute the cost, keep the swap of the best cost.
-- then perform best swap of mbest and obest.
assign :: KMedois (Vec.Vector PreSwaped)
assign = do
  s@(KMedoisState dim ncluster clusters points _) <- get
  return
    $ (Vec.create $ do
        vec <- MVec.replicate ncluster (PreSwaped 0 0 [])
        let addBestSwap c@(Cluster cid _ _) = do
              let bs = localBestSwap c s
              ps <- MVec.read vec cid
              MVec.write vec cid $! bs
        mapM_ addBestSwap clusters
        return vec
      )
 where
  localBestSwap :: Cluster -> KMedoisState -> PreSwaped
  localBestSwap (Cluster ci _ _) s@(KMedoisState _ _ cs ps _) = fst $ minimumBy
    (compare `on` snd)
    [ let cost  = getCost $ swap (ci, pi) s
          pswap = PreSwaped ci pi []
      in  (pswap, cost)
    | pi <- [0 .. length ps]
    ]

-- | swap a point and a cluster, return the swap as new kmedoids state
swap :: (Int, Int) -> KMedoisState -> KMedoisState
swap (ci, pi) s@(KMedoisState _ _ cs ps _) =
  let clusterP = cs !! ci ^. clusterCent
      pointsP  = ps !! pi
  in  s { kmedClusters = cs & (ix ci) . clusterCent .~ pointsP
        , kmedPoints   = ps & (ix pi) .~ clusterP
        }

getCost :: KMedoisState -> Double
getCost (KMedoisState _ _ clusters points _) =
  sum [ euclideanDistance p c | (Cluster _ c _) <- clusters, p <- points ]

isCostImproved :: Double -> Double -> Bool
isCostImproved old new | new - old < 0 = True
                       | otherwise     = False


-- | filter away clustser has no point close to it.
newCluster :: Vec.Vector PreSwaped -> KMedois [Cluster]
newCluster vec = return $ []
