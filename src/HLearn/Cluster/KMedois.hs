{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module HLearn.Cluster.KMedois where


import           Data.List
import           Data.Function                  ( on )
import qualified Data.Vector.Unboxed           as U
import qualified Data.Vector                   as Vec
import qualified Data.Vector.Mutable           as MVec
import           HLearn.Cluster.Error
import           HLearn.Cluster.Data
import           HLearn.Internal.Data
import           HLearn.Internal.Metric
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           System.Random
import qualified Lens.Micro.Platform           as L
import           Lens.Micro.Platform     hiding ( assign )


-- | record the best medoid and o in each cluster.
data LocalSwap = LocalSwap { _localSwapCi :: !Int
                           , _localSwapPi :: !Int
                           -- points associated with the cluster cneter
                           , _localSwapPoints :: ![Point]
                           }
L.makeLenses ''LocalSwap

limit = 100


data KMedoisEnv = KMedoisEnv { kmedEnvDim :: Int
                             , kmedEnvNum :: Int
                             , kmedPoints :: [Point]
                             , kmedEnvBound :: [(Double, Double)]
                             }


data KMedoisState = KMedoisState { kmedClusters :: [Cluster]
                                 , kmedCostImproved :: Bool
                                 }


type KMedois' a
  = ExceptT ClusterError (ReaderT KMedoisEnv (State KMedoisState)) a


newtype KMedois a = KMean { unKmean :: KMedois' a }
  deriving (Functor, Applicative, Monad, MonadState KMedoisState, MonadError ClusterError, MonadReader KMedoisEnv)


runKmedois :: KMedoisEnv -> IO (Either ClusterError [Cluster])
runKmedois env@(KMedoisEnv dim ncluster points bounds) = do
  cs <- initClusters
  let s = initState cs
  return $ flip evalState s $ flip runReaderT env $ runExceptT (unKmean kmedois)
 where
  initClusters = do
    idxs <- replicateM ncluster $ randomRIO (0, length points)
    return [ Cluster idx (points !! idx) [] | idx <- idxs ]  -- randomly choose
  initState cl = KMedoisState cl False


kmedois :: KMedois [Cluster]
kmedois = loop 0
 where
  loop :: Int -> KMedois [Cluster]
  loop n = do
    s@(KMedoisState clusters costImproved) <- get
    ncluster                               <- kmedEnvNum <$> ask
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
-- Note: Each local swap will convert to the next cluster.
--       When constructing the local swap points assoicated with the same
--       cluster will also be added.
assign :: KMedois (Vec.Vector LocalSwap)
assign = do
  s@(KMedoisState clusters _) <- get
  ncluster                    <- kmedEnvNum <$> ask
  points                      <- kmedPoints <$> ask

  let nearestCluster p =
        fst $ minimumBy (compare `on` snd) [ mkpair c p | c <- clusters ]

  return
    $ (Vec.create $ do
        vec <- MVec.replicate ncluster (LocalSwap 0 0 [])

        let
          addPoint p = do
            let c   = nearestCluster p
                cid = _clusterId c
            lswap <- MVec.read vec cid
            MVec.write vec cid $! addToLocalSwap lswap p

          -- try to swap cluster center with all points in the cluster.
          -- write the best swap to vec.
          doBestSwap c@(Cluster cid _ _) = do
            let (ci, pi) = localBestSwap c points s
            ps <- MVec.read vec cid
            MVec.write vec cid $! ps { _localSwapCi = ci, _localSwapPi = pi }

        mapM_ addPoint   points   -- add associated points to each local swaps
        mapM_ doBestSwap clusters -- try swap and record
        return vec
      )
  where mkpair c p = (c, sqDistance (_clusterCent c) p)


addToLocalSwap :: LocalSwap -> Point -> LocalSwap
addToLocalSwap lswap p = lswap & localSwapPoints %~ (\points -> p : points)


-- | Swap each point with the center in the same cluster, return the best swap.
localBestSwap :: Cluster -> [Point] -> KMedoisState -> (Int, Int)
localBestSwap (Cluster ci _ _) points s = fst $ minimumBy
  (compare `on` snd)
  [ let cost = getLocalCost $ swap (ci, pi) s in ((ci, pi), cost)
  | pi <- [0 .. length points]
  ]
 where
  -- swap a point and a cluster, return the swap as new kmedoids state
  -- Note: because cluster is also in points, swap bascially means replace
  --       the cluster point.
  swap (ci, pi) s@(KMedoisState clusters _) =
    let pointsP = points !! pi
    in  s { kmedClusters = clusters & (ix ci) . clusterCent .~ pointsP }
  getLocalCost :: KMedoisState -> Double
  getLocalCost = undefined



isCostImproved :: Double -> Double -> Bool
isCostImproved old new | new - old < 0 = True
                       | otherwise     = False


-- | filter away clustser has no point close to it.
newCluster :: Vec.Vector LocalSwap -> KMedois [Cluster]
newCluster vec = undefined
 -- where
 --  getCost (KMedoisState clusters _) =
 --    sum [ euclideanDistance p c | (Cluster _ c _) <- clusters, p <- points ]




-- newCluster vec =
--   return
--     $ [ pointSumToCluster i ps
--       | (i, ps) <- [0 ..] `Prelude.zip` (Vec.toList vec)
--       , ps ^. psumCount > 0
--       ]
