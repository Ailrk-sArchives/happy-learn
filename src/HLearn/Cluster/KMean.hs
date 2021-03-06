{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module HLearn.Cluster.KMean where

import           Data.List
import           Data.Function                  ( on )
import           System.IO
import           System.Random
import           HLearn.Internal.Data
import           HLearn.Cluster.Data
import           HLearn.Cluster.Error
import           HLearn.Internal.Metric
import qualified Data.Vector.Generic           as G
import qualified Data.Vector                   as Vec
import qualified Data.Vector.Mutable           as MVec
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.DeepSeq
import qualified Lens.Micro.Platform           as L
import           Lens.Micro.Platform     hiding ( assign )

limit = 100

data PointSum = PointSum { _psumCount :: !Int
                         , _psumCenter :: !Point
                         , _psumPoints :: ![Point]
                         } deriving (Eq, Show)
L.makeLenses ''PointSum

instance NFData PointSum where
  rnf (PointSum count (Point dim xs) ps) = ()

-- | indicate if two clusters are close enough that we can
-- treet them as the same cluster
sameCluster :: Cluster -> Cluster -> Bool
sameCluster (Cluster a _ _) (Cluster b _ _) | a /= b = False
sameCluster (Cluster _ as _) (Cluster _ bs _) = (sqDistance as bs) < 0.001


data KMeanEnv = KMeanEnv { kenvDim :: Int
                         , kenvNum :: Int -- number of clusters
                         , kenvPoints :: [Point]
                         , kenvBound :: [(Double, Double)]
                         }

data KMeanState = KMeanState { ksClusters :: [Cluster]
                             , ksPoints :: [Point]
                             }

type KMean' a = ExceptT ClusterError (ReaderT KMeanEnv (State KMeanState)) a


newtype KMean a = KMean { unKmean :: KMean' a }
  deriving (Functor, Applicative, Monad, MonadState KMeanState, MonadError ClusterError, MonadReader KMeanEnv)


runKmeanLloy :: KMeanEnv -> IO (Either ClusterError [Cluster])
runKmeanLloy env@(KMeanEnv dim ncluster points bounds) = do
  cs <- initClusters
  let s = initState cs
  return $ flip evalState s $ flip runReaderT env $ runExceptT
    (unKmean kmeanLloy)
 where
  initClusters = do
    ps <- replicateM ncluster $ randomPoint bounds
    return [ Cluster idx p [] | (idx, p) <- [0 ..] `zip` ps ]
  initState cl = KMeanState { ksClusters = cl, ksPoints = points }


kmeanLloy :: KMean [Cluster]
kmeanLloy = loop 0
 where
  loop :: Int -> KMean [Cluster]
  loop n = do
    s@(KMeanState clusters points) <- get
    KMeanEnv _ ncluster _ _        <- ask
    when (ncluster > limit) $ do
      liftEither $ Left (ClusterInitError "too many clusters")
    clusters' <- step
    return clusters'
    if and $ zipWith sameCluster clusters' clusters
      then return clusters
      else put (s { ksClusters = clusters' }) >> loop (n + 1)


step :: KMean [Cluster]
step = assign >>= newCluster


assign :: KMean (Vec.Vector PointSum)
assign = do
  KMeanState clusters points <- get
  ncluster                   <- kenvNum <$> ask
  ndim                       <- kenvDim <$> ask
  let nearestCluster p =
        fst $ minimumBy (compare `on` snd) [ mkpair c p | c <- clusters ]
  return
    $ (Vec.create $ do
        vec <- MVec.replicate ncluster (PointSum 0 (zeroPoint ndim) [])
        let addpoint p = do
              let c   = nearestCluster p
                  cid = _clusterId c
              psum <- MVec.read vec cid
              MVec.write vec cid $! addToPointSum psum p
        mapM_ addpoint points
        return vec
      )
  where mkpair c p = (c, sqDistance (_clusterCent c) p)


addToPointSum :: PointSum -> Point -> PointSum
addToPointSum psum p@(Point _ xs) =
  psum
    & (psumCount +~ 1)
    & (psumCenter . unPoint %~ (\ps -> G.zipWith (+) xs ps))
    & (psumPoints %~ (\points -> p : points))


pointSumToCluster :: Int -> PointSum -> Cluster
pointSumToCluster cid ps@(PointSum count p points) = Cluster
  { _clusterId = cid
  , _clusterCent = p { _unPoint = G.map (/ (fromIntegral count)) $ _unPoint p }
  , _clusterPoints = points
  }


-- | filter away clustser has no point close to it.

newCluster :: (MonadState s m) => Vec.Vector PointSum -> m [Cluster]
newCluster vec =
  return
    $ [ pointSumToCluster i ps
      | (i, ps) <- [0 ..] `Prelude.zip` (Vec.toList vec)
      , ps ^. psumCount > 0
      ]
