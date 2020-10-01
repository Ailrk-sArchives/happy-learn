module ClusterSpec where

import           HLearn.Cluster.KMean
import           HLearn.Cluster.Data
import           HLearn.Internal.Data
import           HLearn.DataSet.Import
import           Test.Hspec
import           Graphics.Matplotlib
import           Data.Vector.Unboxed           as U
import           Control.Monad.IO.Class
import           Debug.Trace

spec :: Spec
spec = do
  it "kmean" $ do
    val <- liftIO kmeanTest
    val `shouldBe` ()

-- | 2d cluster
kmeanTest :: IO ()
kmeanTest = do
  ClusterData d <- readCluserData
  let a  = pToList d
      a' = transpose a
  result1 <- runKmeanLloy (KMeanConfig 2 3 d [(0, 100), (0, 100)])
  result2 <- runKmeanLloy (KMeanConfig 2 2 d [(0, 100), (0, 100)])
  case sequenceA [result1, result2] of
    Left  s           -> return ()
    Right (v : u : _) -> do
      let v' = transpose . pToList $ _clusterCent <$> v
      let u' = transpose . pToList $ _clusterCent <$> u
      onscreen
        $ (  addSubplot (2 :: Int) (1 :: Int) (1 :: Int)
          %  scatter (a' Prelude.!! 0) (a' Prelude.!! 1)
          %  scatter (v' Prelude.!! 0) (v' Prelude.!! 1)
          @@ [o2 "color" "r"]
          %  title "KMean"
          )
        % (  addSubplot (2 :: Int) (1 :: Int) (2 :: Int)
          %  scatter (a' Prelude.!! 0) (a' Prelude.!! 1)
          %  scatter (u' Prelude.!! 0) (u' Prelude.!! 1)
          @@ [o2 "color" "r"]
          %  title "KMean1"
          )
  where pToList d = (\(Point _ v) -> U.toList v) <$> d
