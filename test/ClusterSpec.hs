module ClusterSpec where

import           HLearn.Cluster.KMean
import           HLearn.Cluster.Data
import           HLearn.Internal.Data
import           HLearn.DataSet.Import
import           Test.Hspec
import           Graphics.Matplotlib
import           Data.Vector.Unboxed           as U
import           Control.Monad.IO.Class

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
  onscreen $ scatter (a' Prelude.!! 0) (a' Prelude.!! 1)
  result <- runKmeanLloy (KMeanConfig 2 3 d [(0, 100), (0, 100)])
  case result of
    Left  _ -> return ()
    Right v -> do
      let v' = transpose $ pToList $ clusterCent <$> v
      onscreen $ scatter (v' Prelude.!! 0) (v' Prelude.!! 1)
 where
   pToList d = (\(Point _ v) -> U.toList v) <$> d
