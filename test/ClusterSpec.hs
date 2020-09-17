module ClusterSpec where

import HLearn.Cluster.KMean
import HLearn.Internal.Data
import Test.Hspec

spec :: Spec
spec = do
  it "base" $
    True `shouldBe` True
