{-# LANGUAGE DeriveGeneric #-}
 {-# LANGUAGE OverloadedStrings #-}
module HLearn.DataSet.Import where

import           HLearn.Internal.Data
import           Data.Aeson
import           GHC.Generics
import           Data.ByteString.Lazy.Char8     ( pack )
import qualified Data.Vector.Unboxed           as U

newtype ClusterData = ClusterData [Point] deriving (Show, Generic)

instance FromJSON ClusterData
instance ToJSON ClusterData

readCluserData :: IO ClusterData
readCluserData = readCluserData' "data/clusterData.json"

-- | clusterData is a 2d test data
readCluserData' :: String -> IO ClusterData
readCluserData' n = do
  contents <- readFile n
  print contents
  return $ ClusterData $ case (decode $ pack contents) :: Maybe [[Double]] of
    Just c  -> (Point 2) . U.fromList <$> transpose c
    Nothing -> []

transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose x        = (map head x) : transpose (map tail x)
