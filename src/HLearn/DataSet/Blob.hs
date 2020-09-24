module HLearn.DataSet.Blob where

import           Data.Vector                   as Vec
import           HLearn.Internal.Data
import           System.Random

newtype Blob = Blob { unBlob :: Vec.Vector Point }

newtype BoundingBox = BoundingBox (Double, Double)
data BlobConfig = BlobConfig { blobNSample :: Int
                             , blobNFeatures :: Int
                             , blobCenters :: Int
                             , blobCenterBox :: BoundingBox
                             , blobClusterStd :: Double
                             }

makeBlob :: BlobConfig -> IO Blob
makeBlob (BlobConfig nSample nFeatures centers (BoundingBox (max, min)) clusterStd)
  = undefined
