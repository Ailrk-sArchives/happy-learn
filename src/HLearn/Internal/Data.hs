{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module HLearn.Internal.Data where

import           Data.Vector.Unboxed           as U
import           Data.Aeson
import           GHC.Generics
import           System.Random
import qualified Lens.Micro.Platform           as L


data Point = Point { _unDim :: !Int
                   , _unPoint :: !(U.Vector Double)
                   } deriving (Eq, Show, Generic)
L.makeLenses ''Point

instance FromJSON Point
instance ToJSON Point

data PointPair = PointPair !Point !Point

zeroPoint :: Int -> Point
zeroPoint n = Point { _unDim = n, _unPoint = U.replicate n 0 }

makePointPair :: Point -> Point -> Maybe PointPair
makePointPair p1 p2 | (_unDim p1) /= (_unDim p2) = Nothing
                    | otherwise                = Just $ PointPair p1 p2


-- | generate random point within the boundary
randomPoint :: [(Double, Double)] -> IO Point
randomPoint bounds = do
  let dim = Prelude.length bounds
  rp <- sequenceA $ randomRIO <$> bounds
  return $ Point dim (U.fromList rp)


