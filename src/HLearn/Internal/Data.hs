{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module HLearn.Internal.Data
  ( Point (..),
    PointPair (..),
    Distance (..),
    mapPoint,
    makePoint,
    makePointPair,
    zeroPoint,
  )
where

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Unboxed as RP
import qualified Data.Vector as V
import HLearn.Internal.Error

{- Define some generic types that can be used for all algorithms. -}

-- used to index.
data Point sh a = Point
  { unDim :: {-# UNPACK #-} !sh, -- shape of the array the point is used to index.
    unPoint :: {-# UNPACK #-} !(V.Vector a) -- coordinate of the point.
  }
  deriving (Show, Read, Eq)

data PointPair sh a
  = PointPair
      {-# UNPACK #-} !(Point sh a)
      {-# UNPACK #-} !(Point sh a)

data Distance a where
  Distance :: Num a => a -> Distance a

instance R.Shape sh => Foldable (Point sh) where
  foldr f base (Point _ vs) = foldr f base vs

mapPoint :: (a -> b) -> Point sh a -> Either InternalError (Point sh b)
mapPoint f (Point sh vs)
  | True = undefined
  | otherwise = undefined
  where
    vs' = f <$> vs

-- guarantee point pair retured are of the same shape.
makePointPair ::
  (R.Shape sh, RP.Unbox a) => Point sh a -> Point sh a -> Either InternalError (PointPair sh a)
makePointPair as@(Point ad _) bs@(Point bd _)
  | R.rank ad == R.rank bd = Right $ PointPair as bs
  | otherwise = Left $ DataError "Failed to make point pair with different shapes"

-- make sure the point can be used to index shape sh.
validPoint :: (Num a, Ord a, R.Shape sh) => Point sh a -> Either InternalError (Point sh a)
validPoint p@(Point sh vs)
  | R.rank sh /= length vs = Left $ DataError "Inconsisent Point, shape mismatch"
  | any (== False) $ zipWith (<) shlist xs = Left $ DataError "wrong data"
  | otherwise = Right $ p
  where
    shlist = fromIntegral <$> R.listOfShape sh
    xs = V.toList vs

-- return Right if all elements in the list have the same shape
validatePointsShape :: (R.Shape sh) => [Point sh a] -> Either InternalError [Point sh a]
validatePointsShape [] = Right []
validatePointsShape xs
  | (unDim <$> xs) = undefined
  | otherwise = undefined

-- compress Points into compact repa array.
compactPoints :: R.Shape sh => [Point sh a] -> R.Array R.U sh a
compactPoints = undefined

makePoint ::
  (Num a, Ord a, R.Shape sh) => sh -> V.Vector a -> Either InternalError (Point sh a)
makePoint = (validPoint .) . Point

zeroPoint :: (Monoid a, Ord a, Num a, R.Shape sh) => sh -> Point sh a
zeroPoint sh =
  Point
    { unPoint = V.replicate (R.rank sh) 0,
      unDim = sh
    }
