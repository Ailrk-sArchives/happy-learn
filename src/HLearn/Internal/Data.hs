{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module HLearn.Internal.Data
  ( Point (..),
    PointPair (..),
    Distance (..),
    NonEmptyPointList (..),
    makeNonEmptyPointList,
    mapPoint,
    makePoint,
    makePointPair,
    zeroPoint,
  )
where

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Unboxed as RP
import qualified Data.Vector as V
import qualified Data.List.NonEmpty as NL
import HLearn.Internal.Error

{- Define some generic types that can be used for all algorithms. -}

-- | Used to index.
data Point a = Point
  { unPoint :: !(V.Vector a) -- coordinate of the point.
  }
  deriving (Show, Read, Eq, Functor)

-- | PointPair guarantee two points are of the same shape.
data PointPair a = PointPair !(Point a) !(Point a)

data Distance a where
  Distance :: Num a => a -> Distance a

mapPoint :: (a -> b) -> Point a -> Either InternalError (Point b)
mapPoint f (Point vs)
  | True = undefined
  | otherwise = undefined
  where
    vs' = f <$> vs

inBoundCheck :: (Ord a, Num a, R.Shape sh) => sh -> Point a -> Bool
inBoundCheck sh (Point xs) = any (== False) $ zipWith (<) shlist (V.toList xs)
  where
    shlist = fromIntegral <$> R.listOfShape sh
{-# INLINE inBoundCheck #-}

-- | Guarantee point pair retured are of the same shape.
makePointPair ::
  (R.Shape sh, RP.Unbox a, Ord a, Num a) =>
  sh ->
  Point a ->
  Point a ->
  Either InternalError (PointPair a)
makePointPair sh as@(Point ad) bs@(Point bd)
  | length ad /= length bd =
    Left $ DataError $ leftMsg <> "points have different shape"
  | all (== True) (not . inBoundCheck sh <$> [as, bs]) =
    Left $ IndexError $ leftMsg <> "failed inbound check"
  | otherwise = Right $ PointPair as bs
  where
    leftMsg = "Failed to make point pair: "

-- | Make sure the point can be used to index shape sh.
validatePoint ::
  (Num a, Ord a, R.Shape sh) => sh -> Point a -> Either InternalError (Point a)
validatePoint sh p@(Point vs)
  | R.rank sh /= length vs = Left $ DataError "Inconsisent Point, shape mismatch"
  | inBoundCheck sh p = Left $ DataError "Point failed inbound check"
  | otherwise = Right $ p

makePoint ::
  (Num a, Ord a, R.Shape sh) => sh -> V.Vector a -> Either InternalError (Point a)
makePoint sh = validatePoint sh . Point

-- | Make zero point.
zeroPoint :: (Ord a, Num a) => Int -> Point a
zeroPoint dim =
  Point
    { unPoint = V.replicate dim 0
    }
{-# INLINE zeroPoint #-}

-- | Points with the same shape
--    NonEmptyPointList guarantee all points has the same size
newtype NonEmptyPointList a = NonEmptyPointList
  {unPointArray :: NL.NonEmpty (Point a)}

-- | Return Right if all elements in the list have the same shape
makeNonEmptyPointList :: [Point a] -> Either InternalError (NonEmptyPointList a)
makeNonEmptyPointList [] = Left $ DataError "PointArray is non empty"
makeNonEmptyPointList xs
  | let dims = (length . unPoint <$> xs)
        x = head dims
        eqlen = length $ filter (x ==) dims
     in eqlen == length dims =
    Right $ NonEmptyPointList $ NL.fromList xs
  | otherwise = Left $ DataError "Exists element in the list with different shape"

-- | Compress Points into compact repa array.
compactPoints :: R.Shape sh => NonEmptyPointList a -> R.Array R.U sh a
compactPoints (NonEmptyPointList t@(x NL.:| _)) = undefined
