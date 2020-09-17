{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

import Control.Monad (replicateM_)
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Unboxed as RP
import qualified Data.List.NonEmpty as NL
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as V
import HLearn.Internal.Error

{- Define some generic types that can be used for all algorithms. -}

-- ------------------------------------------------------------------------------
-- data definitions

-- | Used to index.
data Point a = Point
  { unPoint :: {-# UNPACK #-} !(V.Vector a) -- coordinate of the point.
  }
  deriving (Show, Read, Eq)

-- | PointPair guarantee two points are of the same shape.
data PointPair a = PointPair {-# UNPACK #-} !(Point a) {-# UNPACK #-} !(Point a)

data Distance a where
  Distance :: Num a => a -> Distance a

-- | Points with the same shape
--    NonEmptyPointList guarantee all points has the same size
newtype NonEmptyPointList a = NonEmptyPointList
  {unPointArray :: NL.NonEmpty (Point a)}
  deriving (Show)

-- ------------------------------------------------------------------------------
-- internal definitions

inBoundCheck :: (V.Unbox a, Ord a, Num a, R.Shape sh) => sh -> Point a -> Bool
inBoundCheck sh (Point xs) = any (== False) $ zipWith (<) shlist vs
  where
    shlist = fromIntegral <$> R.listOfShape sh
    vs = V.toList xs
{-# INLINE inBoundCheck #-}

-- | Make sure the point can be used to index shape sh.
validatePoint ::
  (V.Unbox a, Num a, Ord a, R.Shape sh) =>
  sh ->
  Point a ->
  Either InternalError (Point a)
validatePoint sh p@(Point vs)
  | shapeMismatch = Left $ DataError "Inconsisent Point, shape mismatch"
  | outOfBound = Left $ DataError "Point failed inbound check"
  | otherwise = Right $ p
  where
    shapeMismatch = R.rank sh /= V.length vs
    outOfBound = inBoundCheck sh p

-- ------------------------------------------------------------------------------
-- smart constructors

-- | Guarantee point pair retured are of the same shape.
makePointPair ::
  (R.Shape sh, V.Unbox a, Ord a, Num a) =>
  sh ->
  Point a ->
  Point a ->
  Either InternalError (PointPair a)
makePointPair sh as@(Point ad) bs@(Point bd)
  | shapeMismatch = Left $ DataError $ leftMsg <> "points have different shape"
  | outOfBound = Left $ IndexError $ leftMsg <> "failed inbound check"
  | otherwise = Right $ PointPair as bs
  where
    leftMsg = "Failed to make point pair: "
    shapeMismatch = V.length ad /= V.length bd
    outOfBound = all (== True) (not . inBoundCheck sh <$> [as, bs])

makePoint ::
  (V.Unbox a, Num a, Ord a, R.Shape sh) => sh -> V.Vector a -> Either InternalError (Point a)
makePoint sh = validatePoint sh . Point

-- | Make zero point.
zeroPoint :: (V.Unbox a, Ord a, Num a) => Int -> Point a
zeroPoint dim =
  Point
    { unPoint = V.replicate dim 0
    }
{-# INLINE zeroPoint #-}

-- | Return Right if all elements in the list have the same shape
makeNonEmptyPointList :: V.Unbox a => [Point a] -> Either InternalError (NonEmptyPointList a)
makeNonEmptyPointList [] = Left $ DataError "PointArray is non empty"
makeNonEmptyPointList xs
  | let dims = (V.length . unPoint <$> xs)
        x = head dims
        eqlen = length $ filter (x ==) dims
     in eqlen == length dims =
    Right $ NonEmptyPointList $ NL.fromList xs
  | otherwise = Left $ DataError "Exists element in the list with different shape"

-- ------------------------------------------------------------------------------
-- operations

-- | Compress Points into compact repa array.
--     this will return an 1d array with shape (Z :. len :. rank)
compactPoints :: NonEmptyPointList a -> R.Array R.U R.DIM2 a
compactPoints (NonEmptyPointList t@(x NL.:| _)) = undefined

-- where
--   rank' = length . unPoint $ x
--   x' = length t
--   sh = R.Z R.:. x' R.:. rank'
--   unboxedArray = V.create $ do
--     let idxedList = [0 ..] `zip` (NL.toList t)
--         totalSize = length t * V.length (unPoint x)
--     v <- MV.new $ totalSize
--     sequence_
--       [ MV.write v (i * rank' + j) e
--         | (i, pv) <- idxedList,
--           (j, e) <- [0 ..] `zip` (V.toList . unPoint $ pv)
--       ]
--     return v

mapPoint :: (a -> b) -> Point a -> Either InternalError (Point b)
mapPoint f (Point vs)
  | True = undefined
  | otherwise = undefined
