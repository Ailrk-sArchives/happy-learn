module HLearn.LinearModel.LinearRegression where

{-
   Multiple linear regression. Using the least square distance to fit
   linear model.

   Model:
    y = bX + s. Known Y, and X trying to find b and s.
    define the residual error:
      J(b, s) =  (1/2n) * Sum([si^2 | si <- s])
    The goal is to find b, s s.t J(b, s) is minimized.

   When J(b, s) = 0, Least square:
    b_hat = (X'X)^(-1) X'y
    s = avg(y) - avg(x)
-}


import           HLearn.LinearModel.Data
import           HLearn.Internal.Data
import           Data.Either
import qualified Data.Matrix                   as M

-- | b_hat

newtype LinearRegression = LinearRegression
  { linearRegressionPredict :: Point -> LinearModel }

-- | calculate (X'X)^(-1) X'
-- The hat matrix that map observed value y to y_hat on the regression
-- hyper plane.
hatMatrix :: LinearModelData -> Either String (M.Matrix Double)
hatMatrix (LinearModelData m _) = do
  let m' = M.transpose m
  mm'inv <- M.inverse (m' `M.multStd2` m)
  return $ m `M.multStd2` mm'inv `M.multStd2` m'

-- | fit model with input data.
fit :: LinearModelData -> LinearRegression
fit = undefined
