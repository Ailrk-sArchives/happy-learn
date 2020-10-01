{-# LANGUAGE TemplateHaskell #-}
module HLearn.LinearModel.Data where

import qualified Data.Matrix                   as M
import qualified Data.Vector.Unboxed           as U
import           HLearn.Internal.Data
import           Lens.Micro.Platform           as L
                                         hiding ( assign )

-- | Y = bX + s. Known Y, and X trying to find b and s.
data LinearModelData = LinearModelData { linmodDataX :: M.Matrix Double
                                       , linmodDataY:: Point
                                       }
L.makeLenses ''LinearModelData


data LinearModel = LinearModel { linmodBetaHat :: Point
                               , linmodResidue :: Point
                               }
L.makeLenses ''LinearModel
