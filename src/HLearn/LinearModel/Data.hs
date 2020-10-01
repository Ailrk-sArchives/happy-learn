{-# LANGUAGE TemplateHaskell #-}
module HLearn.LinearModel.Data where

import qualified Data.Matrix                   as M
import qualified Data.Vector.Unboxed           as U
import           HLearn.Internal.Data
import           Lens.Micro.Platform           as L
                                         hiding ( assign )

data LinearModel = LinearModel { linmodY :: Point
                               , linmodX :: !(M.Matrix Double)
                               , linmodBeta :: Point
                               , linmodSemga :: U.Vector Double
                               }
L.makeLenses ''LinearModel
