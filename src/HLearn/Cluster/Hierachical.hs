module HLearn.Cluster.Hierachical where


import           Data.List
import           Data.Function                  ( on )
import           Control.Monad.ST
import           Data.STRef
import qualified Data.Vector.Unboxed           as U
import qualified Data.Vector                   as Vec
import qualified Data.Vector.Mutable           as MVec
import           HLearn.Cluster.Error
import           HLearn.Cluster.Data
import           HLearn.Internal.Data
import           HLearn.Internal.Metric
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           System.Random
import qualified Lens.Micro.Platform           as L
import           Lens.Micro.Platform     hiding ( assign )


data AgglomerativeState = AgglomeratieState {

                                            }


