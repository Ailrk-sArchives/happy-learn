{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module HLearn.Cluster.Data where

import           HLearn.Internal.Data
import           HLearn.Internal.Metric
import           Data.Vector.Generic           as G
import qualified Data.Vector                   as Vec
import qualified Data.Vector.Mutable           as MVec
import           Control.Monad.State
import qualified Lens.Micro.Platform           as L

{- | Cluter data
Contains the id of the cluster, the center of the cluster, and points
associated to the cluster.
-}
data Cluster = Cluster { _clusterId :: !Int
                       , _clusterCent :: !Point
                       , _clusterPoints :: ![Point]} deriving (Eq, Show)
L.makeLenses ''Cluster
