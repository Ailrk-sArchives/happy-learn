{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module HLearn.Cluster.Data where

import           HLearn.Internal.Data
import           HLearn.Internal.Metric
import           Data.Vector.Generic           as G
import qualified Data.Vector                   as Vec
import qualified Data.Vector.Mutable           as MVec
import           Control.Monad.State

data Cluster = Cluster { clusterId :: !Int
                       , clusterCent :: !Point} deriving (Eq, Show)


