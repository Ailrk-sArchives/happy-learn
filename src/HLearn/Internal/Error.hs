module HLearn.Internal.Error where

data InternalError
  = MetricsError String
  | DataError String
  | IndexError String
