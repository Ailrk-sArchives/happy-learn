module HLearn.Internal.Error where

data InternalError
  = MetricsError String
  | DataError String
  | IndexError String

data InternalWarning
  = UndefinedMetricsWarning String
  | EfficiencyWarning String
  | DataConversionWarning String
