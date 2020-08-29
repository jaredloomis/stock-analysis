module Error where

import Servant.Client (ClientError)

data StockError =
  StockApiError ClientError
  deriving (Show, Eq)