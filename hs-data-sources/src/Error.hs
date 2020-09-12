module Error where

import Data.Text (Text)
import Servant.Client (ClientError)

data StockError =
    StockApiError ClientError
  | StockError Text
  deriving (Show, Eq)