module Error where

import Data.Text (Text)
import Servant.Client (ClientError)

data StockError =
    StockApiError ClientError
  | LocalIOError String
  | StockError Text
  deriving (Show, Eq)