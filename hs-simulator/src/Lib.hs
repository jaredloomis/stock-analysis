module Lib (
  someFunc
) where

import RIO

import qualified Data.Text.IO as T

someFunc :: RIO () ()
someFunc = liftIO $ T.putStrLn "someFunc"
