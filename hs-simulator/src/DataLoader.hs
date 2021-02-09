{-# LANGUAGE ScopedTypeVariables #-}
module DataLoader where

import RIO
import Prelude (putStrLn, print)

import Control.Monad.Free (Free(..))
import Data.Maybe (maybeToList, listToMaybe)
import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime)

import GHC.IO.Handle (mkFileHandle)
import System.IO (openTempFile, nativeNewline)
import System.Process
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Hashable (Hashable)

import qualified Data.DList as DL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as A
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

import IndicatorSample
import IndicatorArgs
import Time
import Strategy

--
-- DataLoader instances
--

class Functor f => DataLoader f i o a | a f o -> i, a f i -> o where
  {-# MINIMAL initLoader, (loadSample | loadSamples) #-}
  initLoader  :: a -> i -> NominalDiffTime -> (IndicatorTime, IndicatorTime) -> f o
  loadSample  :: a -> o -> Text -> IndicatorArgs -> f (Maybe IndicatorSample)
  loadSample d o t  = (listToMaybe <$>) . loadSamples d o t
  loadSamples :: a -> o -> Text -> IndicatorArgs -> f [IndicatorSample]
  loadSamples d o t = (maybeToList <$>) . loadSample d o t

data CachingDataLoader = CachingDataLoader

instance DataLoader IO (StrategyM ()) (HM.HashMap DataLoaderQuery [IndicatorSample]) CachingDataLoader where
  initLoader dataLoader strategy deltaTime timerange =
    let emptyHm = HM.empty :: HM.HashMap DataLoaderQuery [IndicatorSample]
    in foldStrategyOccasionally deltaTime timerange (InitDataLoader emptyHm) (Right emptyHm) strategy emptyHm $ \stmt _ _ _ acc ->
      case stmt of
        FetchSample  indicatorID (IndicatorArgs schedule args) _ -> do
          let queryConfig = DataLoaderQueryConfig [DataLoaderQuery indicatorID args schedule]
          samples <- withQueryConfig queryConfig executeDataLoader
          pure $ HM.insert (DataLoaderQuery indicatorID args schedule) samples acc
        FetchSamples indicatorID (IndicatorArgs schedule args) _ -> do
          let queryConfig = DataLoaderQueryConfig [DataLoaderQuery indicatorID args schedule]
          samples <- withQueryConfig queryConfig executeDataLoader
          pure $ HM.insert (DataLoaderQuery indicatorID args schedule) samples acc
        _ ->
          pure acc

  loadSamples _ cachedValues indicatorID iargs@(IndicatorArgs schedule args) = do
    -- Try to fetch sample from cache; if not, execute the data loader
    let cachedResult = join . maybeToList . HM.lookup (DataLoaderQuery indicatorID args schedule) $ cachedValues
    if length cachedResult > 0
      then pure cachedResult
      else withSingleQueryConfig indicatorID iargs executeDataLoader

data NaiveDataLoader = NaiveDataLoader

instance DataLoader IO (StrategyM ()) () NaiveDataLoader where
  initLoader _ _ _ _ = pure ()
  loadSamples _ _ indicatorID args =
    withSingleQueryConfig indicatorID args executeDataLoader

naiveDataLoader :: NaiveDataLoader
naiveDataLoader = NaiveDataLoader

data DummyDataLoader = DummyDataLoader

instance DataLoader IO (StrategyM ()) () DummyDataLoader where
  initLoader _ _ _ _ = pure mempty
  loadSamples _ _ indicatorID args = pure []

data InitDataLoader o = InitDataLoader o

instance DataLoader IO (StrategyM ()) o (InitDataLoader o) where
  initLoader (InitDataLoader o) _ _ _ = pure o
  loadSamples _ _ indicatorID args = pure []

--
-- Top-level functions - run a strategy with a data loader
--

executeStrategyPointInTime :: forall a o d. DataLoader IO (StrategyM ()) o d => IndicatorTime -> d -> StrategyM () -> IO [TradeLog]
executeStrategyPointInTime time dataLoader strategy =
  DL.toList <$> foldStrategyPointInTime time dataLoader (Left strategy) strategy DL.empty executeStrategy'
 where
  executeStrategy' (Trade ticker ty count ()) samples utime initOut acc = do
    price <- maybe (fetchSample initOut utime ticker) (pure . sampleValue)
      . listToMaybe
      . filter (\sample -> sampleStockID sample == ticker)
      $ samples
    pure $ DL.snoc acc (TradeLog ty ticker count utime price)
  executeStrategy' _ _ _ _ _ = pure DL.empty

  fetchSample initOut utime ticker =
    let sampleArgs = IndicatorArgs (IndicatorTime utime, IndicatorTime utime) $ HM.fromList [("tickers", Any . A.Array . V.fromList $ [A.String ticker])]
    in maybe 0 sampleValue <$> sampleGetter initOut "price" sampleArgs

  initializer :: IO o
  initializer   = initLoader dataLoader strategy (fromIntegral 0) (time, time)
  sampleGetter  = loadSample dataLoader

executeStrategyOccasionally :: DataLoader IO (StrategyM ()) o d => NominalDiffTime -> (IndicatorTime, IndicatorTime) -> d -> StrategyM () -> IO [TradeLog]
executeStrategyOccasionally deltaTime (startTime, endTime) dataLoader strategy = executeStrategy' startTime
 where
  executeStrategy' time
    | time > endTime = pure []
    | otherwise = do
      samplesThis <- executeStrategyPointInTime time dataLoader strategy -- TODO: execute initializer, then pass (Right o) to executeStrategyPointInTime
      samplesRest <- executeStrategy' =<< addTime time deltaTime
      pure $ samplesThis ++ samplesRest

executeStrategyDaily :: DataLoader IO (StrategyM ()) o d => (IndicatorTime, IndicatorTime) -> d -> StrategyM () -> IO [TradeLog]
executeStrategyDaily timerange dataLoader strategy = -- TODO: execute initializer, then pass (Right o) to executeStrategyOccasionally
  executeStrategyOccasionally (fromInteger $ 60*60*24) timerange dataLoader strategy


type StrategyFold m i o d c =
  d -> Either i o ->
  StrategyM () ->
  c -> (StrategyF () -> [IndicatorSample] -> UTCTime -> o -> c -> m c) ->
  m c

foldStrategyPointInTime :: (DataLoader m i o d, MonadIO m) => IndicatorTime -> StrategyFold m i o d c
foldStrategyPointInTime time dataLoader loaderInitVal strategy initAcc f =
  initializer >>= (\initOut -> foldStrategyPointInTime' initAcc initOut strategy)
 where
  foldStrategyPointInTime' acc initOut (Free (FetchSamples indicatorID args next)) = do
    let strippedStmt = FetchSamples indicatorID args (const ())
    samples <- samplesGetter initOut indicatorID args
    utime   <- utcTime
    acc'    <- f strippedStmt samples utime initOut acc
    foldStrategyPointInTime' acc' initOut (next samples)
  foldStrategyPointInTime' acc initOut (Free (FetchSample indicatorID args next)) = do
    let strippedStmt = FetchSample indicatorID args (const ())
    sample <- sampleGetter initOut indicatorID args
    utime  <- utcTime
    acc'   <- f strippedStmt (maybeToList sample) utime initOut acc
    foldStrategyPointInTime' acc' initOut (next sample)
  foldStrategyPointInTime' acc initOut (Free (Trade ticker ty count next)) = do
    let strippedStmt = Trade ticker ty count ()
    let sampleArgs = IndicatorArgs (time, time) $ HM.fromList [("tickers", Any . A.Array . V.fromList $ [A.String ticker])]
    price <- maybe 0 sampleValue <$> sampleGetter initOut "price" sampleArgs
    utime <- utcTime
    acc'  <- f strippedStmt [] utime initOut acc
    foldStrategyPointInTime' acc' initOut next
  foldStrategyPointInTime' acc initOut (Free (GetTime next)) = do
    let strippedStmt = GetTime (const ())
    utime <- utcTime
    acc'  <- f strippedStmt [] utime initOut acc
    foldStrategyPointInTime' acc initOut (next utime)
  foldStrategyPointInTime' acc _ (Pure x) = pure acc

  utcTime       = liftIO $ toUTC time
  initializer   = case loaderInitVal of
                    Left loaderIn   -> initLoader dataLoader loaderIn (fromIntegral 0) (time, time)
                    Right loaderOut -> pure loaderOut
  samplesGetter = loadSamples dataLoader
  sampleGetter  = loadSample dataLoader

foldStrategyOccasionally :: (DataLoader m i o d, MonadIO m) =>
  NominalDiffTime -> (IndicatorTime, IndicatorTime) -> StrategyFold m i o d c
foldStrategyOccasionally deltaTime (startTime, endTime) dataLoader loaderIn strategy initAcc f = foldStrategyOccasionally' initAcc startTime
 where
  foldStrategyOccasionally' acc time
    | time > endTime = pure acc
    | otherwise = do
      acc'  <- foldStrategyPointInTime time dataLoader loaderIn strategy initAcc f -- TODO: execute initializer, then pass (Right o) to foldStrategyPointInTime
      foldStrategyOccasionally' acc' =<< liftIO (addTime time deltaTime)

foldStrategyDaily :: (DataLoader m i o d, MonadIO m) =>
  (IndicatorTime, IndicatorTime) -> StrategyFold m i o d c
foldStrategyDaily timerange dataLoader loaderIn strategy initAcc f = -- TODO: execute initializer, then pass (Right o) to foldStrategyOccasionally
  foldStrategyOccasionally (fromInteger $ 60*60*24) timerange dataLoader loaderIn strategy initAcc f

--
-- Config format
--

data DataLoaderQueryConfig = DataLoaderQueryConfig {
  queries :: [DataLoaderQuery]
} deriving (Show, Eq, Ord, Generic)

instance ToJSON DataLoaderQueryConfig
instance FromJSON DataLoaderQueryConfig

data DataLoaderQuery = DataLoaderQuery {
  indicator :: Text,
  arguments :: HM.HashMap Text Any,
  schedule  :: (IndicatorTime, IndicatorTime)
} deriving (Show, Eq, Ord, Hashable, Generic)

instance ToJSON DataLoaderQuery
instance FromJSON DataLoaderQuery

--
-- Actually executing the data-loader process
--

withQueryConfig :: DataLoaderQueryConfig -> (FilePath -> IO a) -> IO a
withQueryConfig config f = do
  (path, handle) <- openTempFile "/tmp" "stonks_hs-simulator_Strategy-hs_query-config.json"
  let jsonBytes = A.encode config
  BL.hPutStr handle jsonBytes
  hFlush handle
  y <- f path
  hClose handle
  -- TODO: Delete tmp file?
  pure y

withSingleQueryConfig :: Text -> IndicatorArgs -> (FilePath -> IO a) -> IO a
withSingleQueryConfig indicatorID (IndicatorArgs schedule args) f = do
  (path, handle) <- openTempFile "/tmp" "stonks_hs-simulator_Strategy-hs_query-config.json"
  let jsonBytes = A.encode $ DataLoaderQueryConfig [DataLoaderQuery indicatorID args schedule]
  BL.hPutStr handle jsonBytes
  hFlush handle
  y <- f path
  hClose handle
  -- TODO: Delete tmp file?
  pure y

executeDataLoader :: FilePath -> IO [IndicatorSample]
executeDataLoader configPath = do
  -- Spawn data loader process
  let cmd = ShellCommand $ "java -jar ../kt-data-loader/build/libs/kt-data-loader.jar --query-config-file \"" ++ configPath ++ "\" --log-level NONE"
  --putStrLn $ show cmd
  proc@(mhIn, mhOut, mhErr, pHandle) <- createProcess $ CreateProcess cmd
    Nothing Nothing Inherit CreatePipe CreatePipe
    False False False False False False Nothing Nothing False
  let stdOutHandler = \hOut -> foldProcessOutput pHandle hOut DL.empty $ \acc output ->
        let moutVal = A.decode . BL.fromChunks . return $ output
        in case moutVal of
          Nothing     -> trace ("Couldn't parse: '" <> T.decodeUtf8 output <> "'") acc
          Just outVal -> acc <> DL.fromList outVal
  samples <- DL.toList <$> maybe (pure mempty) stdOutHandler mhOut
  -- Wait for process to complete
  exitCode <- waitForProcess pHandle
  -- Print out errors
  --TODO: maybe (pure ()) (\hErr -> T.putStrLn =<< T.hGetContents hErr) mhErr
  -- Pull output, parse each line as JSON
  --output <- maybe (pure "") BL.hGetContents mhOut
  --let Just ret = A.decode output
  -- Cleanup process
  cleanupProcess proc
  --
  pure samples

foldProcessOutput :: ProcessHandle -> Handle -> a -> (a -> B.ByteString -> a) -> IO a
foldProcessOutput ph h initAcc f = work initAcc
  where
    work !acc = do
      -- Read any outstanding input
      bs <- B.hGetNonBlocking h (64 * 1024)

      -- Continue fold, if input isn't empty
      let acc' = if B.length bs == 0
            then acc
            else f acc bs

      -- Check on the process
      s <- getProcessExitCode ph

      -- Exit or loop
      case s of
        Nothing -> work acc'
        Just ec -> do
          -- Get any last bit written between the read and the status check
          last <- B.hGetContents h
          return (f acc' last)
