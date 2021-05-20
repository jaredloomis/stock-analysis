module DataLoader where

import RIO
import Prelude (putStrLn, print)

import Control.Monad.Free (Free(..))
import Data.Maybe (maybeToList, listToMaybe)
import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime)

import GHC.IO.Handle (mkFileHandle)
import System.IO (openTempFile, nativeNewline)
import System.Process
import System.Directory (removeFile)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Hashable (Hashable)
import Control.Monad.State

import qualified Data.DList as DL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as A
import qualified Data.Text as T
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

class Functor f => DataLoader f a where
  {-# MINIMAL (loadSample | loadSamples) #-}
  loadSample  :: a -> Text -> IndicatorArgs -> f (Maybe IndicatorSample)
  loadSample d t  = (listToMaybe <$>) . loadSamples d t
  loadSamples :: a -> Text -> IndicatorArgs -> f [IndicatorSample]
  loadSamples d t = (maybeToList <$>) . loadSample d t

data CachingDataLoader = CachingDataLoader

instance DataLoader (StateT (HM.HashMap DataLoaderQuery [IndicatorSample]) IO) CachingDataLoader where
  loadSamples _ indicatorID iargs@(IndicatorArgs schedule args) = do
    let query = DataLoaderQuery indicatorID args schedule
    cache <- get
    (samples, cache') <- lift $ loadSamplesWithCache cache query
    put cache'
    pure samples

data ScanningDataLoader = ScanningDataLoader

instance DataLoader IO ScanningDataLoader where
  loadSample _ indicatorID iargs@(IndicatorArgs schedule args) = pure Nothing -- XXX Can be tuned to get more accurate pre-compilation

scanStrategy :: StrategyM ()
     -> NominalDiffTime
     -> (IndicatorTime, IndicatorTime)
     -> IO (HashMap DataLoaderQuery [IndicatorSample])
scanStrategy strategy deltaTime timerange =
    let emptyHm = HM.empty :: HM.HashMap DataLoaderQuery [IndicatorSample]
    in trace "====== INIT LOADER ========" $ foldStrategyOccasionally deltaTime timerange ScanningDataLoader strategy emptyHm $ \stmt _ _ acc ->
      let mquery = case stmt of
            FetchSample  indicatorID (IndicatorArgs schedule args) _ -> Just $ DataLoaderQuery indicatorID args schedule
            FetchSamples indicatorID (IndicatorArgs schedule args) _ -> Just $ DataLoaderQuery indicatorID args schedule
            _                                                        -> Nothing
      in trace (T.pack $ "== INIT SAMPLE " ++ show stmt ++ " ==") 12 `seq`
          maybe (pure acc) ((snd <$>) . loadSamplesWithCache acc) mquery

loadSamplesWithCache :: HM.HashMap DataLoaderQuery [IndicatorSample] -> DataLoaderQuery -> IO ([IndicatorSample], HM.HashMap DataLoaderQuery [IndicatorSample])
loadSamplesWithCache cache query =
  let mcachedValue = HM.lookup query cache
      fetchSamples   = do
        samples <- withQueryConfig (DataLoaderQueryConfig [query]) executeDataLoader
        pure (samples, HM.insert query samples cache)
  in maybe fetchSamples (\cachedValue -> trace "FETCHED CACHED VALUE" $ pure (cachedValue, cache)) mcachedValue

data NaiveDataLoader = NaiveDataLoader

instance DataLoader IO NaiveDataLoader where
  loadSamples _ indicatorID args =
    withSingleQueryConfig indicatorID args executeDataLoader

data DummyDataLoader = DummyDataLoader

instance DataLoader IO DummyDataLoader where
  loadSamples _ indicatorID args = pure []

data InitDataLoader o = InitDataLoader o

instance DataLoader IO (InitDataLoader o) where
  loadSamples _ indicatorID args = pure []

--
-- Top-level functions - run a strategy with a data loader
--

executeStrategyPointInTime :: (DataLoader m d, MonadIO m) =>
  IndicatorTime ->
  d -> StrategyM () ->
  m [TradeLog]
executeStrategyPointInTime time dataLoader strategy =
  DL.toList <$> foldStrategyPointInTime time dataLoader strategy DL.empty executeStrategy'
 where
  executeStrategy' (Trade ticker ty count ()) samples utime acc = do
    mprice <- maybe (fetchSampleValue utime ticker) (pure . pure . sampleValue)
      . listToMaybe
      . filter (\sample -> sampleStockID sample == ticker)
      $ samples
    pure $ maybe acc (DL.snoc acc . TradeLog ty ticker count utime) mprice
  executeStrategy' _ _ _ _ = pure DL.empty

  fetchSampleValue utime ticker =
    let sampleArgs = IndicatorArgs (IndicatorTime utime, IndicatorTime utime) $ HM.fromList [("tickers", Any . A.Array . V.fromList $ [A.String ticker])]
    in fmap sampleValue <$> sampleGetter "price" sampleArgs

  sampleGetter  = loadSample dataLoader

executeStrategyOccasionally :: (DataLoader m d, MonadIO m) =>
  NominalDiffTime -> (IndicatorTime, IndicatorTime) ->
  d -> StrategyM () ->
  m [TradeLog]
executeStrategyOccasionally deltaTime (startTime, endTime) dataLoader strategy =
  executeStrategy' startTime
 where
  executeStrategy' time
    | time > endTime = pure []
    | otherwise = do
      samplesThis <- executeStrategyPointInTime time dataLoader strategy
      samplesRest <- executeStrategy' =<< liftIO (addTime time deltaTime)
      pure $ samplesThis ++ samplesRest

executeStrategyDaily :: (DataLoader m d, MonadIO m) =>
  (IndicatorTime, IndicatorTime) ->
  d -> StrategyM () ->
  m [TradeLog]
executeStrategyDaily timerange dataLoader strategy =
  executeStrategyOccasionally (fromInteger $ 60*60*24) timerange dataLoader strategy

--
-- Strategy Folds
--

type StrategyFold m d a =
  d ->
  -- ^ The data loader.
  StrategyM () ->
  -- ^ The strategy to fold on.
  a ->
  -- ^ The fold accumulator's initial value.
  (StrategyF () -> [IndicatorSample] -> UTCTime -> a -> m a) ->
  -- ^ The fold.
  m a

foldStrategyPointInTime :: (DataLoader m d, MonadIO m) => IndicatorTime -> StrategyFold m d a
foldStrategyPointInTime time dataLoader strategy initAcc f =
  foldStrategyPointInTime' initAcc strategy
 where
  foldStrategyPointInTime' acc (Free (FetchSamples indicatorID args next)) = do
    let strippedStmt = FetchSamples indicatorID args (const ())
    samples <- samplesGetter indicatorID args
    utime   <- utcTime
    acc'    <- f strippedStmt samples utime acc
    foldStrategyPointInTime' acc' (next samples)
  foldStrategyPointInTime' acc (Free (FetchSample indicatorID args next)) = do
    let strippedStmt = FetchSample indicatorID args (const ())
    sample <- sampleGetter indicatorID args
    utime  <- utcTime
    acc'   <- f strippedStmt (maybeToList sample) utime acc
    foldStrategyPointInTime' acc' (next sample)
  foldStrategyPointInTime' acc (Free (Trade ticker ty count next)) = do
    let strippedStmt = Trade ticker ty count ()
    let sampleArgs = IndicatorArgs (time, time) $ HM.fromList [("tickers", Any . A.Array . V.fromList $ [A.String ticker])]
    price <- maybe 0 sampleValue <$> sampleGetter "price" sampleArgs
    utime <- utcTime
    acc'  <- f strippedStmt [] utime acc
    foldStrategyPointInTime' acc' next
  foldStrategyPointInTime' acc (Free (GetTime next)) = do
    let strippedStmt = GetTime (const ())
    utime <- utcTime
    acc'  <- f strippedStmt [] utime acc
    foldStrategyPointInTime' acc (next utime)
  foldStrategyPointInTime' acc (Pure _) = pure acc

  utcTime       = liftIO $ toUTC time
  samplesGetter = loadSamples dataLoader
  sampleGetter  = loadSample dataLoader

foldStrategyOccasionally :: (DataLoader m d, MonadIO m) =>
  NominalDiffTime -> (IndicatorTime, IndicatorTime) -> StrategyFold m d a
foldStrategyOccasionally deltaTime (startTime, endTime) dataLoader strategy initAcc f = foldStrategyOccasionally' initAcc startTime
 where
  foldStrategyOccasionally' acc time
    | time > endTime = pure acc
    | otherwise = do
      acc'  <- foldStrategyPointInTime time dataLoader strategy initAcc f
      foldStrategyOccasionally' acc' =<< liftIO (addTime time deltaTime)

foldStrategyDaily :: (DataLoader m d, MonadIO m) =>
  (IndicatorTime, IndicatorTime) -> StrategyFold m d a
foldStrategyDaily timerange dataLoader strategy initAcc f =
  foldStrategyOccasionally (fromInteger $ 60*60*24) timerange dataLoader strategy initAcc f

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

{-
joinQueryConfigMap :: NominalDiffTime -> HM.HashMap DataLoaderQueryConfig [IndicatorSample] -> HM.HashMap DataLoaderQueryConfig [IndicatorSample]
joinQueryConfigMap mergeLimit configMap = HM.foldrWithKey loop HM.empty configMap
 where
  loop config samples acc =
-}


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
  removeFile path
  -- TODO: Delete tmp file?
  pure y

executeDataLoader :: FilePath -> IO [IndicatorSample]
executeDataLoader configPath = do
  -- Spawn data loader process
  let cmd = ShellCommand $ "java -jar ../kt-data-loader/build/libs/kt-data-loader.jar --query-config-file \"" ++ configPath ++ "\" --log-level OFF"
  trace (T.pack . show $ cmd) (return ())
  --putStrLn $ show cmd
  proc@(mhIn, mhOut, mhErr, pHandle) <- createProcess $ CreateProcess cmd
    Nothing Nothing Inherit CreatePipe CreatePipe
    False False False False False False Nothing Nothing False
  let stdOutHandler = \hOut -> foldProcessOutput pHandle hOut DL.empty $ \acc output ->
        let moutVal = A.decode . BL.fromChunks . return $ output
        in case moutVal of
          Nothing     -> if output /= "" then trace ("Couldn't parse: '" <> T.decodeUtf8 output <> "'") acc else acc
          Just outVal -> trace (T.pack $ "Parsed samples: " ++ show outVal) $ acc <> DL.fromList outVal
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

-- | Given a handle, fold over each chunk of output.
foldProcessOutput :: ProcessHandle -> Handle -> a -> (a -> B.ByteString -> a) -> IO a
foldProcessOutput ph h initAcc f = work initAcc
  where
    work !acc = do
      -- Read any outstanding input
      bs <- B.hGetNonBlocking h (64 * 1024)

      -- Update acc, if input isn't empty
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
