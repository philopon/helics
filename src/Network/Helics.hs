{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Network.Helics
    ( HelicsConfig(..)
    , withHelics
    , sampler
    -- * metric
    , recordMetric
    , recordCpuUsage
    , recordMemoryUsage
    -- * transaction
    , TransactionType(..)
    , TransactionId
    , withTransaction
    , addAttribute
    , setRequestUrl
    , setMaxTraceSegments
    , TransactionError(..)
    , setError
    , noticeError
    , clearError
    -- * segment
    , SegmentId
    , autoScope
    , rootSegment
    , genericSegment
    , Operation(..)
    , DatastoreSegment(..)
    , datastoreSegment
    , externalSegment
    -- * status code
    , StatusCode
    , statusShutdown
    , statusStarting
    , statusStopping
    , statusStarted
    -- * reexports
    , def
    ) where

import System.IO.Error

import Control.Exception
import Control.Monad
import Control.Concurrent

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

import Data.IORef
import Data.Word
import Data.Default.Class
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

import qualified Network.Helics.Sampler as Sampler
import Network.Helics.Foreign.Common
import Network.Helics.Foreign.Client
import Network.Helics.Foreign.Transaction
import Network.Helics.Internal.Types

guardNr :: CInt -> IO ()
guardNr c = unless (c == 0) $ throwIO $ ReturnCode c

initialize :: HelicsConfig -> IO ()
initialize HelicsConfig{..} =
    S.useAsCString licenseKey      $ \key ->
    S.useAsCString appName         $ \app ->
    S.useAsCString language        $ \lng ->
    S.useAsCString languageVersion $ \ver ->
    newrelic_init key app lng ver >>= guardNr

shutdown :: S.ByteString -> IO ()
shutdown reason =
    S.useAsCString reason $ \r ->
    newrelic_request_shutdown r >>= \c ->
    guardNr c

-- | start new relic®  collector client.
-- you must call this function when embed-mode.
withHelics :: HelicsConfig -> IO a -> IO a
withHelics cfg m = bracket bra ket (const m)
  where
    bra = do
        mv <- newEmptyMVar
        cb <- makeStatusCallback (\i -> do
            when (i == 3 || i == 0) $ putMVar mv ()
            maybe (return ()) ($ StatusCode i) $ statusCallback cfg)
        newrelic_register_status_callback cb

        newrelic_register_message_handler newrelic_message_handler

        initialize cfg
        takeMVar mv
        return (freeHaskellFunPtr cb, mv)

    ket (freePtr, mv) = do
        shutdown "withHelics: shutdown"
        takeMVar mv :: IO ()
        freePtr :: IO ()

-- | record custom metric.
recordMetric :: S.ByteString -> Double -> IO ()
recordMetric str d = S.useAsCString str $ \mtr ->
    newrelic_record_metric mtr (realToFrac d) >>= guardNr

-- | sample and send metric of cpu/memory usage.
sampler :: Int -- ^ sampling frequency (sec)
        -> IO ()
sampler s = flip Sampler.sampler (s * 10^(6::Int)) $ \user cpu mem -> do
    recordCpuUsage user cpu
    recordMemoryUsage (fromIntegral mem / (1024 * 1024))

-- | record CPU usage. Normally, you don't need to call this function. use sampler.
recordCpuUsage :: Double -> Double -> IO ()
recordCpuUsage ut p =
    newrelic_record_cpu_usage (realToFrac ut) (realToFrac p) >>= guardNr

-- | record memory usage. Normally, you don't need to call this function. use sampler.
recordMemoryUsage :: Double -> IO ()
recordMemoryUsage mb = 
   newrelic_record_memory_usage (realToFrac mb) >>= guardNr

withTransaction :: S.ByteString -- ^ name of transaction
                -> TransactionType -> (TransactionId -> IO c) -> IO c
withTransaction name typ act = bracket bra ket
    (\tid -> act tid `catch` (\e -> exceptionHandler tid e >> throwIO e))
  where
    bra = do
        tid <- newrelic_transaction_begin
        guardNr =<< S.useAsCString name (newrelic_transaction_set_name tid)
        err <- newIORef Nothing
        return $ TransactionId tid err
    ket DummyTransactionId = return ()
    ket (TransactionId tid er) = do
        case typ of
            Default -> return ()
            Web cat ->
                guardNr =<< S.useAsCString cat (newrelic_transaction_set_category tid)
            Other cat -> do
                guardNr =<< newrelic_transaction_set_type_other tid
                guardNr =<< S.useAsCString cat (newrelic_transaction_set_category tid)
        err <- readIORef er
        case err of
            Nothing -> return ()
            Just TransactionError{..} ->
                S.useAsCString exceptionType $ \ert ->
                S.useAsCString errorMessage  $ \msg ->
                S.useAsCString stackTrace    $ \trc ->
                S.useAsCString stackFrameDelimiter $ \dlm ->
                guardNr =<< newrelic_transaction_notice_error tid ert msg trc dlm
        guardNr =<< newrelic_transaction_end tid

    exceptionHandler DummyTransactionId _ = return ()
    exceptionHandler (TransactionId _ err) se = case fromException se of
        Just e  -> writeIORef err $ Just $ TransactionError
            (S8.pack . show $ ioeGetErrorType e)
            (S8.pack $ ioeGetErrorString e) "" ""
        Nothing -> writeIORef err $ Just $ TransactionError
            (S8.pack $ show se) "" "" ""

guardSid :: CLong -> IO CLong
guardSid sid =
   if sid >= 0
   then return sid
   else throwIO $ ReturnCode (fromIntegral sid)

segment :: CLong -> IO CLong -> IO a -> IO a
segment tid m a = bracket (m >>= guardSid) (newrelic_segment_end tid) (const a)

genericSegment :: SegmentId     -- ^ parent segment id
               -> S.ByteString  -- ^ name of represent segment
               -> IO c          -- ^ action in segment
               -> TransactionId
               -> IO c
genericSegment _ _ m DummyTransactionId = m
genericSegment (SegmentId pid) name act (TransactionId tid _) = segment tid
    (S.useAsCString name $ newrelic_segment_generic_begin tid pid) act

opToBS :: Operation -> S.ByteString
opToBS SELECT = newrelicDatastoreSelect
opToBS INSERT = newrelicDatastoreInsert
opToBS UPDATE = newrelicDatastoreUpdate
opToBS DELETE = newrelicDatastoreDelete

toCObfuscator :: (S.ByteString -> S.ByteString) -> CString -> IO CString
toCObfuscator f i = do
    s <- S.packCString i
    m <- mallocBytes (S.length s + 1)
    pokeByteOff m (S.length s) (0 :: Word8)
    S.useAsCString (f s) (\c -> copyBytes m c (S.length s))
    return m

datastoreSegment :: SegmentId -> DatastoreSegment -> IO a -> TransactionId -> IO a
datastoreSegment _ _ m DummyTransactionId = m
datastoreSegment (SegmentId pid) DatastoreSegment{..} act (TransactionId tid _) = 
    S.useAsCString table              $ \tbl ->
    S.useAsCString (opToBS operation) $ \op ->
    S.useAsCString sql                $ \q  ->
    S.useAsCString sqlTraceRollupName $ \tr -> do
    case sqlObFuscator of
        Nothing -> segment tid
            (newrelic_segment_datastore_begin tid pid tbl op q tr
                newrelic_basic_literal_replacement_obfuscator) act
        Just f ->
            bracket (makeObfuscator $ toCObfuscator f) freeHaskellFunPtr $ \cf ->
            segment tid
                (newrelic_segment_datastore_begin tid pid tbl op q tr cf) act

externalSegment :: SegmentId
                -> S.ByteString -- ^ host of segment
                -> S.ByteString -- ^ name of segment
                -> IO a -> TransactionId -> IO a
externalSegment _ _ _ m DummyTransactionId = m
externalSegment (SegmentId pid) host name act (TransactionId tid _) =
    S.useAsCString host $ \h ->
    S.useAsCString name $ \n ->
    segment tid (newrelic_segment_external_begin tid pid h n) act

addAttribute :: S.ByteString -> S.ByteString -> TransactionId -> IO ()
addAttribute _ _ DummyTransactionId = return ()
addAttribute name value (TransactionId tid _) =
   S.useAsCString name  $ \n ->
   S.useAsCString value $ \v ->
   guardNr =<< newrelic_transaction_add_attribute tid n v

setRequestUrl :: S.ByteString -> TransactionId -> IO ()
setRequestUrl _ DummyTransactionId = return ()
setRequestUrl req (TransactionId tid _) =
   guardNr =<< S.useAsCString req (newrelic_transaction_set_request_url tid)

setMaxTraceSegments :: Int -> TransactionId -> IO ()
setMaxTraceSegments _ DummyTransactionId = return ()
setMaxTraceSegments mx (TransactionId tid _) = guardNr =<<
    newrelic_transaction_set_max_trace_segments tid (fromIntegral mx)

setError :: Maybe TransactionError -> TransactionId -> IO ()
setError _ DummyTransactionId = return ()
setError e (TransactionId _ err) = writeIORef err e

noticeError :: TransactionError -> TransactionId -> IO ()
noticeError = setError . Just

clearError :: TransactionId -> IO ()
clearError = setError Nothing
