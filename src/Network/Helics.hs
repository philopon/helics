{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Helics
    (NewRelicConfig ( newRelicLicenseKey
                    , newRelicAppName
                    , newRelicLanguage
                    , newRelicLanguageVersion
                    , newRelicStatusCallback
                    ), def
    , withNewRelic
    , recordMetric
    , recordCpuUsage
    , recordMemoryUsage
    , transaction
    , rootSegment
    ) where

import Control.Exception
import Control.Monad
import Control.Concurrent

import Foreign.Ptr
import Foreign.C

import Data.Default.Class

import Network.Helics.Foreign.Common
import Network.Helics.Foreign.Client
import Network.Helics.Foreign.Transaction

data NewRelicConfig = NewRelicConfig
    { newRelicLicenseKey      :: String
    , newRelicAppName         :: String
    , newRelicLanguage        :: String
    , newRelicLanguageVersion :: String
    , newRelicStatusCallback  :: Maybe (NewRelicStatusCode -> IO ())
    }

instance Default NewRelicConfig where
    def = NewRelicConfig
        (error "license key is not set.")
        "App"
        "Haskell"
        "7.8.3" -- TOOL_VERSION_ghc
        Nothing

initNewRelic :: NewRelicConfig -> IO NewRelicSegmentId
initNewRelic NewRelicConfig{..} =
    withCString newRelicLicenseKey      $ \key ->
    withCString newRelicAppName         $ \app ->
    withCString newRelicLanguage        $ \lng ->
    withCString newRelicLanguageVersion $ \ver ->
    newrelic_init key app lng ver >>= \r ->
    if r >= 0
        then return . NewRelicSegmentId $ fromIntegral r
        else throwIO $ NewRelicReturnCode r

shutdownNewRelic :: String -> IO ()
shutdownNewRelic reason =
    withCString reason $ \r ->
    newrelic_request_shutdown r >>= \c ->
    if c == 0
        then return ()
        else throwIO $ NewRelicReturnCode c

withNewRelic :: NewRelicConfig -> IO a -> IO a
withNewRelic cfg m = bracket bra ket (const m)
  where
    bra = do
        mv <- newEmptyMVar
        cb <- makeStatusCallback (\i -> do
            when (i == 3 || i == 0) $ putMVar mv ()
            maybe (return ()) ($ NewRelicStatusCode i) $ newRelicStatusCallback cfg)
        newrelic_register_status_callback cb

        newrelic_register_message_handler newrelic_message_handler

        _ <- initNewRelic cfg
        takeMVar mv
        return (freeHaskellFunPtr cb, mv)

    ket (freePtr, mv) = do
        shutdownNewRelic "withNewRelic: shutdown"
        takeMVar mv
        freePtr :: IO ()

recordMetric :: String -> Double -> IO ()
recordMetric str d = withCString str $ \mtr ->
    newrelic_record_metric mtr (realToFrac d) >>= \r ->
    unless (r == 0) $ throwIO (NewRelicReturnCode r)

recordCpuUsage :: Double -> Double -> IO ()
recordCpuUsage ut p = do
    r <- newrelic_record_cpu_usage (realToFrac ut) (realToFrac p)
    unless (r == 0) $ throwIO (NewRelicReturnCode r)

recordMemoryUsage :: Double -> IO ()
recordMemoryUsage mb = do
    r <- newrelic_record_memory_usage (realToFrac mb)
    unless (r == 0) $ throwIO (NewRelicReturnCode r)

transaction :: String -> String -> NewRelicSegmentId -> IO a -> IO a
transaction name desc (NewRelicSegmentId pid) m = do
    tid <- newrelic_transaction_begin

    r <- withCString name $ newrelic_transaction_set_name tid 
    if r == 0 then return () else throwIO $ NewRelicReturnCode r

    sid <- withCString desc $ newrelic_segment_generic_begin tid pid
    a <- m
    s <- newrelic_segment_end tid sid
    if s == 0 then return () else throwIO $ NewRelicReturnCode s
    newrelic_transaction_set_type_other tid
    e <- newrelic_transaction_end tid
    if e == 0 then return () else throwIO $ NewRelicReturnCode e
    return a
