{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Network.NewRelic
    (NewRelicConfig ( newRelicLicenseKey
                    , newRelicAppName
                    , newRelicLanguage
                    , newRelicLanguageVersion
                    , newRelicStatusCallback
                    ), def
    , withNewRelic
    , recordMetric
    , transaction
    ) where

import Control.Exception

import Foreign.Ptr
import Foreign.C

import Data.Default.Class

import Network.NewRelic.Foreign.Common
import Network.NewRelic.Foreign.Client
import Network.NewRelic.Foreign.Transaction

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
        TOOL_VERSION_ghc
        Nothing

newtype NewRelicSegmentId = NewRelicSegmentId CLong

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

withNewRelic :: NewRelicConfig -> (NewRelicSegmentId -> IO a) -> IO a
withNewRelic cfg m = bracket bra ket (m . snd)
  where
    bra = do
        freePtr <- case newRelicStatusCallback cfg of
            Nothing -> return (return ())
            Just f  -> do
                cb <- makeStatusCallback (f . NewRelicStatusCode . fromIntegral)
                newrelic_register_status_callback cb
                return (freeHaskellFunPtr cb)

        newrelic_register_message_handler newrelic_message_handler

        seg <- initNewRelic cfg
        return (freePtr, seg)

    ket (freePtr, _) = do
        freePtr :: IO ()
        shutdownNewRelic "withNewRelic: end"

recordMetric :: String -> Double -> IO ()
recordMetric str d = withCString str $ \mtr ->
    newrelic_record_metric mtr (realToFrac d) >>= \r ->
    if r == 0
    then return ()
    else throwIO $ NewRelicReturnCode r

transaction :: String -> String -> NewRelicSegmentId -> IO a -> IO a
transaction name desc (NewRelicSegmentId pid) m = do
    tid <- newrelic_transaction_begin
    r <- withCString name $ newrelic_transaction_set_name tid 
    if r == 0 then return () else throwIO $ NewRelicReturnCode r

    sid <- withCString desc $ newrelic_segment_generic_begin tid pid
    a <- m
    s <- newrelic_segment_end tid sid
    if s == 0 then return () else throwIO $ NewRelicReturnCode s
    return a
