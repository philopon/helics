{-# LANGUAGE ForeignFunctionInterface #-}

module Network.NewRelic.Foreign.Client where

#include <newrelic_collector_client.h>

import Foreign.C
import Foreign.Ptr

newtype NewRelicStatusCode = NewRelicStatusCode CInt

#{enum NewRelicStatusCode, NewRelicStatusCode
 , shutdown = NEWRELIC_STATUS_CODE_SHUTDOWN
 , starting = NEWRELIC_STATUS_CODE_STARTING
 , stopping = NEWRELIC_STATUS_CODE_STOPPING
 , started  = NEWRELIC_STATUS_CODE_STARTED
 }

foreign import ccall "&newrelic_message_handler" newrelic_message_handler :: FunPtr (Ptr rawMessage -> IO ())

type StatusCallback = CInt -> IO ()
foreign import ccall "wrapper" makeStatusCallback :: StatusCallback -> IO (FunPtr StatusCallback)
foreign import ccall newrelic_register_status_callback :: FunPtr StatusCallback -> IO ()

foreign import ccall newrelic_init :: CString -> CString -> CString -> CString -> IO CInt

foreign import ccall newrelic_request_shutdown :: CString -> IO CInt
