{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Helics.Foreign.Client where

#include <newrelic_collector_client.h>

import Foreign.C
import Foreign.Ptr
import Network.Helics.Types

#{enum StatusCode, StatusCode
 , statusShutdown = NEWRELIC_STATUS_CODE_SHUTDOWN
 , statusStarting = NEWRELIC_STATUS_CODE_STARTING
 , statusStopping = NEWRELIC_STATUS_CODE_STOPPING
 , statusStarted  = NEWRELIC_STATUS_CODE_STARTED
 }

foreign import ccall "&newrelic_message_handler" newrelic_message_handler :: FunPtr (Ptr rawMessage -> IO ())

foreign import ccall "wrapper" makeStatusCallback :: StatusCallback -> IO (FunPtr StatusCallback)
foreign import ccall newrelic_register_status_callback :: FunPtr StatusCallback -> IO ()

foreign import ccall newrelic_init :: CString -> CString -> CString -> CString -> IO CInt

foreign import ccall newrelic_request_shutdown :: CString -> IO CInt
