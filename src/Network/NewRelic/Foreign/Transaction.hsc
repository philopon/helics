{-# LANGUAGE ForeignFunctionInterface #-}

module Network.NewRelic.Foreign.Transaction where

import Foreign.C
import Foreign.Ptr

foreign import ccall newrelic_enable_instrumentation :: CInt -> IO ()

foreign import ccall newrelic_register_message_handler :: FunPtr (Ptr rawMessage -> IO a) -> IO ()

foreign import ccall newrelic_record_metric :: CString -> CDouble -> IO CInt

foreign import ccall newrelic_record_cpu_usage :: CDouble -> CDouble -> IO CInt

foreign import ccall newrelic_record_memory_usage :: CDouble -> IO CInt

foreign import ccall newrelic_transaction_begin :: IO CLong

foreign import ccall newrelic_transaction_set_type_web :: CLong -> IO CInt

foreign import ccall newrelic_transaction_set_type_other :: CLong -> IO CInt

foreign import ccall newrelic_transaction_set_category :: CLong -> CString -> IO CInt

foreign import ccall newrelic_transaction_notice_error :: CLong -> CString -> CString -> CString -> CString -> IO CInt

foreign import ccall newrelic_transaction_add_attribute :: CLong -> CString -> CString -> IO CInt

foreign import ccall newrelic_transaction_set_name :: CLong -> CString -> IO CInt

foreign import ccall newrelic_transaction_set_request_url :: CLong -> CString -> IO CInt

foreign import ccall newrelic_transaction_set_max_trace_segments :: CLong -> CInt -> IO CInt

foreign import ccall newrelic_transaction_end :: CLong -> IO CInt

foreign import ccall newrelic_segment_generic_begin :: CLong -> CLong -> CString -> IO CLong

type SqlObfuscator = CString -> IO CString
foreign import ccall newrelic_segment_datastore_begin
    :: CLong -> CLong -> CString -> CString -> CString -> CString -> Ptr SqlObfuscator -> IO CLong

foreign import ccall newrelic_segment_external_begin :: CLong -> CLong -> CString -> CString -> CLong

foreign import ccall newrelic_segment_end :: CLong -> CLong -> IO CInt
