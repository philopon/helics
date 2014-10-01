{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Network.NewRelic.Foreign.Common where

#include <newrelic_common.h>

import Data.Typeable
import Control.Exception
import Foreign.C
import Foreign.Ptr

newtype NewRelicReturnCode = NewRelicReturnCode CInt
    deriving (Show, Typeable)
instance Exception NewRelicReturnCode

#{enum NewRelicReturnCode, NewRelicReturnCode
 , ok                    = NEWRELIC_RETURN_CODE_OK
 , other                 = NEWRELIC_RETURN_CODE_OTHER
 , disabled              = NEWRELIC_RETURN_CODE_DISABLED
 , invalidParam          = NEWRELIC_RETURN_CODE_INVALID_PARAM
 , invalidId             = NEWRELIC_RETURN_CODE_INVALID_ID
 , transactionNotStarted = NEWRELIC_RETURN_CODE_TRANSACTION_NOT_STARTED
 , transactionInProgress = NEWRELIC_RETURN_CODE_TRANSACTION_IN_PROGRESS
 , transactionNotNamed   = NEWRELIC_RETURN_CODE_TRANSACTION_NOT_NAMED
 }

foreign import ccall newrelic_basic_literal_replacement_obfuscator
    :: CString -> IO CString
