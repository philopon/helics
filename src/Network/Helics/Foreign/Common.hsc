{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Network.Helics.Foreign.Common where

#include <newrelic_common.h>

import Foreign.C
import Foreign.Ptr
import Network.Helics.Internal.Types

#{enum ReturnCode, ReturnCode
 , returnCodeOk                    = NEWRELIC_RETURN_CODE_OK
 , returnCodeOther                 = NEWRELIC_RETURN_CODE_OTHER
 , returnCodeDisabled              = NEWRELIC_RETURN_CODE_DISABLED
 , returnCodeInvalidParam          = NEWRELIC_RETURN_CODE_INVALID_PARAM
 , returnCodeInvalidId             = NEWRELIC_RETURN_CODE_INVALID_ID
 , returnCodeTransactionNotStarted = NEWRELIC_RETURN_CODE_TRANSACTION_NOT_STARTED
 , returnCodeTransactionInProgress = NEWRELIC_RETURN_CODE_TRANSACTION_IN_PROGRESS
 , returnCodeTransactionNotNamed   = NEWRELIC_RETURN_CODE_TRANSACTION_NOT_NAMED
 }

foreign import ccall "&newrelic_basic_literal_replacement_obfuscator" newrelic_basic_literal_replacement_obfuscator
    :: FunPtr (CString -> IO CString)
