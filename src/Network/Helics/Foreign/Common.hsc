{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Network.Helics.Foreign.Common where

#include <newrelic_common.h>

import Data.Typeable
import Control.Exception
import Foreign.C
import Foreign.Ptr

newtype ReturnCode = ReturnCode CInt
    deriving (Eq, Typeable)

instance Show ReturnCode where
    show c@(ReturnCode i)
        | c == returnCodeOther                 = "ReturnCode Other(" ++ show (#{const NEWRELIC_RETURN_CODE_OTHER} :: Int) ++ ")"
        | c == returnCodeDisabled              = "ReturnCode Disabled(" ++ show (#{const NEWRELIC_RETURN_CODE_DISABLED} :: Int) ++ ")"
        | c == returnCodeInvalidParam          = "ReturnCode Invalid param(" ++ show (#{const NEWRELIC_RETURN_CODE_INVALID_PARAM} :: Int) ++ ")"
        | c == returnCodeInvalidId             = "ReturnCode Invalid id(" ++ show (#{const NEWRELIC_RETURN_CODE_INVALID_ID} :: Int) ++ ")"
        | c == returnCodeTransactionNotStarted = "ReturnCode Transaction not started(" ++ show (#{const NEWRELIC_RETURN_CODE_TRANSACTION_NOT_STARTED} :: Int) ++ ")"
        | c == returnCodeTransactionInProgress = "ReturnCode Transaction in progress(" ++ show (#{const NEWRELIC_RETURN_CODE_TRANSACTION_IN_PROGRESS} :: Int) ++ ")"
        | c == returnCodeTransactionNotNamed   = "ReturnCode Transaction not named(" ++ show (#{const NEWRELIC_RETURN_CODE_TRANSACTION_NOT_NAMED} :: Int) ++ ")"
        | otherwise                            = "ReturnCode Unknown(" ++ show i ++ ")"


instance Exception ReturnCode

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
