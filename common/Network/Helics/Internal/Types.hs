{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Network.Helics.Internal.Types where

import Control.Exception

import Foreign.C.Types
import Foreign.C.String

import Data.IORef
import Data.Typeable
import Data.Default.Class
import qualified Data.ByteString as S

-- common
newtype ReturnCode = ReturnCode CInt deriving (Eq, Typeable, Show)
instance Exception ReturnCode

-- client
type StatusCallback = CInt -> IO ()
newtype StatusCode = StatusCode CInt deriving (Eq, Show)

-- transaction
data TransactionError = TransactionError
    { exceptionType       :: S.ByteString
    , errorMessage        :: S.ByteString
    , stackTrace          :: S.ByteString
    , stackFrameDelimiter :: S.ByteString
    }

data TransactionId
    = TransactionId
        { rawTransactionId :: CLong
        , transactionError :: IORef (Maybe TransactionError)
        }
    | DummyTransactionId

newtype SegmentId = SegmentId CLong
type SqlObfuscator = CString -> IO CString

-- main
data HelicsConfig = HelicsConfig
    { licenseKey      :: S.ByteString
    , appName         :: S.ByteString
    , language        :: S.ByteString
    , languageVersion :: S.ByteString
    , statusCallback  :: Maybe (StatusCode -> IO ())
    }

instance Default HelicsConfig where
    def = HelicsConfig
        (error "license key is not set.")
        "App"
        "Haskell"
        COMPILER_VERSION
        Nothing

data TransactionType
    = Default
    | Web   S.ByteString
    | Other S.ByteString

instance Default TransactionType where
    def = Default

data Operation
    = SELECT
    | INSERT
    | UPDATE
    | DELETE
    deriving (Show)

data DatastoreSegment = DatastoreSegment
    { table              :: S.ByteString
    , operation          :: Operation
    , sql                :: S.ByteString
    , sqlTraceRollupName :: S.ByteString
    , sqlObFuscator      :: Maybe (S.ByteString -> S.ByteString)
    }
instance Default DatastoreSegment where
    def = DatastoreSegment "unknown" SELECT "" "unknown" Nothing


