module Network.Helics.Wai.Safe
    ( HelicsMiddlewareConfig(..)
    -- * middleware
    , helics
    , dummyHelics
    -- * getter
    , transactionId
    , lookupTransactionId
    -- * reexports
    , def
    ) where

import Network.Wai
import Network.Helics
import Network.Helics.Internal.Types (TransactionId(..))

import Data.IORef
import Data.Default.Class
import Data.Vault.Lazy as V
import qualified Data.ByteString as S

newtype HelicsMiddlewareConfig = HelicsMiddlewareConfig
    { transactionName :: Request -> S.ByteString
    }

instance Default HelicsMiddlewareConfig where
    def = HelicsMiddlewareConfig rawPathInfo

-- | helics middleware.
helics :: Key TransactionId -> HelicsMiddlewareConfig -> Middleware
helics key conf app req send =
    withTransaction (transactionName conf req) def $ \tid -> do
   setRequestUrl (rawPathInfo req) tid
   app req { vault = insert key tid (vault req) } send

-- | Middleware which add dummy TransactionId to Request. since v0.4.0.
dummyHelics :: Key TransactionId -> Middleware
dummyHelics key app req send = do
    err <- newIORef Nothing
    app req { vault = insert key (TransactionId 0 err) (vault req) } send

-- | get TransactionId from request.
transactionId :: Key TransactionId -> Request -> TransactionId
transactionId key req = maybe (error "helics middleware is not installed.") id $
    lookupTransactionId key req

-- | get TransactionId when middleware installed.
lookupTransactionId :: Key TransactionId -> Request -> Maybe TransactionId
lookupTransactionId key req = V.lookup key (vault req)
