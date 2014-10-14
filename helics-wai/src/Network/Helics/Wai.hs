module Network.Helics.Wai
    ( Safe.HelicsMiddlewareConfig(..)
    -- * middleware
    , helics
    -- * getter
    , transactionId
    , lookupTransactionId
    -- * reexports
    , def
    ) where


import Network.Wai
import Network.Helics
import qualified Network.Helics.Wai.Safe as Safe
import qualified Data.Vault.Lazy as V
import System.IO.Unsafe

tidKey :: V.Key TransactionId
tidKey = unsafePerformIO V.newKey
{-# NOINLINE tidKey #-}

-- | helics middleware.
helics :: Safe.HelicsMiddlewareConfig -> Middleware
helics = Safe.helics tidKey

-- | get TransactionId from request.
transactionId :: Request -> TransactionId
transactionId = Safe.transactionId tidKey

lookupTransactionId :: Request -> Maybe TransactionId
lookupTransactionId = Safe.lookupTransactionId tidKey
