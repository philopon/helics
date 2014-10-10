module Network.Helics.Wai (helics, transactionId) where

import Network.Wai
import Network.Helics
import qualified Network.Helics.Wai.Safe as Safe
import qualified Data.Vault.Lazy as V
import System.IO.Unsafe

tidKey :: V.Key TransactionId
tidKey = unsafePerformIO V.newKey
{-# NOINLINE tidKey #-}

-- | helics middleware.
helics :: Middleware
helics = Safe.helics tidKey

-- | get TransactionId from request.
transactionId :: Request -> TransactionId
transactionId = Safe.transactionId tidKey
