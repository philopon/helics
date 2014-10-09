module Network.Helics.Wai (helics, transactionId) where

import Network.Wai
import Data.Vault.Lazy as V
import Network.Helics
import qualified Data.ByteString as S
import System.IO.Unsafe

tidKey :: Key TransactionId
tidKey = unsafePerformIO newKey
{-# NOINLINE tidKey #-}

-- | helics middleware.
helics :: Middleware
helics app req send =
   withTransaction (rawPathInfo req) def $ \tid -> do
   setRequestUrl (rawPathInfo req) tid
   app req { vault = insert tidKey tid (vault req) } send

-- | get TransactionId from request.
transactionId :: Request -> TransactionId
transactionId req = maybe (error "helics middleware is not installed.") id $ V.lookup tidKey (vault req)
