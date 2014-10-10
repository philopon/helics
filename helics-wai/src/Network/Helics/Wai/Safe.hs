module Network.Helics.Wai.Safe (helics, transactionId) where

import Network.Wai
import Network.Helics
import Data.Vault.Lazy as V

-- | helics middleware.
helics :: Key TransactionId -> Middleware
helics key app req send =
   withTransaction (rawPathInfo req) def $ \tid -> do
   setRequestUrl (rawPathInfo req) tid
   app req { vault = insert key tid (vault req) } send

-- | get TransactionId from request.
transactionId :: Key TransactionId -> Request -> TransactionId
transactionId key req = maybe (error "helics middleware is not installed.") id $ V.lookup key (vault req)
