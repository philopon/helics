{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Control.Concurrent

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Helics
import Network.Helics.Wai

import qualified Data.ByteString.Char8 as S8

main :: IO ()
main = do
    key:_ <- getArgs
    withHelics def { licenseKey = S8.pack key, appName = "Test3" } $ do
        _ <- forkIO $ sampler 20
        run 3000 $ helics app

app :: Application
app req send = case pathInfo req of
    []      -> threadDelay (10^(5::Int))     >> send (responseLBS status200 [] "root")
    ["foo"] -> threadDelay (2 * 10^(5::Int)) >> send (responseLBS status200 [] "foo")
    ["bar"] -> datastoreSegment autoScope (DatastoreSegment "bar" "select" "SELECT * FROM bar WHERE key =  'baz'" "select bars" Nothing) (threadDelay (3 * 10^(5::Int)) >> send (responseLBS status200 [] "bar")) (transactionId req)
    _       -> send (responseLBS status404 [] "not found")
