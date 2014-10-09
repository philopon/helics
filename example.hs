{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Concurrent
import System.Environment
import Network.Helics
import qualified Data.ByteString.Char8 as S
import Control.Exception
import System.IO.Error

main :: IO ()
main = do
    k:_ <- getArgs
    _ <- forkIO $ sampler 60
    withHelics def { licenseKey = S.pack k } $ putStrLn "start" >> loop 0
  where
    loop i = do
        withTransaction "test" def (\tid -> do
            withTransaction "inu" def (const $ threadDelay (10^5))
            genericSegment autoScope "neko" (threadDelay (10^5)) tid
            when (i `mod`  97 == 0) $ ioError $ userError "user error!"
            when (i `mod` 101 == 0) $ throwIO Overflow
            ) `catch` (\e -> print (e::SomeException))
        threadDelay (2 * 10^5)
        loop (succ i)
