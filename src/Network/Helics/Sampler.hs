module Network.Helics.Sampler where

import System.Posix.Process

import Foreign.C.Types

import GHC.Conc

import Control.Applicative

import Network.Helics.Foreign.System

import Data.Time.Clock

type Callback = Double -> Double -> Int -> IO ()

sampler :: Callback -> Int -> IO ()
sampler callback sleep = do
    t     <- fromIntegral <$> clockTick
    core  <- fromIntegral <$> getNumCapabilities
    cTime <- getCurrentTime
    uTime <- fromIntegral <$> getUserTime
    pSize <- fromIntegral <$> pageSize
    pid   <- getProcessID
    threadDelay sleep
    go t core pSize pid cTime uTime
  where

    unCClock (CClock c) = c
    getUserTime = unCClock . userTime <$> getProcessTimes

    go tick core pSize pid = loop
      where
        loop cTime uTime = do
            cTime' <- getCurrentTime
            uTime' <- fromIntegral <$> getUserTime
            pages  <- getPages pid
            let real = realToFrac $ diffUTCTime cTime' cTime
                user = (uTime' - uTime) / tick
                cpu  = user / (real * core)
                mem  = pages * pSize
            callback user cpu mem
            threadDelay sleep
            loop cTime' uTime'
