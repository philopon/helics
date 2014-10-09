{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Helics.Foreign.System
    ( clockTick
    , pageSize
    , getPages
    ) where

#include <unistd.h>
#include <sys/times.h>

import System.Posix.Types
import System.Posix.IO.ByteString

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Text.Show.ByteString as L

import qualified Data.ByteString.Unsafe as U

foreign import ccall sysconf :: CInt -> IO CLong

clockTick :: IO CLong
clockTick = sysconf #const _SC_CLK_TCK

pageSize :: IO CLong
pageSize = sysconf #const _SC_PAGESIZE

bufSize :: Int
bufSize = 1024

getPages :: CPid -> IO Int
getPages (CPid pid) = do
    let name = S8.concat ["/proc/", L.toStrict $ L.show pid, "/statm"]
    fd <- openFd name ReadOnly Nothing defaultFileFlags
    allocaBytes bufSize $ \ptr -> do
        CSize l <- fdReadBuf fd ptr $ fromIntegral bufSize
        bs      <- U.unsafePackCStringLen (castPtr ptr, fromIntegral l)
        return . maybe 0 fst . S8.readInt . S8.tail . snd $ S8.break (== ' ') bs
