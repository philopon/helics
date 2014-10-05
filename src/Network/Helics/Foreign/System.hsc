{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Helics.Foreign.System
    ( userTime
    , clockTick
    ) where

#include <unistd.h>
#include <sys/times.h>

import Foreign
import Foreign.C
import Foreign.Storable

foreign import ccall times :: Ptr a -> IO CClock

userTime :: IO CClock
userTime = allocaBytes (sizeOf (undefined :: CClock) * 4) $ \ptr -> do
    _ <- times ptr
    peek ptr

foreign import ccall sysconf :: CInt -> IO CLong

clockTick :: IO CLong
clockTick = sysconf #const _SC_CLK_TCK
