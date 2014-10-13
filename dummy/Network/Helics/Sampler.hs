module Network.Helics.Sampler where

type Callback = Double -> Double -> Int -> IO ()

sampler :: Callback -> Int -> IO ()
sampler _ _ = return ()
