import Control.Concurrent
import System.Environment
import Network.NewRelic

main :: IO ()
main = do
    key:_ <- getArgs
    withNewRelic def { newRelicLicenseKey = key } $ \root -> do
        transaction "Test" "test_transaction" root $ do
            threadDelay (10 ^ (6::Int))
