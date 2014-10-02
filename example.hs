import System.Environment
import Network.NewRelic

main :: IO ()
main = do
    key:_ <- getArgs
    withNewRelic def { newRelicLicenseKey = key } $ \_ ->
        recordMetric "Custom/TestMetric/A" 12.42
