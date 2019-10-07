import           Coach                    (coachApi)
import           Network.Wai.Handler.Warp
import           System.Environment

main :: IO ()
main = do
  port <- getEnv "PORT"
  run (read port) coachApi
