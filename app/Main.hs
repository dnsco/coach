import           Coach              (runApi)
import           System.Environment

main :: IO ()
main = do
  port <- getEnv "PORT"
  runApi $ read port
