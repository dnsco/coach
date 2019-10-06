import           Coach              (runApi)
import           Coach.Servant      (buildTsFiles)
import           System.Environment

main :: IO ()
main = do
  buildTsFiles
  port <- getEnv "PORT"
  runApi $ read port
