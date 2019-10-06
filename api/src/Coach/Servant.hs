{-# LANGUAGE DeriveAnyClass #-}
module Coach.Servant where

import           Coach.Network            (parseCsvAt)
import           Coach.Parsing            (delinquents)
import qualified Coach.Structures         as Coach
import           Control.Monad.Except
import           Data.Aeson
import           Data.Hourglass
import           Data.String.Conversions
import           Data.Text

import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

import           System.Environment       (getEnv)
import           System.Hourglass

import           Servant.Foreign     (Foreign, GenerateList, HasForeign,
                                      HasForeignType, Req, listFromAPI, typeFor,
                                      _reqReturnType)
import           ServantTS
import ServantTS.Output.Docs
import ServantTS.Output.RequestFlavors.Fetch (Fetch)
import Typescript
--import Data.Time.Calendar
--import Data.ByteString          (ByteString)
--import Network.HTTP.Media       ((//), (/:))

import ServantTS.Output.TSFunctions (defaultReqToTSFunction)--import Servant.Types.SourceT    (source)
type PeopleApi = Get '[ PlainText, JSON] [Person]

data Person =
  Person
    { name                 :: Text
    , delinquentActivities :: [Text]
    }
  deriving (Eq, Show, Generic, TypescriptType)

instance ToJSON Person

instance MimeRender PlainText [Person] where
  mimeRender Proxy [] = "Y'all are phenomenal!"
  mimeRender Proxy ps =
    convertString . Data.Text.unlines $
    pack("These folks might gotta setup up their game: ") : (render <$> ps)

render :: Person -> Text
render p = name p  -- ++ delinquentActivities p

peopleFromDs :: Coach.Delinquents -> [Person]
peopleFromDs ds = uncurry Person <$> ds

people :: [Person]
people = [Person "Bob" ["GOURDS"]]

server1 :: Server PeopleApi
server1 = do
  sheetUrl <- liftIO $ getEnv "SHEET_URL"
  currentDate <- liftIO (localTimeUnwrap <$> localDateCurrent)
  ps <- liftIO $ parseCsvAt sheetUrl
  case ps of
    Right ps' -> return . peopleFromDs $ delinquents currentDate ps'
    Left _    -> throwError err503 {errBody = "Couldn't parse CSV."}

peopleApi :: Proxy PeopleApi
peopleApi = Proxy

apiApp :: Application
apiApp = serve peopleApi server1

runApi :: Int -> IO ()
runApi port = run port apiApp


buildTsFiles :: IO ()
buildTsFiles =
  apiToTSDocs asTS reqToTSFunction outputFileLocs
  where
    outputFileLocs = OutputFileNames "ts_generated/types.tsx" "ts_generated/api.tsx"
    asTS = servantToReqTS (Proxy :: Proxy FpTs) (Proxy :: Proxy PeopleApi)
    reqToTSFunction = defaultReqToTSFunction (Proxy @Fetch)
