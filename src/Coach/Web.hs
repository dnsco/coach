module Coach.Web where

import           Coach
import           Data.Hourglass (Date)

dqsResponse :: Show e => Date -> Either e PeopleData -> String
dqsResponse t (Right p) = showDqs $ delinquents t p
dqsResponse _ (Left e)  = "FAILBOAT: " ++ show e

showDqs :: Delinquents -> String
showDqs []  = "EVERYBODY'S KOSHER"
showDqs dqs = "Have these bitches stepped up? " ++ show dqs
