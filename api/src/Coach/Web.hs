module Coach.Web where

import           Data.Hourglass   (DateTime)

import           Coach.Parsing
import           Coach.Structures

dqsResponse :: Show e => DateTime -> Either e PeopleData -> String
dqsResponse n (Right p) = showDqs $ delinquents n p
dqsResponse _ (Left e)  = "FAILBOAT: " ++ show e

showDqs :: Delinquents -> String
showDqs [] = "EVERYBODY'S KOSHER"
showDqs dqs =
  "Have these bitches stepped up? " ++
  show dqs ++
  "<br/> <br/>" ++
  "<a href='https://docs.google.com/spreadsheets/d/1nbqOF_xE_ANFzA-pxnDVVG2iH45E6khYVHrc36l5Opo/edit#gid=0'>" ++
  "GET IT!</a>"
