module Coach.Structures where

import           Data.Hourglass.Types (Date)
import           Data.Map.Strict      (Map)
import           Data.Text            (Text)
import           Text.Parsec          (ParseError)

type CSVResult = Either ParseError PeopleData

type NewCSVResult = Either ParseError [[String]]

type Delinquents = Map Person [DActivity]

type DActivity = (ActivityName, Bool, [Event])

--data ActivityStatus = CurrentActivity | DelinquentActivity
type PeopleData = Map Person [Activity]

type Activity = (ActivityName, [Event])

type Event = (Date, Text)

type ActivityName = Text

type Person = Text
