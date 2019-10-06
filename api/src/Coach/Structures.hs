module Coach.Structures where

import           Data.Hourglass.Types (Date)
import           Data.Map.Internal    (Map)
import           Data.Text            (Text)
import           Text.Parsec          (ParseError)

type CSVResult = Either ParseError PeopleData

type Delinquents = [(Person, [ActivityName])]

type PeopleData = Map Person [Activity]

type Activity = (ActivityName, [Event])

type Event = (Date, Text)

type ActivityName = Text

type Person = Text
