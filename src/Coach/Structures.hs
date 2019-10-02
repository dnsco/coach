module Coach.Structures where

import           Data.Hourglass.Types (Date)
import           Data.Map.Internal    (Map)

type PeopleData = Map Person [Activity]

type Activity = (ActivityName, [Event])

type Event = (Date, String)

type ActivityName = String

type Person = String
