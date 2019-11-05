module SendMessage where

import           Coach.Messaging (getTwilioEnv, sendMessage)

main :: IO ()
main = sendMessage getTwilioEnv "WAT" >>= print
