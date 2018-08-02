module Types where

import Data.Time.Calendar
import Data.Text (Text, unpack)

data Person = Person {
            fname :: Text,
            lname :: Text,
            dob   :: Day,
            tel   :: Text
            } deriving (Show)

toPerson :: Text -> Text -> Text -> Text -> Person
toPerson f l d = Person f l (read (unpack d))
