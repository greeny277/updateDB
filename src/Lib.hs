module Lib
    ( someFunc,
      Person(..),
      toPerson
    ) where

import XMLParser
import PostgresUpdater
import Types

pathToXML = "data/update-file.xml"

someFunc :: IO ()
someFunc = connectToDb >>= parseAndFillDB pathToXML
