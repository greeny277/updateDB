module Lib
    ( someFunc,
      Person(..),
      toPerson
    ) where

import XMLParser (parseXML)
import PostgresUpdater
import Types

pathToXML = "data/update-file.xml"

someFunc :: IO ()
someFunc = do
        persons <- parseXML pathToXML
        updateProcess persons
