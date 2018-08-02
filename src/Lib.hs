module Lib
    ( someFunc,
      Person(..),
      toPerson
    ) where

import XMLParser
import PostgresUpdater
import Types
import Control.Monad (unless)

pathToXML = "data/update-file.xml"
pathToCSV = "data/XMLUpdate.csv"

someFunc :: IO ()
someFunc = do
        conn <- connectToDb
        putStrLn "Has XML file already been parse to CSV file? (y/(n))"
        answer <- getLine
        unless (answer == "y") $ parseXMLToCSV pathToXML pathToCSV conn


        tmpDbName <- createTmpDB conn

        debug <- putStrLn "Tempo"
        print debug
        copyCSV2Datebase conn pathToCSV tmpDbName

        putStrLn "DEBUG: Copying succesfull; press enter" >> getLine 
        updates <- upsertDatabase conn tmpDbName

        putStrLn $ "Updates done: " ++ show updates
