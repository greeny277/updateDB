{-|
Module      : Main
Description : Update a database of persons with XML file
Copyright   : (c) Christian Bay, 2018
License     : GPL-3
Maintainer  : christian.bay@posteo.net
Stability   : experimental
Portability : POSIX

This module provides a way to merge a set of data provided as XML file to a database. It consists of two parts:
PostgresUpdater handles the database connection and executes queries. XMLParser on the hand, parses the XML file using
the conduit library and saves the results in a csv file, because a csv file can easily be copied via COPY into a temporary
database.

The whole process is supported by asking for user inputs along the way.
-}
module Main where

{- | Impoprt own modules  -}
import XMLParser
import PostgresUpdater

import Control.Monad (unless)
import System.FilePath (makeRelative)
import System.Posix.Directory (getWorkingDirectory)

{- | Default paths and names -}
defaultPathToXML = "data/update-file.xml"
defaultPathToCSV = "data/XMLUpdate.csv"
defaultDBPerson  = "person"

main :: IO ()
main = do
        putStrLn "This program assumes that the table containing all persons is already loaded to the postegresql sever; if this is not the case please abort and do so. Otherwise press any button." >> getLine

        -- | Ask for name of database containing all persons
        putStrLn $  "Please insert name of database containing all persons (default: " ++ defaultDBPerson ++ "):"
        dbPerson <- getLine >>= (\l -> if null l then return defaultDBPerson else return l)

        -- | Connect to psql server
        conn <- connectToDb

        abs <- getWorkingDirectory
        putStrLn $ "Please insert relative/absolute path to the existing XML file containing updates (default: " ++ defaultPathToXML ++ ")"
        pathToXML <- getLine >>= (\s -> if null s then return defaultPathToXML else return $ makeRelative abs s)
        putStrLn $ "Please insert relative/absolute path to CSV file that will be created (default: " ++ defaultPathToCSV ++ ")"
        pathToCSV <- getLine >>= (\s -> if null s then return defaultPathToCSV else return $ makeRelative abs s)


        -- | Ask if the XML file already has been parsed; do so otherwise
        putStrLn "Has XML file already been parse to CSV file? (y/(n))"
        answer <- getLine
        unless (answer == "y") $
            parseXMLToCSV pathToXML pathToCSV conn


        -- | Create a temporary table and use the person table as
        -- a blueprint
        tmpDbName <- createTmpDB conn dbPerson


        -- | Copy CSV file to temporary table
        -- TODO: Ask user if this is necesssary
        putStrLn "Start copying process from CSV file to temporary table."
        copyCSV2Datebase conn pathToCSV tmpDbName
        putStrLn "Copying has been successful!"

        -- | Start the upsert process
        putStrLn "Start the upsert process:"
        updates <- upsertDatabase conn tmpDbName dbPerson
        putStrLn "Update process has been finished. "

        putStrLn $ "Rows updated: " ++ show updates
