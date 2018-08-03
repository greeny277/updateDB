{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : PostgresUpdater
Description : A module that interacts with the postgres database
Copyright   : (c) Christian Bay, 2018
License     : GPL-3
Maintainer  : christian.bay@posteo.net
Stability   : experimental
Portability : POSIX

This module is able to build a connection to a postgresql data base and provides the
necesssary queries to fulfill the task.
-}
module PostgresUpdater where

import Types

import Control.Applicative ((<$>))
import Control.Exception
import Control.Monad

import Data.ByteString (isInfixOf)
import Data.String
import Data.Time.Calendar
import Database.PostgreSQL.Simple

import System.FilePath
import System.Posix.Directory

defaultDBUser = "user"
defaultDBHost = "localhost"
defaultDBPw   = "12345"
defaultDBName = "my-db"
defaultDBTmpName = "updatePerson"

connectToDb :: IO Connection
connectToDb = do
        -- Connect to database
        putStrLn "In order to connect to the psql, please insert the following information:"
        putStrLn $  "Username (default: " ++ defaultDBUser ++ "):"
        dbUser <- getLine >>= (\l -> if null l then return defaultDBUser else return l)
        putStrLn $  "Password (default: " ++ defaultDBPw ++ "):"
        dbPw <- getLine >>= (\l -> if null l then return defaultDBPw else return l)
        putStrLn $  "Databasename (default: " ++ defaultDBName ++ "):"
        dbName <- getLine >>= (\l -> if null l then return defaultDBUser else return l)
        putStrLn $  "Hostname (default: " ++ defaultDBHost ++ "):"
        dbHost <- getLine >>= (\l -> if null l then return defaultDBHost else return l)

        connect defaultConnectInfo { connectUser = dbUser, connectPassword = dbPw, connectDatabase = dbName, connectHost = dbHost }

dropDB :: String -> Connection -> IO ()
dropDB dbName conn =
        void $ execute_ conn (fromString ("drop table " ++ dbName))

closeConn :: Connection -> IO ()
closeConn = close

{- | Insert a set of persons to a database -}
insertPersons :: String -> Connection -> [Person] -> IO ()
{-# DEPRECATED insertPersons "Use `copyCSV2Datebase' to create the temporary database" #-}
insertPersons db conn ps =
        void $ executeMany conn (fromString ("insert into " ++ db ++ " (fname,lname,dob,phone) values (?,?,?,?)")) (map (\p -> (fname p, lname p, dob p, tel p)) ps)

{- | Insert a single person to a database -}
insertPerson :: String -> Connection -> Person -> IO ()
{-# DEPRECATED insertPerson "Use `copyCSV2Datebase' to create the temporary database" #-}
insertPerson db conn p =
        void $ execute conn (fromString ("insert into " ++ db ++ " (fname,lname,dob,phone) values (?,?,?,?)")) (fname p, lname p, dob p, tel p)

{- Create a temporary database
-}
createTmpDB :: Connection -- ^ connection to database
            -> String -- ^ The name of the database containing all persons
            -> IO String -- ^ Return name of temporary table
createTmpDB conn dbPerson = do
        putStrLn $  "Temporarily databasename (default: " ++ defaultDBTmpName ++ ")"
        dbTmpName <- getLine >>= (\l -> if null l then return defaultDBTmpName else return l)
        (execute_ conn (fromString ("CREATE TEMP TABLE " ++ dbTmpName ++ " AS SELECT * FROM " ++ dbPerson ++ " LIMIT 0;")) >> return dbTmpName) `catch` sqlErrHandler
        where
                  sqlErrHandler e@(SqlError _ _ msg _ _) = if "already exists" `isInfixOf` msg
                                                             then do
                                                                 putStrLn "This tablename already exists; please insert another one."
                                                                 createTmpDB conn dbPerson
                                                             else throwIO e

{- Copy the content of a csv file to a database via the COPY command
-}
copyCSV2Datebase :: Connection -- ^ Connection to psql
                 -> FilePath   -- ^ relative filepath to csv file
                 -> String     -- ^ Name of destination database
                 -> IO ()
copyCSV2Datebase conn rel dbName = do
        abs <- getWorkingDirectory
        let full = abs </> rel
        putStrLn $ "Start copying from file:" ++ full ++ " into database " ++ dbName
        void $ execute_ conn (fromString ("COPY " ++ dbName ++ " FROM '" ++ full ++ "' WITH (FORMAT csv, DELIMITER ';', NULL '\\null')"))

{- Upsert the content from one database to another. If an entry does not
- exist yet in a database it will be created. Other the SET rule describes
- what will be substituted.
-}
upsertDatabase :: Connection -- ^ Connection to psql
                -> String    -- ^ Name of temporary database containing updated persons
                -> String    -- ^ Name of database containing all persons
                -> IO Int    -- ^ Return rows that have been changed (usually all rows)
upsertDatabase conn tmpDB dbPerson = do
        let upsertCmd = fromString ("INSERT INTO " ++ dbPerson ++ " SELECT fname, lname, dob, phone FROM " ++ tmpDB ++ " ON CONFLICT (fname,lname, dob) DO UPDATE SET phone = excluded.phone;")
        fmap fromIntegral (execute_ conn upsertCmd)


{- Upsert on person into the database containing each person. If an entry does not
- exist yet in a database it will be created. Other the SET rule describes
- what will be substituted. This function is not used at the moment but may
- be useful nevertheless.
-}
upsertPerson :: Connection -- ^ Connection to psq
             -> String     -- ^ Name of database containing all persons
             -> Person     -- ^ A single person
             -> IO Int     -- ^ Return rows that have been changed (usually 1)
upsertPerson conn dbPerson p = do
        let upsertCmd = fromString ("INSERT INTO " ++ dbPerson ++ " (fname, lname, dob, phone) values (?,?,?,?) ON CONFLICT (fname,lname, dob) DO UPDATE SET phone = excluded.phone;")
        fromIntegral <$> execute conn upsertCmd (fname p, lname p, dob p, tel p)

