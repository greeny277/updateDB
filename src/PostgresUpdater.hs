{-# LANGUAGE OverloadedStrings #-}

module PostgresUpdater where

import Types
import Database.PostgreSQL.Simple
import Data.ByteString (isInfixOf)
import Data.String
import Control.Exception
import Control.Monad

defaultDBUser = "greeny"
defaultDBPw   = "garfield"
defaultDBName = "greeny"
defaultDBTmpName = "updatePerson"

connectToDB :: String -> String -> String -> IO Connection
connectToDB user pw name = connect defaultConnectInfo { connectUser = user, connectPassword = pw, connectDatabase = name }


updateProcess :: [Person] -> IO ()
updateProcess ps = do
        -- Connect to database
        putStrLn "In order to connect to the psql, please insert the following information:"
        putStr $  "Username (default: " ++ defaultDBUser ++ ")"
        dbUser <- getLine >>= (\l -> if null l then return defaultDBUser else return l)
        putStr $  "Password (default: " ++ defaultDBPw ++ ")"
        dbPw <- getLine >>= (\l -> if null l then return defaultDBPw else return l)
        putStr $  "Databasename (default: " ++ defaultDBName ++ ")"
        dbName <- getLine >>= (\l -> if null l then return defaultDBUser else return l)


        conn <- connectToDB dbUser dbPw dbName

        -- Create a table for saving new data temporalily
        tmpDBName <- createTmpDB conn

        -- Insert persons in table
        insertPersons ps tmpDBName conn

        -- Delete the created table
        --execute_ conn "drop table updatePerson"
        close conn

insertPersons :: [Person] -> String -> Connection -> IO ()
insertPersons ps db conn =
        void $ executeMany conn (fromString ("insert into " ++ db ++ " (fname,lname,dob,phone) values (?,?,?,?)")) (map (\p -> (fname p, lname p, dob p, tel p)) ps)

createTmpDB :: Connection -> IO String
createTmpDB conn = do
        putStr $  "Temporarily databasename (default: " ++ defaultDBTmpName ++ ")"
        dbTmpName <- getLine >>= (\l -> if null l then return defaultDBTmpName else return l)
        (execute_ conn (fromString ("create table " ++ dbTmpName ++ " (fname character varying, lname character varying, dob date, phone character(10), UNIQUE (fname, lname, dob))")) >> return dbTmpName) `catch` sqlErrHandler
        where
                  sqlErrHandler e@(SqlError _ _ msg _ _) = if "already exists" `isInfixOf` msg
                                                             then do
                                                                 putStrLn "This tablename already exists; please insert another one."
                                                                 createTmpDB conn
                                                             else throwIO e

upsertDatabase :: IO ()
upsertDatabase = do
        let upsertCmd = "INSERT INTO person\
                         \SELECT fname, lname, dob, phone\
                         \FROM updateperson\
                         \ON CONFLICT (fname,lname, dob)\
                         \DO UPDATE SET phone = excluded.phone;"
        return ()
--
--
--
--sqlErrHandler = Handler (\(SqlError e) -> print e)
