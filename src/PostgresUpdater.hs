{-# LANGUAGE OverloadedStrings #-}

module PostgresUpdater where

import Types
import Database.PostgreSQL.Simple
import Data.ByteString (isInfixOf)
import Data.String
import Control.Exception
import Control.Monad
import Data.Time.Calendar

defaultDBUser = "greeny"
defaultDBPw   = "garfield"
defaultDBName = "greeny"
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
        connect defaultConnectInfo { connectUser = dbUser, connectPassword = dbPw, connectDatabase = dbName }

dropDB :: String -> Connection -> IO ()
dropDB dbName conn =
        void $ execute_ conn (fromString ("drop table " ++ dbName))

closeConn :: Connection -> IO ()
closeConn = close

insertPersons :: String -> Connection -> [Person] -> IO ()
insertPersons db conn ps =
        void $ executeMany conn (fromString ("insert into " ++ db ++ " (fname,lname,dob,phone) values (?,?,?,?)")) (map (\p -> (fname p, lname p, dob p, tel p)) ps)


insertPerson :: String -> Connection -> Person -> IO ()
insertPerson db conn p =
        void $ execute conn (fromString ("insert into " ++ db ++ " (fname,lname,dob,phone) values (?,?,?,?)")) (fname p, lname p, dob p, tel p)

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

upsertDatabase :: String -> Connection -> IO Int
upsertDatabase tmpDB conn = do
        let upsertCmd = fromString ("INSERT INTO person SELECT fname, lname, dob, phone FROM " ++ tmpDB ++ "ON CONFLICT (fname,lname, dob) DO UPDATE SET phone = excluded.phone;")
        fmap fromIntegral (execute_ conn upsertCmd)


upsertPerson :: Connection -> Person -> IO ()
upsertPerson conn p = do
        let upsertCmd = fromString "INSERT INTO person (fname, lname, dob, phone) values (?,?,?,?) ON CONFLICT (fname,lname, dob) DO UPDATE SET phone = excluded.phone;"
        void $ execute conn upsertCmd (fname p, lname p, dob p, tel p)
