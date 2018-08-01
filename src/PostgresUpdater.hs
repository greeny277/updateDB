{-# LANGUAGE OverloadedStrings #-}

module PostgresUpdater where

import Lib (Person(..))
import Database.PostgreSQL.Simple

connectToDB :: IO Connection
connectToDB = connect defaultConnectInfo { connectUser = "greeny", connectPassword = "garfield", connectDatabase = "greeny" }


updateProcess :: [Person] -> IO ()
updateProcess ps = do
        conn <- connectToDB

        -- Create a table for saving new data temporalily
        execute_ conn "create table updatePerson (fname character varying, lname character varying, dob date, phone character(10), UNIQUE (fname, lname,dob))"

        -- Insert persons in table
        insertPersons ps conn
        -- Delete the created table
        --execute_ conn "drop table updatePerson"
        close conn

insertPersons ps conn =
        executeMany conn "insert into updatePerson (fname,lname,dob,phone) values (?,?,?,?)" (map (\p -> (fname p, lname p, dob p, tel p)) ps)
