{-# LANGUAGE OverloadedStrings #-}

module XMLParser where

import Types
import Data.Time.Calendar
import Data.Conduit
import Data.Text (Text, unpack)
import Text.XML
import Text.XML.Cursor
import Data.Monoid (mconcat)

parseXML :: FilePath -> IO [Person]
parseXML file = do
        doc <- Text.XML.readFile def file
        let cursor = fromDocument doc
        return $ cursor $// element "member" >=> parsePerson

parsePerson :: Cursor -> [Person]
parsePerson c = do
        let fname = c $/ element "firstname" &/ content
        let lname = c $/ element "lastname" &/ content
        let dob = c $/ element "date-of-birth" &/ content
        let phone = c $/ element "phone" &/ content
        [Person (mconcat fname) (mconcat lname) (read . unpack . mconcat $ dob) (mconcat phone)]
