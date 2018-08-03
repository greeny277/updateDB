{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : XMLParser
Description : A module that parses entries of a XML file and saves them into a csv file
Copyright   : (c) Christian Bay, 2018
License     : GPL-3
Maintainer  : christian.bay@posteo.net
Stability   : experimental
Portability : POSIX

This module reads from the XML that contains the persons that need to be updated.
The parsing process is done with the streaming library `conduit`. The librarie allows
to deal with huge datastreams in a pleasent way.
-}
module XMLParser where

import PostgresUpdater (upsertPerson)
import Types (Person (..))


import Conduit
import Control.Monad (void)

import Data.Monoid (mconcat)
import qualified Data.Text as T (Text, intercalate, snoc, pack, unpack)
import Data.Time.Calendar (Day)
import Data.Text.Encoding (encodeUtf8)
import Data.XML.Types (Event)
import Database.PostgreSQL.Simple (Connection)

import Text.Read (readMaybe)
import Text.XML.Stream.Parse

{- Read in a file as a stream and
-  parse the bytestrings to xml events -}
fileInput :: (MonadIO m, MonadResource m, MonadThrow m) =>
          String -> -- ^ source file
          ConduitT () Event m () -- ^ Stream of events
fileInput f = sourceFile f .| parseBytes def

{- Parse the downstream xml events to persons and send them upstream
-}
parseMember:: MonadThrow m => ConduitT Event Person m (Maybe Person)
parseMember= do
        p <- tagNoAttr "member" $ do
            fname <- force "firstname tag missing" $ tagNoAttr "firstname" content
            lname <- force "lastname tag missing tag missing" $ tagNoAttr "lastname" content
            dob   <- force "date-of-birth tag missing" $ tagNoAttr "date-of-birth" content
            phone <- force "phone tag missing" $ tagNoAttr "phone" content
            let dobMay = (readMaybe :: String -> Maybe Day) (T.unpack dob)
            return $ Person fname lname dobMay phone
        case p of
            Just person -> do
                            yield person
                            return p
            Nothing -> return Nothing

{- Parse each person in tag `members`
-}
parseMembers :: MonadThrow m => ConduitT Event Person m ()
parseMembers = void $ force "no members tag" $
    tagNoAttr "members" $ many parseMember

{-
- Parse the whole XML files and write the parsed persons to a CSV file
-}
parseXMLToCSV :: FilePath   -- ^ Path to xml file
              -> FilePath   -- ^ Path to csv file
              -> Connection -- ^ Connection to psql server
              -> IO ()      -- ^ IO Action
parseXMLToCSV xmlFile csvFile conn =
        runConduitRes $ parseFile def xmlFile .| parseMembers .| mapC (\p -> T.intercalate ";" [fname p, lname p, dobHelper (dob p), tel p `T.snoc`  '\n']) .| mapC encodeUtf8 .| sinkFile csvFile
        where dobHelper :: Maybe Day -> T.Text
              dobHelper Nothing = "\\null" -- "\null" represents the NULL value in the csv file
              dobHelper (Just d) = T.pack (show d)
