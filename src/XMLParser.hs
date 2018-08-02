{-# LANGUAGE OverloadedStrings #-}

module XMLParser where

import Types
import Data.Time.Calendar
import Data.Conduit
import Conduit
import Data.Text (Text, unpack)
import Control.Monad (void)
import Text.XML
--import Text.XML.Cursor
import Data.Monoid (mconcat)
import Data.XML.Types (Event)
import Text.Read (readMaybe)
import Text.XML.Stream.Parse
import qualified Data.ByteString as B
import Database.PostgreSQL.Simple (Connection)

import PostgresUpdater (upsertPerson)

{-| The 'parseXML' function reads the input of a XML file that consists of a table
  describing persons. For each entry the values are extracted and parsed as a 'Person'
-}
--parseXML :: FilePath -> IO [Person]
--parseXML file = do
--        doc <- Text.XML.readFile def file
--        let cursor = fromDocument doc
--        return $ cursor $// element "member" >=> parsePerson

--parsePerson :: Cursor -> [Person]
--parsePerson c = do
--        let fname = c $/ element "firstname" &/ content
--        let lname = c $/ element "lastname" &/ content
--        let dob = c $/ element "date-of-birth" &/ content
--        let phone = c $/ element "phone" &/ content
--        [Person (mconcat fname) (mconcat lname) (read . unpack . mconcat $ dob) (mconcat phone)]
--
-- | Read in file as a stream
fileInput :: (MonadIO m, MonadResource m, MonadThrow m) => String -> ConduitT () Event m ()
fileInput f = sourceFile f .| parseBytes def


parseMember:: MonadThrow m => ConduitT Event Person m (Maybe Person)
parseMember= do
        p <- tagNoAttr "member" $ do
            fname <- force "firstname tag missing" $ tagNoAttr "firstname" content
            lname <- force "lastname tag missing tag missing" $ tagNoAttr "lastname" content
            dob   <- force "date-of-birth tag missing" $ tagNoAttr "date-of-birth" content
            phone <- force "phone tag missing" $ tagNoAttr "phone" content
            let dobMay = (readMaybe :: String -> Maybe Day) (unpack dob)
            return $ Person fname lname dobMay phone
        case p of
            Just person -> do
                            yield person
                            return p
            Nothing -> return Nothing

parseMembers :: MonadThrow m => ConduitT Event Person m ()
parseMembers = void $ force "no members tag" $
    tagNoAttr "members" $ many parseMember

parseAndFillDB :: FilePath -> Connection -> IO ()
parseAndFillDB file conn =
        runConduitRes $ parseFile def file .| parseMembers .| mapM_C (liftIO . upsertPerson conn)
