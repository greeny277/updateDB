{-# LANGUAGE OverloadedStrings #-}

module XMLParser where

import Types
import Data.Time.Calendar
import Data.Conduit
import Conduit
import qualified Data.Text as T
import Control.Monad (void)
import Text.XML
--import Text.XML.Cursor
import Data.Monoid (mconcat)
import Data.XML.Types (Event)
import Text.Read (readMaybe)
import Text.XML.Stream.Parse
import qualified Data.ByteString as B
import Database.PostgreSQL.Simple (Connection)
import Data.Text.Encoding (encodeUtf8)
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
            let dobMay = (readMaybe :: String -> Maybe Day) (T.unpack dob)
            return $ Person fname lname dobMay phone
        case p of
            Just person -> do
                            yield person
                            return p
            Nothing -> return Nothing

parseMembers :: MonadThrow m => ConduitT Event Person m ()
parseMembers = void $ force "no members tag" $
    tagNoAttr "members" $ many parseMember

parseXMLToCSV :: FilePath -> FilePath -> Connection -> IO ()
parseXMLToCSV xmlFile csvFile conn =
        runConduitRes $ parseFile def xmlFile .| parseMembers .| mapC (\p -> T.intercalate ";" [fname p, lname p, dobHelper (dob p), tel p `T.snoc`  '\n']) .| mapC encodeUtf8 .| sinkFile csvFile
        where dobHelper :: Maybe Day -> T.Text
              dobHelper Nothing = "\\null"
              dobHelper (Just d) = T.pack (show d)
