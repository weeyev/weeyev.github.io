{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson (FromJSON, parseJSON, withObject, (.:), (.:?), decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromMaybe)

data Bookmark = Bookmark
    { bookmarkTitle       :: String
    , bookmarkUrl         :: String
    , bookmarkDate        :: String
    , bookmarkDomain      :: String
    , bookmarkSnippet     :: String
    , bookmarkToRead      :: Bool
    , bookmarkSignalColor :: String
    } deriving (Show)

instance FromJSON Bookmark where
    parseJSON = withObject "Bookmark" $ \v -> Bookmark
        <$> v .: "title"
        <*> v .: "url"
        <*> v .: "date"
        <*> v .: "domain"
        <*> v .: "snippet"
        <*> v .: "to_read"
        <*> v .: "signal_color"

main :: IO ()
main = do
    jsonContent <- BSL.readFile "data/bookmarks.json"
    let bookmarks = decode jsonContent :: Maybe [Bookmark]
    case bookmarks of
        Just b -> do
            putStrLn $ "Successfully parsed: " ++ show (length b) ++ " bookmarks"
            mapM_ (putStrLn . ("  - " ++) . bookmarkTitle) (take 5 b)
        Nothing -> putStrLn "Failed to parse bookmarks"
