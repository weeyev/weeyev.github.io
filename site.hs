--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<|>), empty)
import           Data.Aeson                 (FromJSON, parseJSON, withObject, (.:), (.:?), decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Process             (callCommand)
import           Data.List (isInfixOf)
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

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

bookmarkCtx :: Context Bookmark
bookmarkCtx = 
    field "title" (return . bookmarkTitle . itemBody) `mappend`
    field "url" (return . bookmarkUrl . itemBody) `mappend`
    field "date" (return . bookmarkDate . itemBody) `mappend`
    field "domain" (return . bookmarkDomain . itemBody) `mappend`
    field "snippet" (return . bookmarkSnippet . itemBody) `mappend`
    field "toRead" (\item -> if bookmarkToRead (itemBody item) then return "true" else empty) `mappend`
    field "signalColor" (return . bookmarkSignalColor . itemBody)

loadBookmarks :: IO [Bookmark]
loadBookmarks = do
    jsonContent <- BSL.readFile "data/bookmarks.json"
    let bookmarks = decode jsonContent :: Maybe [Bookmark]
    case bookmarks of
        Just b -> return b
        Nothing -> return []

main :: IO ()
main = do
    callCommand "python3 scripts/update_bookmarks.py"
    bookmarks <- loadBookmarks
    
    -- Force recompile check
    hakyllWith config $ do
        match "images/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler

        match "projects.md" $ do
            route   $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

        match "posts/links.html" $ do
            route idRoute
            compile $ do
                let ctx = listField "bookmarks" bookmarkCtx (mapM makeItem bookmarks) `mappend`
                          constField "bookmarkCount" (show $ length bookmarks) `mappend`
                          constField "title" "Bookmarks" `mappend`
                          defaultContext
                getResourceBody
                    >>= applyAsTemplate ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls

        match (fromGlob "posts/*" .&&. complement "posts/links.html") $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll (fromGlob "posts/*.md")
                let archiveCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Posts"            `mappend`
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls

        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll (fromGlob "posts/*.md")
                let indexCtx =
                        listField "posts" postCtx (return $ take 10 posts) `mappend`
                        listField "homeBookmarks" bookmarkCtx (mapM makeItem $ take 10 bookmarks) `mappend`
                        constField "title" "Home"                `mappend`
                        defaultContext

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = mconcat
    [ dateField "date" "%B %-d, %Y"
    , field "categoryLabel" $ \item -> do
        metadata <- getMetadata (itemIdentifier item)
        case lookupString "category" metadata <|> lookupString "type" metadata of
            Just cat -> return cat
            Nothing -> empty
    , defaultContext
    ]

