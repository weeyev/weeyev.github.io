--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Applicative ((<|>), empty)
import           Data.Aeson                 (FromJSON, parseJSON, withObject, (.:), decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Process             (callCommand)
import           System.Directory           (removeDirectoryRecursive)
import           Control.Exception          (catch, SomeException)
import           Data.List                  (isPrefixOf, tails)
import           Hakyll


siteRoot :: String
siteRoot = "https://weeyev.github.io"


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
    field "snippet" (\item ->
        let snippet = bookmarkSnippet (itemBody item)
        in if null snippet then empty else return snippet) `mappend`
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
    
    catch (removeDirectoryRecursive "_cache") (\(_ :: SomeException) -> return ())
    
    hakyllWith config $ do
        match "images/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "fonts/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "pfp.png" $ do
            route   idRoute
            compile copyFileCompiler

        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler

        match "robots.txt" $ do
            route idRoute
            compile copyFileCompiler

        match "posts/links.html" $ do
            route idRoute
            compile $ do
                let ctx = listField "bookmarks" bookmarkCtx (mapM makeItem bookmarks) `mappend`
                          constField "bookmarkCount" (show $ length bookmarks) `mappend`
                          constField "title" "Bookmarks" `mappend`
                          constField "description" "A curated collection of articles, papers, tools, and ideas from around the web." `mappend`
                          seoCtx
                getResourceBody
                    >>= applyAsTemplate ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls

        match "posts/*.md" $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll (fromGlob "posts/*.md")
                let archiveCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Blog"             `mappend`
                        constField "description" "Essays, experiments, and notes by weeye." `mappend`
                        constField "isBlog" "true"            `mappend`
                        constField "isArchive" "true"         `mappend`
                        seoCtx

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
                        constField "title" "Home" `mappend`
                        constField "description" "The personal website of weeye, featuring essays, experiments, and a curated bookmark archive." `mappend`
                        constField "isHome" "true" `mappend`
                        seoCtx

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        create ["sitemap.xml"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll (fromGlob "posts/*.md")
                let sitemapCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "siteRoot" siteRoot `mappend`
                        defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

        create ["atom.xml"] $ do
            route idRoute
            compile $ do
                posts <- fmap (take 20) . recentFirst =<< loadAllSnapshots (fromGlob "posts/*.md") "content"
                renderAtom feedConfiguration feedCtx posts

        match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = mconcat
    [ dateField "date" "%B %-d, %Y"
    , dateField "dateIso" "%Y-%m-%d"
    , constField "isPost" "true"
    , constField "isBlog" "true"
    , field "categoryLabel" $ \item -> do
        metadata <- getMetadata (itemIdentifier item)
        case lookupString "category" metadata <|> lookupString "type" metadata of
            Just cat -> return cat
            Nothing -> empty
    , field "tags" $ \item -> do
        metadata <- getMetadata (itemIdentifier item)
        case lookupString "tags" metadata of
            Just t -> return t
            Nothing -> empty
    , metadataJsonField "titleJson" "Untitled"
    , metadataJsonField "descriptionJson" "Research notes by weeye."
    , seoCtx
    ]

seoCtx :: Context String
seoCtx = mconcat
    [ field "canonicalUrl" $ \item -> do
        itemRoute <- getRoute (itemIdentifier item)
        return $ siteRoot ++ canonicalPath itemRoute
    , field "ogImage" $ \item -> do
        metadata <- getMetadata (itemIdentifier item)
        let imagePath = maybe "/images/gpu-poor-clean.png" id $
                lookupString "image" metadata <|> firstImageSrc (itemBody item)
        return $ absoluteImageUrl imagePath
    , constField "siteRoot" siteRoot
    , constField "siteName" "weeye"
    , constField "author" "weeye"
    , defaultContext
    ]
  where
    canonicalPath (Just "index.html") = "/"
    canonicalPath (Just path) = '/' : path
    canonicalPath Nothing = "/"

firstImageSrc :: String -> Maybe String
firstImageSrc html = do
    src <- firstMatch "src=\"" html
    let imagePath = takeWhile (/= '"') src
    if null imagePath then Nothing else Just imagePath

firstMatch :: String -> String -> Maybe String
firstMatch needle haystack =
    case [drop (length needle) suffix | suffix <- tails haystack, needle `isPrefixOf` suffix] of
        result : _ -> Just result
        [] -> Nothing

absoluteImageUrl :: String -> String
absoluteImageUrl imagePath
    | "https://" `isPrefixOf` imagePath = imagePath
    | "http://" `isPrefixOf` imagePath = imagePath
    | otherwise = siteRoot ++ "/" ++ dropWhile (== '/') (dropRelativePrefix imagePath)
  where
    dropRelativePrefix path
        | "../" `isPrefixOf` path = dropRelativePrefix (drop 3 path)
        | "./" `isPrefixOf` path = dropRelativePrefix (drop 2 path)
        | otherwise = path

metadataJsonField :: String -> String -> Context String
metadataJsonField fieldName fallback = field fieldName $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    let metadataName = take (length fieldName - 4) fieldName
    return . BSL.unpack . encode $ maybe fallback id (lookupString metadataName metadata)

feedCtx :: Context String
feedCtx = bodyField "description" `mappend` postCtx

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "weeye"
    , feedDescription = "Essays, experiments, and notes by weeye."
    , feedAuthorName  = "weeye"
    , feedAuthorEmail = ""
    , feedRoot        = siteRoot
    }
