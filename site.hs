--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<|>), empty)
import           Data.List (isInfixOf)
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

main :: IO ()
main = hakyllWith config $ do
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



    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            -- Load the raw markdown content directly
            linksContent <- loadBody (fromFilePath "posts/links.md" :: Identifier)
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "bookmarksPreview" (take5Bookmarks linksContent) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
take5Bookmarks :: String -> String
take5Bookmarks content = 
    let allLines = lines content
        -- Skip frontmatter and find first <li>
        contentLines = dropWhile (not . isListItem) $ dropFrontmatter allLines
        -- Take 5 bookmark items
        bookmarks = takeNItems contentLines 5
    in unlines bookmarks
  where
    dropFrontmatter [] = []
    dropFrontmatter (l:ls)
        | l == "---" = dropFrontmatter' ls
        | otherwise = l:ls
    
    dropFrontmatter' [] = []
    dropFrontmatter' (l:ls)
        | l == "---" = ls
        | otherwise = dropFrontmatter' ls
    
    isListItem l = "<li class=\"post-item\"" `isInfixOf` l
    
    takeNItems _ 0 = []
    takeNItems [] _ = []
    takeNItems (l:ls) n
        | "<li class=\"post-item\"" `isInfixOf` l =
            let (item, rest) = takeOneItem (l:ls)
            in item ++ takeNItems rest (n - 1)
        | otherwise = takeNItems ls n
    
    takeOneItem [] = ([], [])
    takeOneItem (l:ls)
        | "</li>" `isInfixOf` l = ([l], ls)
        | otherwise = 
            let (rest, remaining) = takeOneItem ls
            in (l:rest, remaining)

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

