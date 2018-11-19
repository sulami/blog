--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.List            (isSuffixOf)
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          (mappend)
import           Hakyll
import           System.FilePath      (replaceExtension, takeBaseName,
                                       takeDirectory, (</>))
import qualified System.Process       as Process
import qualified Text.Pandoc          as Pandoc
import           Text.Pandoc.SideNote (usingSideNotes)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "raw/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "keybase.txt" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "tufte/et-book/*/*" $ do
        route $ customRoute $ drop 6 . toFilePath
        compile copyFileCompiler

    match "tufte/tufte.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "css/*" $
      compile compressCssCompiler

    create ["stylesheet.css"] $ do
        route idRoute
        compile $ do
            tufte <- load "tufte/tufte.css"
            csses <- loadAll "css/*.css"
            makeItem $ unlines $ map itemBody $ tufte : csses

    match "pages/*" $ do
        route   $ niceRoute
        compile $ pandocWithSidenotes
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route   $ niceRoute
        compile $ pandocWithSidenotes
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive"] $ do
        route   $ niceRoute
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
                >>= cleanIndexUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            all_posts <- loadAll "posts/*"
            let post_count = length all_posts
            let word_count = sum $ map (length . words . itemBody) all_posts
            posts <- take 5 <$> recentFirst all_posts
            let indexCtx =
                    constField "word_count" (show word_count) `mappend`
                    constField "post_count" (show post_count) `mappend`
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "templates/*" $ compile templateCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = foldr mappend postCtx [bodyField "description",
                                                 escapedTitle]
            posts <- take 10 <$> (recentFirst =<< loadAllSnapshots "posts/*" "content")
            renderAtom atomFeedConfiguration feedCtx posts

    match "pages/cv.md" $ version "pdf" $ do
        route   $ setExtension ".pdf"
        compile $ getResourceBody
            >>= readPandoc
            >>= (return . fmap writeXeTex)
            >>= loadAndApplyTemplate "templates/cv.tex" defaultContext
            >>= xelatex

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where createIndexRoute ident = directory </> pageName </> "index.html"
          where p = toFilePath ident
                directory = takeDirectory p
                bn = takeBaseName p
                pageName = if "posts" `isSuffixOf` directory then drop 11 bn else bn

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls clean)
    where
        idx = "index.html"
        clean url
            | idx `isSuffixOf` url = take (length url - length idx) url
            | otherwise            = url

atomFeedConfiguration :: FeedConfiguration
atomFeedConfiguration = FeedConfiguration
  { feedTitle = "sulami's blog"
  , feedDescription = "a blog"
  , feedAuthorName = "Robin Schroer"
  , feedAuthorEmail = "sulami@peerwire.org"
  , feedRoot = "https://sulami.github.io"
  }

escapedTitle :: Context String
escapedTitle = field "title" $ \i -> do
  value <- getMetadataField (itemIdentifier i) "title"
  return . escapeHtml $ fromMaybe "Post Title" value

pandocWithSidenotes :: Compiler (Item String)
pandocWithSidenotes = let ropts = defaultHakyllReaderOptions
                          wopts = defaultHakyllWriterOptions
                      in pandocCompilerWithTransform ropts wopts usingSideNotes

xelatex :: Item String -> Compiler (Item TmpFile)
xelatex item = do
    TmpFile texPath <- newTmpFile "xelatex.tex"
    let tmpDir  = takeDirectory texPath
        pdfPath = replaceExtension texPath "pdf"

    unsafeCompiler $ do
        writeFile texPath $ itemBody item
        _ <- Process.system $ unwords ["xelatex", "--halt-on-error",
            "-output-directory", tmpDir, texPath]
        return ()

    makeItem $ TmpFile pdfPath

writeXeTex :: Pandoc.Pandoc -> String
writeXeTex = Pandoc.writeLaTeX Pandoc.def {Pandoc.writerTeXLigatures = False}
