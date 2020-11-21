--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Char            (toLower)
import           Data.List            (isSuffixOf, isPrefixOf)
import           Data.List.Split      (splitOn)
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          (mappend)
import           Hakyll
import           System.FilePath      (replaceExtension, takeBaseName,
                                       takeDirectory, (</>))
import qualified System.Process       as Process
import qualified Text.Pandoc          as Pandoc
import           Text.Pandoc.SideNote (usingSideNotes)
import           Text.Pandoc.Walk     (walk)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "content/raw/*" $ do
        route $ customRoute $ drop 8 . toFilePath
        compile copyFileCompiler

    match "keybase.txt" $ do
        route   idRoute
        compile copyFileCompiler

    match "content/images/*" $ do
        route $ customRoute $ drop 8 . toFilePath
        compile copyFileCompiler

    match "tufte/et-book/*/*" $ do
        route $ customRoute $ drop 6 . toFilePath
        compile copyFileCompiler

    match "tufte/tufte.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    create ["stylesheet.css"] $ do
        route idRoute
        compile $ do
            tufte <- load "tufte/tufte.css"
            csses <- loadAll "css/*.css"
            makeItem $ unlines $ map itemBody $ tufte : csses

    match "content/pages/*.org" $ do
        route   $ niceRoute
        compile $ pandocWithShiftHeaders
            >>= loadAndApplyTemplate "templates/default.html" dateCtx
            >>= relativizeUrls

    match "content/posts/*.org" $ do
        route   $ niceRoute
        compile $ pandocWithShiftHeaders
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/default.html" dateCtx
          >>= relativizeUrls

    create ["posts"] $ do
        route   $ niceRoute
        compile $ do
            posts <- recentFirst =<< loadAll "content/posts/*"
            let archiveCtx =
                    listField "posts" dateCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    baseCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "content/index.html" $ do
        route $ constRoute "index.html"
        compile $ do
            all_posts <- loadAll "content/posts/*"
            let post_count = length all_posts
            let word_count = sum $ map (length . words . itemBody) all_posts
            posts <- take 5 <$> recentFirst all_posts
            let indexCtx =
                    constField "word_count" (show word_count) `mappend`
                    constField "post_count" (show post_count) `mappend`
                    listField "posts" dateCtx (return posts) `mappend`
                    baseCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "templates/*" $ compile templateCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = foldr mappend dateCtx [bodyField "description",
                                                 cleanUrlField "url",
                                                 escapedTitle]
            posts <- take 10 <$> (recentFirst =<< loadAllSnapshots "content/posts/*" "content")
            renderAtom atomFeedConfiguration feedCtx posts

    match "content/pages/cv.org" $ version "pdf" $ do
        route $ constRoute "pages/robin-schroer-cv.pdf"
        compile $ getResourceBody
            >>= readPandoc
            >>= (return . fmap writeLaTex)
            >>= loadAndApplyTemplate "templates/cv.tex" baseCtx
            >>= latex

--------------------------------------------------------------------------------

dateCtx :: Context String
dateCtx = dateField "date" "%B %e, %Y" `mappend` baseCtx

stripTags :: Context String
stripTags = functionField "stripTags" $ \args item -> case args of
  [s] -> return $ Hakyll.stripTags s
  _   -> error "stripTags only takes one argument"

cleanUrlField :: String -> Context a
cleanUrlField key = field key $ \i -> do
    let id = itemIdentifier i
        empty' = fail $ "No route url found for item " ++ show id
    fmap (maybe empty' (cleanUrl . toUrl)) $ getRoute id

baseCtx :: Context String
baseCtx = Main.stripTags `mappend` defaultContext

niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where createIndexRoute ident = directory </> pageName </> "index.html"
          where p = toFilePath ident
                directory = if "content" `isPrefixOf` dir
                             then drop 8 dir -- 8 == (len "content/")
                             else dir
                dir = takeDirectory p
                bn = takeBaseName p
                pageName = bn

cleanUrl :: String -> String
cleanUrl url | idx `isSuffixOf` url = take (length url - length idx) url
             | otherwise            = url
  where idx = "index.html"

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanUrl)

atomFeedConfiguration :: FeedConfiguration
atomFeedConfiguration = FeedConfiguration
  { feedTitle = "sulami's blog"
  , feedDescription = "a blog"
  , feedAuthorName = "Robin Schroer"
  , feedAuthorEmail = "blog@peerwire.org"
  , feedRoot = "https://blog.sulami.xyz"
  }

escapedTitle :: Context String
escapedTitle = field "title" $ \i -> do
  value <- getMetadataField (itemIdentifier i) "title"
  return . escapeHtml $ fromMaybe "Post Title" value

pandocWithShiftHeaders :: Compiler (Item String)
pandocWithShiftHeaders = let ropts = defaultHakyllReaderOptions
                             wopts = defaultHakyllWriterOptions
                         in pandocCompilerWithTransform ropts wopts (usingSideNotes . shiftHeaders 1)

latex :: Item String -> Compiler (Item TmpFile)
latex item = do
  TmpFile texPath <- newTmpFile "latex.tex"
  let tmpDir  = takeDirectory texPath
      pdfPath = replaceExtension texPath "pdf"

  unsafeCompiler $ do
    writeFile texPath $ itemBody item
    _ <- Process.system $ unwords ["pdflatex", "--halt-on-error",
                                   "-output-directory", tmpDir, texPath]
    return ()

  makeItem $ TmpFile pdfPath

-- full set of Pandoc options is abailable here:
-- https://www.stackage.org/haddock/lts-9.12/pandoc-1.19.2.4/Text-Pandoc-Options.html#t:WriterOptions
writeLaTex :: Pandoc.Pandoc -> String
writeLaTex = Pandoc.writeLaTeX Pandoc.def {Pandoc.writerTeXLigatures = False}

shiftHeaders :: Int -> Pandoc.Pandoc -> Pandoc.Pandoc
shiftHeaders i p = walk go p
  where
    go (Pandoc.Header l a inl) = Pandoc.Header (l+i) a inl
    go x = x
