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

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    create ["stylesheet.css"] $ do
        route idRoute
        compile $ do
            tufte <- load "tufte/tufte.css"
            csses <- loadAll "css/*.css"
            makeItem $ unlines $ map itemBody $ tufte : csses

    match "content/pages/*" $ do
        route   $ niceRoute
        compile $ pandocWithSidenotes
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "content/posts/*.md" $ do
        route   $ niceRoute
        compile $ pandocWithSidenotes
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "content/posts/*.org" $ do
        route   $ niceRoute
        compile $ getResourceString
          -- >>= withItemBody (unixFilter "emacs" ["--batch",
          --                                       "-l", "ox-hugo/ox-blackfriday.el",
          --                                       "-l", "ox-hugo/ox-hugo.el",
          --                                       "-l", "org-to-md.el",
          --                                       "-f", "org-to-md"])
          -- >>= withItemBody (\s -> return . (++ s) . (++ "\n") . metadatasToStr $ orgMetadatas s)
          >>= (\i -> writePandoc <$> (traverse (return . shiftHeaders 1 . usingSideNotes) =<< readPandoc i))
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls

    create ["archive"] $ do
        route   $ niceRoute
        compile $ do
            posts <- recentFirst =<< loadAll "content/posts/*"
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
            all_posts <- loadAll "content/posts/*"
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
            posts <- take 10 <$> (recentFirst =<< loadAllSnapshots "content/posts/*" "content")
            renderAtom atomFeedConfiguration feedCtx posts

    match "content/pages/cv.md" $ version "pdf" $ do
        route   $ setExtension ".pdf"
        compile $ getResourceBody
            >>= readPandoc
            >>= (return . fmap writeLaTex)
            >>= loadAndApplyTemplate "templates/cv.tex" defaultContext
            >>= latex

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where createIndexRoute ident = directory </> pageName </> "index.html"
          where p = toFilePath ident
                directory = takeDirectory p
                bn = takeBaseName p
                pageName = takeBaseName p
        -- if "posts" `isSuffixOf` directory
        --           then drop 11 bn -- 11 == (len "2006-03-21-")
        --           else bn

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

orgCompiler :: Item String -> Compiler (Item String)
orgCompiler = pure . fmap (\s -> (metadatasToStr . orgMetadatas) s ++ s)

orgMetadatas :: String -> [String]
orgMetadatas = map (format . lower . clean) . takeWhile (/= "") . lines
  where
    clean = concat . splitOn "#+"
    lower s = (map toLower . takeWhile (/= ':')) s ++ dropWhile (/= ':') s
    format xs
        | -- drop weekday str. 2018-05-03 Thu -> 2018-05-03
          "date" `isPrefixOf` xs
        = take 16 . concat . splitOn ">" . concat . splitOn "<" $ xs
        | otherwise
        = xs

metadatasToStr :: [String] -> String
metadatasToStr = ("---\n" ++) . (++ "---\n") . unlines
