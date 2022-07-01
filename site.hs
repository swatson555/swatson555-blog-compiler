{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile)
import Data.Monoid (mappend)
import Data.Functor.Identity (runIdentity)
import Control.Monad
import Hakyll
import Text.Pandoc
import Text.Pandoc.UTF8 (readFile)

feedConfig = FeedConfiguration
    { feedTitle       = "Steven's Blog"
    , feedDescription = "This is the homepage and blog of Steven Watson."
    , feedAuthorName  = "Steven Watson"
    , feedAuthorEmail = "66756748+swatson555@users.noreply.github.com"
    , feedRoot        = "https://swatson555.github.io/"
    }


config = defaultConfiguration
    { deployCommand = "`pwd`/deploy"
    }


loadPostWriter = do
    template <- readFile "./templates/toc.html"
    let compliedTemplate  = either error id . runIdentity . compileTemplate "" $ template

    return defaultHakyllWriterOptions
        { writerTableOfContents = True
        , writerTOCDepth        = 1
        , writerTemplate        = Just compliedTemplate
        }


postCtx =
    dateField "date" "%B %d, %Y"     `mappend`
    dateField "datetime" "%Y-%m-%d"  `mappend`
    defaultContext


main = do
    postWriter <- loadPostWriter

    hakyllWith config $ do
        match "templates/*" $ compile templateBodyCompiler

        match "favicon.ico" $ do
            route   idRoute
            compile copyFileCompiler

        match "img/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler

        match "posts/**" $ do
            route $ setExtension "html"
            compile $ pandocCompilerWith defaultHakyllReaderOptions postWriter
                >>= loadAndApplyTemplate "templates/article.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

        create ["rss.xml"] $ do
            route idRoute
            compile $ do
                posts <- fmap (take 10) . recentFirst =<< loadAll "posts/**"

                renderRss feedConfig
                          (defaultContext `mappend` constField "description" "")
                          posts

        pagination <- buildPaginateWith
          (sortRecentFirst >=> return . paginateEvery 6) "posts/**"
          (\pageNum -> if pageNum == 1 then "index.html" else fromCapture "*.html" (show pageNum))

        paginateRules pagination $ \pageNum patt -> do
          route idRoute
          compile $ do
            posts <- recentFirst =<< loadAll patt
            let ctx =
                  listField "posts" postCtx (return posts)
                  <> constField "title" "Steven's Journal"
                  <> paginateContext pagination pageNum
                  <> defaultContext
            makeItem ""
              >>= loadAndApplyTemplate "templates/index.html" ctx
              >>= loadAndApplyTemplate "templates/default.html" ctx
              >>= relativizeUrls
