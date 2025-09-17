{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile)
import Data.Monoid (mappend)
import Data.Functor.Identity (runIdentity)
import Control.Monad
import Hakyll
import Text.Pandoc
import Text.Pandoc.UTF8 (readFile)

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

    hakyllWith defaultConfiguration $ do
        match "templates/*" $ compile templateBodyCompiler

        match "favicon.ico" $ do
            route   idRoute
            compile copyFileCompiler

        match "img/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "js/**" $ route idRoute >> compile copyFileCompiler

        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler

        match "posts/**" $ do
            route $ setExtension "html"
            compile $ pandocCompilerWith defaultHakyllReaderOptions postWriter
                >>= loadAndApplyTemplate "templates/article.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

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
