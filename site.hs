{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (***), arr)
import Data.Monoid (mempty, mconcat)
import System.FilePath

import Hakyll

main :: IO ()
main = hakyll $ do
    -- Copy downloadable files
    match "files/*" $ do
        route idRoute
        compile copyFileCompiler

    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Render posts
    match "posts/*" $ do
        route   $ customRoute dropDate `composeRoutes` setExtension ".html"
        compile $ pageCompiler
            >>> arr pageTitle
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Index
    match  "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "pageTitle" "Epigramme")
        >>> requireAllA "posts/*" addPostList
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Render RSS feed
    match  "rss.xml" $ route idRoute
    create "rss.xml" $
        requireAll_ "posts/*" >>> renderRss feedConfiguration

    -- Read templates
    match "templates/*" $ compile templateCompiler

pageTitle :: Page a -> Page a
pageTitle = renderField "title" "pageTitle" ("Epigramme: " ++)

prettyDate :: Page a -> Page a
prettyDate = renderDateField "pdate" "%d %b %Y" "unknown"

dropDate :: Identifier a -> FilePath
dropDate ident = let file = toFilePath ident
                 in replaceFileName file (drop 11 $ takeFileName file)

-- Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
        arr (map prettyDate)
        >>> arr (reverse . chronological)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

feedConfiguration = FeedConfiguration
    { feedTitle       = "Fernand Pajot's blog feed"
    , feedDescription = "thoughts about *"
    , feedAuthorName  = "Fernand Pajot"
    , feedRoot        = "http://wwww.epigram.me"
    }
