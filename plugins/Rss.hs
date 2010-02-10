{-# LANGUAGE ViewPatterns, PatternGuards #-}
module Rss (plugin) where

import Data.Maybe
import Data.Ord
import Data.List

import Debug.Trace

import Network.Gitit.Interface
import Network.Browser
import Network.HTTP

import qualified Text.Atom.Feed as Atom
import Text.Feed.Types
import Text.Feed.Import

 
plugin :: Plugin
plugin = mkPageTransformM transformBlock
 
transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (_, classes, namevals) contents)
  | "rss" `elem` classes , Just url <- lookup "url" namevals = do
    feed_data <- liftIO $ browse $ do
        setAllowRedirects True
        fmap (rspBody . snd) $ request $ getRequest url
    
    --liftIO $ print $ parseFeedString feed_data
    
    return $ case parseFeedString feed_data of
      Just (AtomFeed f) ->
        BulletList [ [Para [Link [title] (Atom.linkHref link, fromMaybe (show title) (Atom.linkTitle link))]]
                   | Atom.Entry { Atom.entryTitle = (textContentToInline -> title), Atom.entryLinks = links } <- Atom.feedEntries f
                   , trace (show $ [link | link@(Atom.Link { Atom.linkType = Just "text/html" }) <- links]) True
                   , (link:_) <- [sortBy (comparing (maybe True (\t -> "omment" `isInfixOf` t) . Atom.linkTitle))
                                         [link | link@(Atom.Link { Atom.linkType = Just "text/html" }) <- links]]
                   ]
      _ -> Para [Emph [Str "Error parsing feed from", Str url]]
transformBlock x = return x

textContentToInline (Atom.TextString s) = Str s
textContentToInline (Atom.HTMLString s) = HtmlInline s
textContentToInline (Atom.XHTMLString x) = HtmlInline (show x)
