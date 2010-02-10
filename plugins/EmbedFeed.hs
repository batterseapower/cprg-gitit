{-# LANGUAGE ViewPatterns, PatternGuards #-}
module EmbedFeed (plugin) where

import Data.Maybe
import Data.Ord
import Data.List

import Network.Gitit.Interface
import Network.Browser
import Network.HTTP

import qualified Text.Atom.Feed as Atom
import qualified Text.RSS.Syntax as RSS
import Text.Feed.Types
import Text.Feed.Import

 
plugin :: Plugin
plugin = mkPageTransformM transformBlock
 
transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (_, classes, namevals) contents)
  | "feed" `elem` classes , Just url <- lookup "url" namevals = do
    feed_data <- liftIO $ browse $ do
        setAllowRedirects True
        fmap (rspBody . snd) $ request $ getRequest url
    
    --liftIO $ print $ parseFeedString feed_data
    
    return $ case parseFeedString feed_data of
      Just (feedToEntries -> Just entries)
        -> BulletList [[Plain [Link [title] (uri, linktitle)]] | (title, (uri, linktitle)) <- entries]
      _ -> Para [Emph [Str "Error parsing feed from", Str url]]
transformBlock x = return x

feedToEntries :: Feed -> Maybe [(Inline, (String, String))]
feedToEntries (AtomFeed f)
  = Just [ (title, (Atom.linkHref link, fromMaybe (show title) (Atom.linkTitle link)))
         | Atom.Entry { Atom.entryTitle = (textContentToInline -> title), Atom.entryLinks = links } <- Atom.feedEntries f
         , (link:_) <- [sortBy (comparing (maybe True (\t -> "omment" `isInfixOf` t) . Atom.linkTitle))
                               [link | link@(Atom.Link { Atom.linkType = Just "text/html" }) <- links]]
         ]
feedToEntries (RSSFeed r)
  = Just [ (Str title, (url, title))
         | item@(RSS.RSSItem { RSS.rssItemLink = Just url }) <- RSS.rssItems (RSS.rssChannel r)
         , let title = fromMaybe (fromJust $ RSS.rssItemDescription item) (RSS.rssItemTitle item)
         ]
feedToEntries _ = Nothing

textContentToInline (Atom.TextString s) = Str s
textContentToInline (Atom.HTMLString s) = HtmlInline s
textContentToInline (Atom.XHTMLString x) = HtmlInline (show x)
