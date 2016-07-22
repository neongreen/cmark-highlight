{-# LANGUAGE
ViewPatterns
  #-}


module CMark.Highlight
(
  -- * Highlighting
  highlightNode,
  highlightNodeWith,

  -- * Options
  FormatOptions(..),
  defaultFormatOpts,

  -- * Styles
  module Text.Highlighting.Kate.Styles,
  styleToCss,
)
where


import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import CMark
import Text.Highlighting.Kate
import Text.Highlighting.Kate.Styles
import Text.Blaze.Html.Renderer.Text


{- |
Highlight a document by replacing code blocks with raw HTML blocks.

By default, the rest of the attribute line (i.e. all words after the first word after @~~~@ or @```@) get added as classes to the container block of the code.
-}
highlightNode :: Node -> Node
highlightNode = highlightNodeWith (\_ _ x -> x)

{- |
The function is given code block's language (i.e. the 1st word of the attribute line after @~~~@ or @```@) and the rest of the attribute line.

If you don't want the classes to be derived from the attribute line, make the function set 'containerClasses' to @[]@.
-}
highlightNodeWith
  :: (Text -> Text -> FormatOptions -> FormatOptions) -> Node -> Node
highlightNodeWith f (Node pos (CODE_BLOCK info code) ns) =
    Node pos (HTML_BLOCK formatted) (map (highlightNodeWith f) ns)
  where
    (codeLang, T.drop 1 -> codeInfo) = T.break (== ' ') (T.strip info)
    highlighted = highlightAs (T.unpack codeLang) (T.unpack code)
    opts = defaultFormatOpts {
             containerClasses = words (T.unpack codeInfo) }
    formatted = TL.toStrict . renderHtml $
      formatHtmlBlock (f codeLang codeInfo opts) highlighted
highlightNodeWith f (Node pos type_ ns) =
    Node pos type_ (map (highlightNodeWith f) ns)
