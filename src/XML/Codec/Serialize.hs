{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module XML.Codec.Serialize
  ( Node (..),
    Attribute (..),
    Document (..),
    serializeDoc,
    serializeNode,
    serializeAttribute,
  )
where

import qualified Data.Text as T
import qualified Data.Text.Lazy as L

-- | Key-value pair representing an attribute.
data Attribute = Attribute T.Text T.Text

-- | XML Node. It is either an XML tag or a chunk of plain text.
data Node
  = Tag -- ^ XML Tag.
      { name :: T.Text, -- ^ Name of the tag.
        attributes :: [Attribute], -- ^ List of XML attributes.
        children :: [Node] -- ^ Children of this node, in the order of appearance.
      }
  | Text T.Text -- ^ Plain text.

-- | XML Document.
newtype Document = Document
  { root :: Node
  }

-- | Serializes `Document` into an XML text.
serializeDoc :: Document -> L.Text
serializeDoc = serializeNode . root

-- | Serializes a `Node` into an XML fragment.
serializeNode :: Node -> L.Text
serializeNode Tag {name, attributes, children} =
  case children of
    [] -> "<"
            `L.append` L.fromStrict name
            `L.append` L.concat (map (L.cons ' ' . serializeAttribute) attributes)
            `L.append` " />"
    c -> "<"
            `L.append` L.fromStrict name
            `L.append` L.concat (map (L.cons ' ' . serializeAttribute) attributes)
            `L.append` ">"
            `L.append` L.concat (map serializeNode c)
            `L.append` "</"
            `L.append` L.fromStrict name
            `L.append` ">"
serializeNode (Text t) = L.concatMap escape $ L.fromStrict t
  where
    escape '<' = "&lt;"
    escape '>' = "&gt;"
    escape '"' = "&quot;"
    escape c = L.singleton c

-- | Serializes an attribute. The resulted text is formatted as `key="val"`.
--
-- The value is escaped so that it does not contain '"' in the output. Example:
--
-- >>> serializeAttribute (Attribute "key" "a\"b")
-- key="a&quot;b"
serializeAttribute :: Attribute -> L.Text
serializeAttribute (Attribute name val) =
  L.fromStrict name
    `L.append` "="
    `L.append` serializeAsQuoted (L.fromStrict val)
  where
    serializeAsQuoted t =
      "\""
        `L.append` escapeQuotes t
        `L.append` "\""
    escapeQuotes = L.replace "\"" "&quot;"