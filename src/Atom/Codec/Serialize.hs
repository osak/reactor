{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Atom.Codec.Serialize where

import Atom (Category (..), Content (..), Entry (..), Link (..), Person (..), uriToText, Source(..))
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Time (ZonedTime)
import Data.Time.Format.ISO8601
import XML.Codec.Serialize (Attribute (Attribute))
import qualified XML.Codec.Serialize as X

serializeEntry :: Entry -> L.Text
serializeEntry e =
  let xml =
        X.Tag
          { X.name = "atom:entry",
            X.attributes = [],
            X.children =
              concat
                [ map (serializePersonConstruct "atom:author") e.author,
                  map serializeCategory e.category,
                  [serializeContent e.content],
                  map (serializePersonConstruct "atom:contributor") e.contributor,
                  [simpleTextTag "atom:id" e.id],
                  map serializeLink e.link,
                  case e.published of
                    Just t -> [serializeDateConstruct "atom:published" t]
                    Nothing -> [],
                  case e.rights of
                    Just r -> [simpleTextTag "atom:rights" r]
                    Nothing -> [],
                  case e.source of
                    Just s -> [serializeSourceConstruct "atom:source" s]
                    Nothing -> [],
                  case e.summary of
                    Just s -> [simpleTextTag "atom:summary" s]
                    Nothing -> [],
                  [simpleTextTag "atom:title" e.title],
                  [serializeDateConstruct "atom:updated" e.updated]
                ]
          }
   in X.serializeNode xml

-- | Serializes `Person` as a list of tags representing Atom Person construct.
serializePersonConstruct :: T.Text -> Person -> X.Node
serializePersonConstruct tag p =
  let contents =
        catMaybes
          [ Just $ simpleTextTag "atom:name" p.name,
            fmap (simpleTextTag "atom:uri" . uriToText) p.uri,
            fmap (simpleTextTag "atom:email") p.email
          ]
   in X.Tag
        { X.name = tag,
          X.attributes = [],
          X.children = contents
        }

-- | Serializes `Category` as Atom Category XML Node.
serializeCategory :: Category -> X.Node
serializeCategory c =
  X.Tag
    { X.name = "atom:category",
      X.attributes =
        catMaybes
          [ Just $ Attribute "term" c.term,
            fmap (Attribute "scheme") c.scheme,
            fmap (Attribute "label") c.label
          ],
      X.children = []
    }

serializeContent :: Content -> X.Node
serializeContent (InlineText {text, mimeType}) =
  let typeAttr = fromMaybe "text" mimeType
   in X.Tag
        { name = "atom:content",
          attributes = [Attribute "type" typeAttr],
          children = [X.Text text]
        }
serializeContent (InlineXHtml _) = undefined
serializeContent (OutOfLine uri) =
  X.Tag
    { name = "atom:content",
      attributes = [Attribute "src" (uriToText uri)],
      children = []
    }

serializeLink :: Link -> X.Node
serializeLink l =
  X.Tag
    { X.name = "atom:link",
      X.attributes =
        catMaybes
          [ Just $ Attribute "href" l.href,
            fmap (Attribute "rel") l.rel,
            fmap (Attribute "type") l.linkType,
            fmap (Attribute "hreflang") l.hrefLang,
            fmap (Attribute "title") l.title,
            fmap (Attribute "length") l.length
          ],
      X.children = []
    }

serializeDateConstruct :: T.Text -> ZonedTime -> X.Node
serializeDateConstruct tag t =
  let timestamp = T.pack $ iso8601Show t
   in simpleTextTag tag timestamp

serializeSourceConstruct :: T.Text -> Source -> X.Node
serializeSourceConstruct tag s = undefined

-- | Creates a `Tag` node whose only children is a `Text` node of the specified text.
simpleTextTag :: T.Text -> T.Text -> X.Node
simpleTextTag name text =
  X.Tag
    { X.name = name,
      X.attributes = [],
      X.children = [X.Text text]
    }