{-# LANGUAGE DuplicateRecordFields #-}

module Atom(
  Person(..),
  Category(..),
  Generator(..),
  Link(..),
  Entry(..),
  Content(..),
  Source(..),
  Feed(..),
  uriToText
) where

import qualified Data.Text as T
import Data.Time (ZonedTime)

type URI = T.Text

uriToText :: URI -> T.Text
uriToText t = t :: T.Text

-- See https://datatracker.ietf.org/doc/html/rfc4287

data Person = Person
  { name :: T.Text,
    uri :: Maybe URI,
    email :: Maybe T.Text
  }

data Category = Category
  { term :: T.Text,
    scheme :: Maybe T.Text,
    label :: Maybe T.Text
  }

data Generator = Generator
  { uri :: Maybe URI,
    version :: Maybe T.Text
  }

data Link = Link
  { href :: URI,
    rel :: Maybe T.Text,
    linkType :: Maybe T.Text,
    hrefLang :: Maybe T.Text,
    title :: Maybe T.Text,
    length :: Maybe T.Text
  }

data Content
  = InlineText
      { text :: T.Text,
        mimeType :: Maybe T.Text
      }
  | InlineXHtml T.Text
  | OutOfLine URI

data Source = Source
  { author :: [Person],
    category :: [Category],
    contributor :: [Person],
    generator :: Maybe Generator,
    icon :: Maybe URI,
    id :: T.Text,
    link :: [Link],
    logo :: Maybe URI,
    rights :: Maybe T.Text,
    subtitle :: Maybe T.Text,
    title :: T.Text,
    updated :: ZonedTime
  }

data Entry = Entry
  { author :: [Person],
    category :: [Category],
    content :: Content,
    contributor :: [Person],
    id :: T.Text,
    link :: [Link],
    published :: Maybe ZonedTime,
    rights :: Maybe T.Text,
    source :: Maybe Source,
    summary :: Maybe T.Text,
    title :: T.Text,
    updated :: ZonedTime
  }

data Feed = Feed
  { author :: [Person],
    category :: [Category],
    contributor :: [Person],
    generator :: Maybe Generator,
    icon :: Maybe URI,
    id :: T.Text,
    link :: [Link],
    logo :: Maybe URI,
    rights :: Maybe T.Text,
    subtitle :: Maybe T.Text,
    title :: T.Text,
    updated :: ZonedTime,
    entry :: [Entry]
  }