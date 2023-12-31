{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Atom.Codec.SerializeSpec (tests) where

import Atom (Category (..), Content (..), Entry (..), Person (..))
import Atom.Codec.Serialize (serializeEntry)
import Data.Maybe (fromJust)
import qualified Data.Text.Lazy as L
import Data.Time (ZonedTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

personFixture :: Person
personFixture =
  Person
    { name = "Azusa Nakano",
      uri = Just "https://azusa.example.com",
      email = Just "azusa@example.com"
    }

entryFixture :: Entry
entryFixture =
  Entry
    { author = [personFixture],
      category =
        [ Category {term = "cat1", scheme = Just "s", label = Just "label1"}
        ],
      content = InlineText {text = "Test Entry", mimeType = Nothing},
      contributor = [personFixture],
      id = "entry1",
      link = [],
      published = Nothing,
      rights = Just "(c) Nakano Azusa 2023",
      source = Nothing,
      summary = Just "Summary",
      title = "Test Title",
      updated = fromJust (iso8601ParseM "2023-06-14T19:18:00+09:00" :: Maybe ZonedTime)
    }

expectedEntryXML :: L.Text
expectedEntryXML =
  "\
  \<atom:entry>\
  \<atom:author>\
  \<atom:name>Azusa Nakano</atom:name>\
  \<atom:uri>https://azusa.example.com</atom:uri>\
  \<atom:email>azusa@example.com</atom:email>\
  \</atom:author>\
  \<atom:category term=\"cat1\" scheme=\"s\" label=\"label1\" />\
  \<atom:content type=\"text\">Test Entry</atom:content>\
  \<atom:contributor>\
  \<atom:name>Azusa Nakano</atom:name>\
  \<atom:uri>https://azusa.example.com</atom:uri>\
  \<atom:email>azusa@example.com</atom:email>\
  \</atom:contributor>\
  \<atom:id>entry1</atom:id>\
  \<atom:rights>(c) Nakano Azusa 2023</atom:rights>\
  \<atom:summary>Summary</atom:summary>\
  \<atom:title>Test Title</atom:title>\
  \<atom:updated>2023-06-14T19:18:00+09:00</atom:updated>\
  \</atom:entry>"

minimumEntryFixture :: Entry
minimumEntryFixture =
  Entry
    { author = [],
      category = [],
      content = InlineText {text = "Test Entry", mimeType = Nothing},
      contributor = [],
      id = "entry1",
      link = [],
      published = Nothing,
      rights = Nothing,
      source = Nothing,
      summary = Nothing,
      title = "Test Title",
      updated = fromJust (iso8601ParseM "2023-06-14T19:18:00+09:00" :: Maybe ZonedTime)
    }

minimumEntryXML :: L.Text
minimumEntryXML =
  "\
  \<atom:entry>\
  \<atom:content type=\"text\">Test Entry</atom:content>\
  \<atom:id>entry1</atom:id>\
  \<atom:title>Test Title</atom:title>\
  \<atom:updated>2023-06-14T19:18:00+09:00</atom:updated>\
  \</atom:entry>"

tests :: TestTree
tests =
  testGroup
    "Atom.Codec.SerializeSpec"
    [ testCase "Serialize into XML" $ expectedEntryXML @=? serializeEntry entryFixture,
      testCase "Serialize minimum entry into XML" $ minimumEntryXML @=? serializeEntry minimumEntryFixture
    ]