{-# LANGUAGE OverloadedStrings #-}

module XML.Codec.SerializeSpec(tests) where

import XML.Codec.Serialize as X
import Test.Tasty.HUnit
import Test.Tasty

testSerializeDoc :: Assertion
testSerializeDoc =
  let doc =
        X.Document
          { root =
              X.Tag
                { name = "root",
                  attributes = [],
                  children =
                    [ X.Tag {name = "inner", attributes = [], children = []},
                      X.Text "Hello world!",
                      X.Tag {name = "inner2", attributes = [], children = []}
                    ]
                }
          }
      serialized = X.serializeDoc doc
   in "<root><inner />Hello world!<inner2 /></root>" @=? serialized

testSerializeDocEscapeSpecialChars :: Assertion
testSerializeDocEscapeSpecialChars =
  let doc =
        X.Document
          { root =
              X.Tag
                { name = "root",
                  attributes = [],
                  children =
                    [ 
                        X.Text "Hello world! ><\""
                    ]
                }
          }
      serialized = X.serializeDoc doc
   in "<root>Hello world! &gt;&lt;&quot;</root>" @=? serialized

testSerializeDocEscapeAttributes :: Assertion
testSerializeDocEscapeAttributes =
  let doc =
        X.Document
          { root =
              X.Tag
                { name = "root",
                  attributes =
                    [ X.Attribute "attr1" "value",
                      X.Attribute "attr2" "a\"b"
                    ],
                  children = []
                }
          }
      serialized = X.serializeDoc doc
   in "<root attr1=\"value\" attr2=\"a&quot;b\" />" @=? serialized

testSerializeEmptyTag :: Assertion
testSerializeEmptyTag =
  let doc =
        X.Document {
          root = X.Tag {
            name = "root",
            attributes = [],
            children = []
          }
        }
      serialized = X.serializeDoc doc
  in "<root />" @?= serialized

testSerializeEmptyTagWithAttributes :: Assertion
testSerializeEmptyTagWithAttributes =
  let doc =
        X.Document {
          root = X.Tag {
            name = "root",
            attributes = [Attribute "attr1" "value"],
            children = []
          }
        }
      serialized = X.serializeDoc doc
  in "<root attr1=\"value\" />" @=? serialized

tests::TestTree
tests = testGroup "XML.Codec.Serialize"
    [ testCase "Serialize Document" testSerializeDoc,
      testCase "Escape special charactors in text nodes" testSerializeDocEscapeSpecialChars,
      testCase "Escape special charactors in attributes" testSerializeDocEscapeAttributes,
      testCase "Serialize empty tag" testSerializeEmptyTag,
      testCase "Serialize empty tag with attributes" testSerializeEmptyTagWithAttributes
    ]