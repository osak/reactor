module Main (main) where

import qualified Atom.Codec.SerializeSpec as AtomSerialize
import Test.Tasty
import qualified XML.Codec.SerializeSpec as XMLSerialize

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ XMLSerialize.tests,
        AtomSerialize.tests
      ]
