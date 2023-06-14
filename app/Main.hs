module Main where

import qualified MyLib (someFunc, someFunc2)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
  MyLib.someFunc
  MyLib.someFunc2 12