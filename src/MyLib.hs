module MyLib (someFunc, someFunc2) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

someFunc2 :: Int -> IO ()
someFunc2 = print
