module Main where

import Lib

main :: IO ()
main = fmap answer getContents >>= putStr
