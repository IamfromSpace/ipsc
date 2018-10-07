module Lib
    ( someFunc
    , answer
    , lexographic
    , toLexo
    , toBigLexo
    , parse
    ) where

import Data.List
import Data.List.Split

someFunc :: IO ()
someFunc = putStrLn "someFunc"

answer :: String -> String
answer s = intercalate "\n" (map (toBigLexo) (parse s))

lexographic' :: String -> String -> String -> String -> String
lexographic' a b (ha:ta) (hb:tb) =
    if ha < hb
        then a
        else if ha == hb
                 then lexographic' a b ta tb
                 else b
lexographic' a b [] (hb:tb) = a
lexographic' a b (ha:ta) [] = b

lexographic :: String -> String -> String
lexographic a b = lexographic' a b a b

toLexo :: Int -> String
toLexo x = '1' : (replicate (length (show x) - 1) '0')

toBigLexo :: [Int] -> String
toBigLexo xs = intercalate " " (map toLexo (take (length xs - 1) xs) ++ ["1"])

parseLine :: String -> [Int]
parseLine s = map read (splitOn " " s)

parse :: String -> [[Int]]
parse s =
    map (\(_:s:_) -> parseLine s) $
    chunksOf 2 (filter ((/=) "") (drop 2 (lines s)))
