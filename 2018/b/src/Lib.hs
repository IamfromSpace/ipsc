module Lib
    ( someFunc
    , answer
    , foldPaper
    , p
    , Edge(..)
    , Axis(..)
    , Corner(..)
    , foldAndCount
    , parseGroup
    , parse
    ) where

import Data.List
import Data.List.Split

someFunc :: IO ()
someFunc = putStrLn "someFunc"

m x = mod x (10 ^ 9 + 7)

answer :: String -> String
answer s = intercalate "\n" (map (show . m . foldAndCount) (parse s))

data Edge
    = L
    | B
    | T
    | R
    deriving (Eq, Show)

data Corner
    = LT_
    | TR
    | RB
    | BL
    deriving (Eq, Show)

data Axis
    = TB
    | LR
    deriving (Eq, Show)

data Sheet =
    Sheet Integer
          Integer
          Integer
          Integer
          Integer
          Integer
          Integer
          Integer
          Integer
    deriving (Show)

foldLeft :: Sheet -> Sheet
foldLeft (Sheet c l t r b lt tr rb bl) =
    Sheet (c * 2) c (2 * t) (r + l) (2 * b) t (lt + tr) (rb + bl) b

rotate90CW :: Sheet -> Sheet
rotate90CW (Sheet c l t r b lt tr rb bl) = Sheet c b l t r bl lt tr rb

r :: Sheet -> Sheet
r (Sheet c l t r b lt tr rb bl) =
    Sheet (c * 2) (r + l) (2 * t) c (2 * b) (lt + tr) t b (bl + rb)

foldPaper :: Edge -> Sheet -> Sheet
foldPaper L s = foldLeft s
foldPaper R s = r s
foldPaper T s = rotate90CW (foldLeft (rotate90CW (rotate90CW (rotate90CW s))))
foldPaper B s = rotate90CW (rotate90CW (rotate90CW (foldLeft (rotate90CW s))))

countCuts :: Corner -> Sheet -> Integer
countCuts LT_ (Sheet _ _ _ _ _ x _ _ _) = x
countCuts TR (Sheet _ _ _ _ _ _ x _ _) = x
countCuts RB (Sheet _ _ _ _ _ _ _ x _) = x
countCuts BL (Sheet _ _ _ _ _ _ _ _ x) = x

p = Sheet 1 1 1 1 1 1 1 1 1

parseEdge :: String -> Edge
parseEdge "L" = L
parseEdge "B" = B
parseEdge "T" = T
parseEdge "R" = R

parseCorner :: String -> Corner
parseCorner "LT" = LT_
parseCorner "TL" = LT_
parseCorner "TR" = TR
parseCorner "RT" = TR
parseCorner "RB" = RB
parseCorner "BR" = RB
parseCorner "BL" = BL
parseCorner "LB" = BL
parseCorner x = error ("saw " ++ x)

parseAxis :: String -> Maybe Axis
parseAxis "TB" = Just TB
parseAxis "BT" = Just TB
parseAxis "LR" = Just LR
parseAxis "RL" = Just LR
parseAxis _ = Nothing

countCutsAxis :: Axis -> [Edge] -> Integer
countCutsAxis TB es = 2 ^ length (filter (\x -> x == L || x == R) es) + 1
countCutsAxis LR es = 2 ^ length (filter (\x -> x == T || x == B) es) + 1

parseGroup :: [String] -> ([Edge], Either Corner Axis)
parseGroup (_:es:c:_) =
    let ee = map (parseEdge . pure) es
    in case parseAxis c of
           Just a -> (ee, Right a)
           Nothing -> (ee, Left (parseCorner c))

parse :: String -> [([Edge], Either Corner Axis)]
parse s = map parseGroup (chunksOf 3 (filter ((/=) "") (drop 2 (lines s))))

foldAndCountCorners :: ([Edge], Corner) -> Integer
foldAndCountCorners (es, c) = countCuts c (foldl (flip foldPaper) p es) + 1

foldAndCountAxis :: ([Edge], Axis) -> Integer
foldAndCountAxis (es, a) = countCutsAxis a es

foldAndCount :: ([Edge], Either Corner Axis) -> Integer
foldAndCount (es, e) =
    case e of
        Left c -> foldAndCountCorners (es, c)
        Right a -> foldAndCountAxis (es, a)
