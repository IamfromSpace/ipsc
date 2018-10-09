module Lib
    ( noOpposites
    , Direction(..)
    , perms
    , mazeSize
    , genPoints
    , reaches
    , search
    , search'
    , doIt
    ) where

import Data.List
import Data.Maybe

data Direction
    = R
    | U
    | L
    | D
    | LU
    | UR
    | RD
    | DL
    deriving (Show, Eq)

opposite :: Direction -> Direction
opposite d =
    case d of
        R -> L
        U -> D
        L -> R
        D -> U
        LU -> RD
        UR -> DL
        RD -> LU
        DL -> UR

noOpposites :: [Direction] -> Bool
noOpposites ds =
    case ds of
        (h:t) ->
            snd $
            foldl
                (\(last, b) next -> (next, b && opposite last /= next))
                (h, True)
                t
        [] -> True

perms :: [[Direction]]
perms = filter noOpposites $ permutations [R, U, L, D, LU, UR, RD, DL]

-- Computes if we can jump from one open empty maze position to another
reaches :: (Int, Int) -> (Int, Int) -> Bool
reaches (x1, y1) (x2, y2) =
    x1 == x2 || y1 == y2 || y2 - y1 == x2 - x1 || y2 - y1 == x1 - x2

mazeSize :: [(Int, Int)] -> Int
mazeSize points =
    (maximum (map fst points) - minimum (map fst points) + 1) *
    (maximum (map snd points) - minimum (map snd points) + 1)

move :: (Direction, Int) -> (Int, Int) -> (Int, Int)
move (d, a) (x, y) =
    case d of
        R -> (x + a, y)
        U -> (x, y + a)
        L -> (x - a, y)
        D -> (x, y - a)
        LU -> (x - a, y + a)
        UR -> (x + a, y + a)
        RD -> (x + a, y - a)
        DL -> (x - a, y - a)

genPoints :: [(Direction, Int)] -> [(Int, Int)]
genPoints = foldr (\v ps -> move v (head ps) : ps) [(0, 0)]

search ::
       [(Direction, Int)]
    -> (Direction, Int)
    -> [Direction]
    -> Maybe [(Direction, Int)]
search history (d, a) remaining =
    let n = (d, a + 1)
        ps = genPoints (n : history)
    in if mazeSize ps < 50
           then if any (reaches (head ps)) (tail (tail ps))
                    then search history n remaining
                    else case remaining of
                             (h:t) -> search (n : history) (h, 0) t
                             [] -> Just (n : history)
           else case history of
                    (h:t) -> search t h (d : remaining)
                    [] -> Nothing

search' :: [Direction] -> Maybe [(Direction, Int)]
search' ds = search [] (head ds, 0) (tail ds)

doIt = genPoints $ head $ catMaybes $ filter (Nothing /=) $ map search' perms
