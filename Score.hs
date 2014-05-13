module Score where

import Data.Char
import Data.List

data EndMatch = Failure | EndPos Int

-- s must start with first char of que
-- query is 
matchLength :: String -> String -> Int
matchLength qrest str = go qrest str 0 
  where 
    go [] _ lastIndex = lastIndex 
    go (q:qs) s lastIndex = case elemIndex q s of
        Just i -> go qs (drop (i + 1) s) (i + 1 + lastIndex)
        Nothing -> 0
    

minMatchLength :: String -> String -> Int
minMatchLength [] _ = 1
minMatchLength _ [] = 0
minMatchLength (first:queryRest) choice = minimum $ filter (>0) matchLengths
  where
    firstIndices = elemIndices first choice
    matchLengths = [1,2]

score :: String -> String -> Int
score q choice
    | null q      = 1
    | null choice = 0
    | otherwise = minMatchLength (map toLower q) (map toLower choice)
