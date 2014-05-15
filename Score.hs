module Score where

import Data.Char
import Data.List

-- TODO: test this properly
matchLength :: String -> String -> Int
matchLength [] _  =  1
matchLength _ []  =  0
matchLength (firstChar:rest) str = 
    case elemIndex firstChar str of
        Just startPos -> let endPos = endMatch rest (drop (startPos + 1) str) 
                                                    (startPos + 1)
                         in if endPos == 0 
                            then 0
                            else endPos - startPos
        Nothing -> 0
  where 
    endMatch [] _ lastIndex = lastIndex 
    endMatch (q:qs) s lastIndex = case elemIndex q s of
        Just i -> endMatch qs (drop (i + 1) s) (i + 1 + lastIndex)
        Nothing -> 0


minMatchLength :: String -> String -> Int
minMatchLength [] _ = 1
minMatchLength _ [] = 0
minMatchLength query@(q:_) choice = 
  let lengths = filter (>0) 
                $ map (matchLength query)
                $ filter (isPrefixOf [q])
                $ tails choice
  in if null lengths 
     then 0
     else minimum lengths

score :: String -> String -> Int
score q choice
    | null q      = 1
    | null choice = 0
    | otherwise = minMatchLength (map toLower q) (map toLower choice)
