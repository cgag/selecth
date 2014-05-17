module Score where

import Data.Char
import Data.List

-- IDEA: look for position of last character first to limit
-- the potential search area of end match or quickly eliminate the str
-- -- Do this with all chars?

-- TODO: test this properly
-- TODO: matchLength searches for first char anywhere in string,
-- it should return 0 if first char isn't correct, otherwise find endMatch
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

{-hasMatch :: String -> String -> Bool-}
{-hasMatch query choice = True-}


minMatchLength :: String -> String -> Int
minMatchLength [] _ = 1
minMatchLength _ [] = 0
minMatchLength query@(q:_) choice = 
  let lengths = filter (>0) 
                $ map (matchLength query)
                {-$ filter (hasMatch query)-}
                $ filter ((== q) . head) 
                $ filter (not . null)
                $ tails choice
  in if null lengths 
     then 0
     else minimum lengths

score :: String -> String -> Double
score q choice
    | null q      = 1
    | null choice = 0
    | otherwise = let minLength = minMatchLength (map toLower q) (map toLower choice)
                  in if minLength > 0 
                     then fromIntegral (length q) / fromIntegral minLength -- penalize bigger match lengths
                          / fromIntegral (length choice) -- penalize longer choice strings
                     else 0 
