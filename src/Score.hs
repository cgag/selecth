module Score where

import qualified Data.Vector as V
import Data.Char

-- TODO: test this properly
-- pattern match on uncons
-- TODO: matchLength searches for first char anywhere in string,
-- it should return 0 if first char isn't correct, otherwise find endMatch
matchLength :: V.Vector Char -> V.Vector Char -> Int
matchLength q str
    | V.null q   = 1
    | V.null str = 0
    | otherwise = 
        let (firstChar, rest) = (V.head q, V.tail q)
        in
            case V.findIndex (== firstChar) str of
                Just startPos -> let endPos = endMatch rest (V.drop (startPos + 1) str) 
                                                            (startPos + 1)
                                 in if endPos == 0 
                                    then 0
                                    else endPos - startPos
                Nothing -> 0
          where 
            endMatch query s lastIndex 
                | V.null query = lastIndex
                | otherwise = let (qFirst, qRest) = (V.head query, V.tail query)
                              in case V.findIndex (== qFirst) s of
                                  Just i -> endMatch qRest (V.drop (i + 1) s) (i + 1 + lastIndex)
                                  Nothing -> 0

vecTails :: V.Vector a -> [V.Vector a]
vecTails v = map (`V.drop` v) [0..(V.length v)]

minMatchLength :: V.Vector Char -> V.Vector Char -> Int
minMatchLength query choice
    | V.null query  = 1
    | V.null choice = 0
    | otherwise = let q = V.head query
                      lengths = V.filter (>0) 
                                $ V.map (matchLength query)
                                $ V.filter ((== q) . V.head)
                                $ V.filter (not . V.null)
                                $ V.fromList
                                $ vecTails choice
        in if   V.null lengths 
           then 0
           else V.minimum lengths

score :: V.Vector Char -> V.Vector Char -> Double
score q choice
    | V.null q      = 1
    | V.null choice = 0
    | otherwise = let minLength = minMatchLength (V.map toLower q) (V.map toLower choice)
                  in if minLength > 0 
                     then fromIntegral (V.length q) / fromIntegral minLength -- penalize bigger match lengths
                          / fromIntegral (V.length choice) -- penalize longer choice strings
                     else 0 
