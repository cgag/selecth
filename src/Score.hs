module Score where

import qualified Data.Text as T

-- TODO: test this properly
-- pattern match on uncons
-- TODO: matchLength searches for first char anywhere in string,
-- it should return 0 if first char isn't correct, otherwise find endMatch
matchLength :: T.Text -> T.Text -> Int
matchLength q str
    | T.null q   = 1
    | T.null str = 0
    | otherwise = 
        let (firstChar, rest) = (T.head q, T.tail q)
        in
            case T.findIndex (== firstChar) str of
                Just startPos -> let endPos = endMatch rest (T.drop (startPos + 1) str) 
                                                            (startPos + 1)
                                 in if endPos == 0 
                                    then 0
                                    else endPos - startPos
                Nothing -> 0
          where 
            endMatch query s lastIndex 
                | T.null query = lastIndex
                | otherwise = let (qFirst, qRest) = (T.head query, T.tail query)
                              in case T.findIndex (== qFirst) s of
                                  Just i -> endMatch qRest (T.drop (i + 1) s) (i + 1 + lastIndex)
                                  Nothing -> 0


minMatchLength :: T.Text -> T.Text -> Int
minMatchLength query choice
    | T.null query  = 1
    | T.null choice = 0
    | otherwise = let q = T.head query
                      lengths = filter (>0) 
                                $ map (matchLength query)
                                $ filter ((== q) . T.head)
                                $ filter (not . T.null)
                                $ T.tails choice
        in if null lengths 
           then 0
           else minimum lengths

score :: T.Text -> T.Text -> Double
score q choice
    | T.null q      = 1
    | T.null choice = 0
    | otherwise = let minLength = minMatchLength (T.toLower q) (T.toLower choice)
                  in if minLength > 0 
                     then fromIntegral (T.length q) / fromIntegral minLength -- penalize bigger match lengths
                          / fromIntegral (T.length choice) -- penalize longer choice strings
                     else 0 
