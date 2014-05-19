module Score where

import           Control.Parallel.Strategies
import           Data.Char
import           Data.List

-- TODO: test this properly
minMatchLength :: String -> String -> Int
minMatchLength [] _  =  1
minMatchLength _ []  =  0
minMatchLength (qHead:rest) choice =
    let matchLenghts =  filter (>0)
                        $ map (\t -> endMatch rest (drop 1 t) 1)
                        $ filter ((== qHead) . head)
                        $ filter (not . null)
                        $ tails choice
    in  if null matchLenghts
        then 0
        else minimum matchLenghts
  where
    endMatch [] _ lastIndex = lastIndex
    endMatch (q:qs) s lastIndex =
        case elemIndex q s of
            Just i -> endMatch qs (drop (i + 1) s) (i + 1 + lastIndex)
            Nothing -> 0

normalizeScore :: Int -> String -> String -> Double
normalizeScore matchLength query choice =
    if matchLength > 0
    then fromIntegral (length query) / fromIntegral matchLength -- penalize bigger match lengths
          / fromIntegral (length choice) -- penalize longer choice strings
    else 0

score :: String -> String -> Double
score q choice
    | null q      = 1
    | null choice = 0
    | otherwise = let minLength = minMatchLength q (map toLower choice)
                  in normalizeScore minLength q choice

scoreAll :: String -> [String] -> [(String, Double)]
scoreAll query choices = 
    let lowerQuery = map toLower query
    in map (\choice -> (choice, score lowerQuery choice)) choices
       `using` parListChunk 1000 rdeepseq
