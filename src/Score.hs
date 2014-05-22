{-# LANGUAGE ViewPatterns #-}

module Score where

import           Control.Parallel.Strategies
import qualified Data.Text                   as T

-- TODO: test this properly
minMatchLength :: T.Text -> T.Text -> Int
minMatchLength (T.uncons -> Nothing) _  =  1
minMatchLength _ (T.uncons -> Nothing)  =  0
minMatchLength (T.uncons -> Just (qHead, rest)) choice =
    let matchLengths =  filter (>0)
                        $ map (\t -> endMatch rest (T.drop 1 t) 1)
                        $ filter ((== qHead) . T.head)
                        $ filter (not . T.null)
                        $ T.tails choice
    in  if null matchLengths
        then 0
        else minimum matchLengths
  where
    endMatch :: T.Text -> T.Text -> Int -> Int
    endMatch (T.uncons -> Nothing) _ lastIndex = lastIndex
    endMatch (T.uncons -> Just (q, qs)) s lastIndex =
        case T.findIndex (== q) s of
            Just i -> endMatch qs (T.drop (i + 1) s) (i + 1 + lastIndex)
            Nothing -> 0

normalizeScore :: Int -> T.Text -> T.Text -> Double
normalizeScore matchLength query choice =
    if matchLength > 0
    then fromIntegral (T.length query) / fromIntegral matchLength -- penalize bigger match lengths
          / fromIntegral (T.length choice) -- penalize longer choice strings
    else 0

score :: T.Text -> T.Text -> Double
score q choice
    | T.null q      = 1
    | T.null choice = 0
    | otherwise = let minLength = minMatchLength q (T.toLower choice)
                  in normalizeScore minLength q choice

scoreAll :: T.Text -> [T.Text] -> [(T.Text, Double)]
scoreAll query choices =
    let lowerQuery = T.toLower query
    in map (\choice -> (choice, score lowerQuery choice)) choices
       `using` parListChunk 1000 rdeepseq
