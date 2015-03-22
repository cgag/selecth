{-# LANGUAGE ViewPatterns #-}

module Score where

import           Control.Parallel.Strategies
import Data.Text (Text)
import qualified Data.Text                   as T

-- TODO: add tests
minMatchLength :: Text -> Text -> Int
minMatchLength (T.uncons -> Nothing) _  =  1
minMatchLength _ (T.uncons -> Nothing)  =  0
minMatchLength (T.uncons -> Just (qHead, rest)) choice =
    let matchLengths =  filter (>0)
                        . map (\t -> endMatch rest (T.drop 1 t) 1)
                        . filter ((== qHead) . T.head)
                        . filter (not . T.null)
                        $ T.tails choice
    in  if null matchLengths
        then 0
        else minimum matchLengths
  where
    endMatch :: Text -> Text -> Int -> Int
    endMatch (T.uncons -> Nothing) _ lastIndex = lastIndex
    endMatch (T.uncons -> Just (q, qs)) s lastIndex =
        case T.findIndex (== q) s of
            Just i -> endMatch qs (T.drop (i + 1) s) (i + 1 + lastIndex)
            Nothing -> 0

normalizeScore :: Int -> Text -> Text -> Double
normalizeScore matchLength query choice
    | matchLength <= 0 = 0
    | otherwise =
          fromIntegral (T.length query)
          / fromIntegral matchLength       -- penalize longer match lengths
          / fromIntegral (T.length choice) -- penalize longer choice strings

score :: Text -> Text -> Double
score q choice
    | T.null q      = 1
    | T.null choice = 0
    | otherwise = let minLength = minMatchLength q (T.toLower choice)
                  in normalizeScore minLength q choice

scoreAll :: Text -> [Text] -> [(Text, Double)]
scoreAll query choices =
    map (\choice -> (choice, score (T.toLower query) choice)) choices
       `using` parListChunk 1000 rdeepseq
