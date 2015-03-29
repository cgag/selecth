{-# LANGUAGE ViewPatterns #-}

module Score where

import           Control.Parallel.Strategies
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V
import           GHC.Conc                    (numCapabilities)

-- TODO: add tests

normalizeScore :: Int -> Text -> Text -> Double
normalizeScore matchLength query choice
    | matchLength <= 0 = 0
    | otherwise =
          fromIntegral (T.length query)
          / fromIntegral matchLength       -- penalize longer match lengths
          / fromIntegral (T.length choice) -- penalize longer choice strings


-- TODO: matchlenghts could probably be a fold over the chars
-- instead of using tails
score :: Text -> Text -> Double
score q choice
    | T.null q      = 1
    | T.null choice = 0
    | otherwise = let minLength = minMatchLength q choice
                  in normalizeScore minLength q choice
  where
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


scoreAll :: Text -> Vector Text -> Vector (Text, Double)
scoreAll query choices =
    V.concat $
      parMap rdeepseq
             scoreVec
             (chunkVec (V.length choices `div` numCapabilities) choices)
  where
    scoreVec = V.map (\choice -> (choice, score lowerQuery (T.toLower choice)))
    lowerQuery = T.toLower query

chunkVec :: Int -> Vector a -> [Vector a]
chunkVec n v
  | n == 0   = []
  | V.null v = []
  | otherwise = firstChunk : chunkVec n rest
  where
    (firstChunk, rest) = V.splitAt n v
