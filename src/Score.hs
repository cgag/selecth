{-# LANGUAGE ViewPatterns #-}

module Score
( Score(..)
, Match(..)
, score
, scoreAll
)
where

import           Control.Parallel.Strategies
import           Control.DeepSeq
import           Data.Text                   (Text)
import Control.Applicative ((<$>))
import Control.Arrow (first)
import qualified Data.Text                   as T
import Data.Function (on)
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V
import           GHC.Conc                    (numCapabilities)
import qualified Data.List as L


data Match = Match
    { m_startPos :: Int
    , m_endPos :: Int
    }

data Score = Score
    { s_score :: Double
    , s_match :: Match
    }

instance NFData Match where
  rnf (Match start end) = rnf start `seq` rnf end

instance NFData Score where
  rnf (Score s m) = rnf s `seq` rnf m


-- TODO: add tests

matchLength :: Match -> Int
matchLength m = m_endPos m - m_startPos m

normalizeScore :: Match -> Text -> Text -> Double
normalizeScore match query choice =
    fromIntegral (T.length query)
    / fromIntegral (matchLength match) -- penalize longer match lengths
    / fromIntegral (T.length choice)   -- penalize longer choice strings


-- TODO: matchlenghts could probably be a fold over the chars
-- instead of using tails
score :: Text -> Text -> Score
score q c
    | T.null q = Score 1 (Match 0 0)
    | T.null c = Score 0 (Match 0 0)
    | otherwise =
      case shortestMatch of
        Nothing -> Score 0 (Match 0 0)
        Just m  -> Score { s_score = normalizeScore m q c
                         , s_match = m
                         }
  where
    shortestMatch :: Maybe Match
    shortestMatch = L.minimumBy (compare `on` matchLength) <$> matches q c


matches :: Text -> Text -> Maybe [Match]
matches query choice = if null candidates
                       then Nothing
                       else Just candidates
  where
    candidates :: [Match]
    candidates =
       map (\(matchLen, startPos) -> Match startPos (startPos + matchLen))
       . filter (\(matchLen, _)  -> matchLen > 0)
       . map (first (getMatchLength query))
       $ starts query choice

    starts :: Text -> Text -> [(Text, Int)]
    starts q c = filter (\(t, _) -> T.head t == T.head q)
                 . filter (\(t, _) -> (not . T.null $ t))
                 $ zip (T.tails c) [0..]

    getMatchLength :: Text -> Text -> Int
    getMatchLength = go 0
     where
        go n q c
          | T.null q = n
          | T.null c = 0 -- no match
          | otherwise = if T.head q == T.head c
                        then go (n + 1) (T.drop 1 q) (T.drop 1 c)
                        else go (n + 1) q (T.drop 1 c)

scoreAll :: Text -> Vector Text -> Vector (Text, Score)
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
