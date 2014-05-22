{-# LANGUAGE OverloadedStrings #-}

import           Criterion.Main
import           Score
import           System.IO

import qualified Data.Text      as T
{-import qualified Data.Text.IO   as TI-}

scoreBench :: T.Text -> [T.Text] -> Pure
scoreBench query = nf $ scoreAll query

buildWord :: T.Text -> Int -> T.Text
buildWord base baseReps = T.replicate baseReps base

buildChoices :: T.Text -> Int -> Int -> [T.Text]
buildChoices base baseReps totalChoices =
    replicate totalChoices $ buildWord base baseReps

main :: IO ()
main = do
    contents <- openFile "test/benchmark/words" ReadMode >>= hGetContents
    let benchWords = map T.pack (read contents :: [String])
    let cxs  =  buildChoices "x"  16 2000
    let cxys =  buildChoices "xy" 16 2000

    -- force eval?
    print (last benchWords)

    defaultMain [
          bgroup "scoring"
            [ bench "non-matching" $ scoreBench (T.replicate 16 "y")  cxs
            , bench "matching exactly" $ scoreBench (T.replicate 16 "x")  cxs
            , bench "matching broken up" $ scoreBench (T.replicate 16 "x")  cxys
            , bench "overlapping matches" $ scoreBench (T.replicate 16 "x")  cxs
            , bench "words, non-matching" $ scoreBench (T.replicate 16 "x") benchWords
            , bench "words, matching" $ scoreBench "ungovernableness" benchWords
            ]
         ]
