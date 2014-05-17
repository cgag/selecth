{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Criterion.Main
import Score

scoreBench :: T.Text -> [T.Text] -> Pure
scoreBench query = nf $ map (score query)

buildWord :: T.Text -> Int -> T.Text
buildWord base baseReps = T.replicate baseReps base

buildChoices :: T.Text -> Int -> Int -> [T.Text]
buildChoices base baseReps totalChoices = 
    replicate totalChoices $ buildWord base baseReps

main :: IO ()
main = do
    contents <- TI.readFile "test/benchmark/words"
    let benchWords = T.lines contents 

    defaultMain [
          bgroup "scoring" 
            [ bench "non-matching" $ scoreBench (T.replicate 16 "y") $ buildChoices "x" 16 1000
            , bench "matching exactly" $ scoreBench (T.replicate 16 "x") $ buildChoices "x" 16 1000
            , bench "matching broken up" $ scoreBench (T.replicate 16 "x") $ buildChoices "xy" 16 1000
            , bench "overlapping matches" $ scoreBench (T.replicate 16 "x") $ buildChoices "x" 40 1000
            , bench "words, non-matching" $ scoreBench (T.replicate 16 "x") benchWords
            , bench "words, matching" $ scoreBench "ungovernableness" benchWords
            ]
         ]
