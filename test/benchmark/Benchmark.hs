{-# LANGUAGE OverloadedStrings #-}

import           Criterion.Main
import           Score
import           System.IO

import qualified Data.Text      as T
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V

{-import qualified Data.Text.IO   as TI-}

scoreBench :: Text -> Vector Text -> Benchmarkable
scoreBench query = nf $ scoreAll query

buildWord :: Text -> Int -> Text
buildWord base baseReps = T.replicate baseReps base

buildChoices :: Text -> Int -> Int -> Vector Text
buildChoices base baseReps totalChoices =
    V.replicate totalChoices $ buildWord base baseReps

main :: IO ()
main = do
    contents <- openFile "test/benchmark/words" ReadMode >>= hGetContents
    let benchWords = V.fromList (map T.pack (read contents :: [String]))
    let cxs  =  buildChoices "x"  16 2000
    let cxys =  buildChoices "xy" 16 2000

    -- force eval?
    print (V.last benchWords)

    defaultMain [
          bgroup "scoring"
            [ bench "non-matching"        $ scoreBench (T.replicate 16 "y")  cxs
            , bench "matching exactly"    $ scoreBench (T.replicate 16 "x")  cxs
            , bench "matching broken up"  $ scoreBench (T.replicate 16 "x")  cxys
            , bench "overlapping matches" $ scoreBench (T.replicate 16 "x")  cxs
            , bench "words, non-matching" $ scoreBench (T.replicate 16 "x") benchWords
            , bench "words, matching"     $ scoreBench "ungovernableness" benchWords
            ]
         ]
