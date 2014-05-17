import System.IO
import Criterion.Main
import Score

scoreBench :: String -> [String] -> Pure
scoreBench query = nf $ map (score query)

buildWord :: String -> Int -> String
buildWord base baseReps = concat $ replicate baseReps base

buildChoices :: String -> Int -> Int -> [String]
buildChoices base baseReps totalChoices = 
    replicate totalChoices $ buildWord base baseReps

main :: IO ()
main = do
    contents <- openFile "test/benchmark/words" ReadMode >>= hGetContents 
    let benchWords = read contents :: [String]

    defaultMain [
          bgroup "scoring" 
            [ bench "non-matching" $ scoreBench (replicate 16 'y') $ buildChoices "x" 16 1000
            , bench "matching exactly" $ scoreBench (replicate 16 'x') $ buildChoices "x" 16 1000
            , bench "matching broken up" $ scoreBench (replicate 16 'x') $ buildChoices "xy" 16 1000
            , bench "overlapping matches" $ scoreBench (replicate 16 'x') $ buildChoices "x" 40 1000
            , bench "words, non-matching" $ scoreBench (replicate 16 'x') benchWords
            , bench "words, matching" $ scoreBench "ungovernableness" benchWords
            ]
         ]
