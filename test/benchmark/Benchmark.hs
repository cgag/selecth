import qualified Data.Vector as V
import Criterion.Main
import Score

scoreBench :: V.Vector Char -> [V.Vector Char] -> Pure
scoreBench query = nf $ map (score query)

buildWord :: String -> Int -> V.Vector Char
buildWord base baseReps = V.fromList $ concat $ replicate baseReps base

buildChoices :: String -> Int -> Int -> [V.Vector Char]
buildChoices base baseReps totalChoices = 
    replicate totalChoices $ buildWord base baseReps

main :: IO ()
main = do
    contents <- readFile "test/benchmark/words"
    let benchWords = map V.fromList $! lines contents 

    defaultMain [
          bgroup "scoring" 
            [ bench "non-matching"        $ scoreBench (buildWord "y" 16) $ buildChoices "x" 16 1000
            , bench "matching exactly"    $ scoreBench (buildWord "x" 16) $ buildChoices "x" 16 1000
            , bench "matching broken up"  $ scoreBench (buildWord "x" 16) $ buildChoices "xy" 16 1000
            , bench "overlapping matches" $ scoreBench (buildWord "x" 16) $ buildChoices "x" 40 1000
            , bench "words, non-matching" $ scoreBench (buildWord "x" 16) benchWords
            , bench "words, matching"     $ scoreBench (V.fromList "ungovernableness") benchWords
            ]
         ]
