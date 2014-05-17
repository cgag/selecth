import qualified Data.Vector as V
import Criterion.Main
import Score

scoreBench :: V.Vector Char -> [V.Vector Char] -> Pure
scoreBench query = nf $ map (score query)

buildWord :: Int -> String -> V.Vector Char
buildWord baseReps base = V.fromList $ concat $ replicate baseReps base

buildChoices :: String -> Int -> Int -> [V.Vector Char]
buildChoices base baseReps totalChoices = 
    replicate totalChoices $ buildWord baseReps base 

main :: IO ()
main = do
    contents <- readFile "test/benchmark/words"
    let benchWords = map V.fromList $ lines contents 

    -- force eval
    print $ sum $ map V.length benchWords

    let cxs  =  buildChoices "x" 16 1000
    let cxys = buildChoices "xy" 16 1000


    defaultMain [
          bgroup "scoring" 
            [ bench "non-matching" $ scoreBench (buildWord 16 "y")  cxs
            , bench "matching exactly" $ scoreBench (buildWord 16 "x")  cxs
            , bench "matching broken up" $ scoreBench (buildWord 16 "x")  cxys
            , bench "overlapping matches" $ scoreBench (buildWord 16 "x")  cxs
            , bench "words, non-matching" $ scoreBench (buildWord 16 "x") benchWords
            , bench "words, matching"     $ scoreBench (V.fromList "ungovernableness") benchWords
            ]
         ]
