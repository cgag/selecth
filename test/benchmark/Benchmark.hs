import System.IO
import Criterion.Main
import Score

scoreBench :: String -> [String] -> Pure
scoreBench query = nf $ scoreAll query

buildWord :: String -> Int -> String
buildWord base baseReps = concat $ replicate baseReps base

buildChoices :: String -> Int -> Int -> [String]
buildChoices base baseReps totalChoices = 
    replicate totalChoices $ buildWord base baseReps

main :: IO ()
main = do
    contents <- openFile "test/benchmark/words" ReadMode >>= hGetContents 
    let benchWords = read contents :: [String]
    let cxs  =  buildChoices "x"  16 2000
    let cxys =  buildChoices "xy" 16 2000

    -- force eval?
    putStrLn (last benchWords)

    defaultMain [
          bgroup "scoring" 
            [ bench "non-matching" $ scoreBench (replicate 16 'y')  cxs
            , bench "matching exactly" $ scoreBench (replicate 16 'x')  cxs
            , bench "matching broken up" $ scoreBench (replicate 16 'x')  cxys
            , bench "overlapping matches" $ scoreBench (replicate 16 'x')  cxs
            , bench "words, non-matching" $ scoreBench (replicate 16 'x') benchWords
            , bench "words, matching" $ scoreBench "ungovernableness" benchWords
            ]
         ]
