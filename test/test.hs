{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.Hspec

import Data.List
import Data.Ord

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Score

main = do
  hspecTest <- testSpec "hspec" hspecSpec 
  defaultMain $ testGroup "Tests" [ hspecTest ]

hspecSpec = do
  describe "1 + 1 = 2" $ do
    it "whatever" $ do
      property $ \(list :: [Int]) n -> 
        V.concat (chunkVec n (V.fromList list)) == V.fromList list

    it "whatever2" $ do
      property $ \(query :: String) (choices :: [String]) ->
          let cs = V.fromList (map T.pack choices) in
          let q  = T.pack query in
          serialScoreAll q cs == scoreAll q cs
