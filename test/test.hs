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
  describe "chunkVec" $ do
    it "concatting the chunks gives the original vec" $ do
      property $ \(list :: [Int]) n -> 
        V.concat (chunkVec n (V.fromList list)) == V.fromList list
