{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import qualified Data.Vector as V

import Score

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "chunkvec works" $
      \(list :: [Int]) n -> 
          n >= 1 QC.==>
            V.concat (chunkVec n (V.fromList list)) == V.fromList list
  -- the following property does not hold
  -- , QC.testProperty "Fermat's last theorem" $
  --     \x y z n ->
  --       (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  ]
