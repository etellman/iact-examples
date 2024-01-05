module Ch2.Sec3.Exercise42Test (tests) where

import Ch2.Sec3.Exercise42
import qualified Ch2.Sec3.VCategoryProperties as VC
import Data.List (union)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Lib.Graph
import Test.Tasty

genCity :: Gen City
genCity = Gen.element cities

anyOf :: [Transports] -> Transports
anyOf = Transports . (foldr (\(Transports ts) total -> union ts total) [])

cityToTransports :: City -> City -> Transports
cityToTransports c1 c2 =
  let path = pathWith arrowsFrom anyOf weight c1 c2
   in case path of
        Just ts -> ts
        Nothing -> Transports []

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec3.Exercise42Test"
    [ VC.tests genCity cityToTransports
    ]
