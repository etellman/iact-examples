module Ch2.Sec3.Exercise42Test (tests) where

import Ch2.Sec3.Exercise42
import qualified Ch2.Sec3.VCategoryProperties as VC
import Control.Monad (guard)
import Data.List (union)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Lib.Graph
import Test.Tasty
import Test.Tasty.Hedgehog

genCity :: Gen City
genCity = Gen.element cities

anyOf :: [Transports] -> Transports
anyOf = Transports . (foldr (\(Transports ts) total -> union ts total) [])

transportsBetween :: City -> City -> Transports
transportsBetween c1 c2 =
  let path = pathWith arrowsFrom anyOf c1 c2
   in case path of
        Just ts -> ts
        Nothing -> Transports []

hasTransport :: Transport -> Route -> Bool
hasTransport t a =
  let (Transports ts) = weight a
   in t `elem` ts

routeVia :: Transport -> City -> [Route]
routeVia t c = filter (hasTransport t) (arrowsFrom c)

prop_path :: Property
prop_path = property $ do
  -- set up
  c1 <- forAll genCity
  c2 <- forAll genCity

  -- exercise
  let (Transports ts) = transportsBetween c1 c2

  -- verify
  guard $ (not . null) ts
  t <- forAll $ Gen.element $ ts
  H.assert $ isPath (routeVia t) c1 c2

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec3.Exercise42Test"
    [ VC.tests genCity transportsBetween,
      testProperty "path" prop_path
    ]
