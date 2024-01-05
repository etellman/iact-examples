module Ch2.Sec3.Exercise42Test
  ( tests,
  )
where

import Ch2.Sec3.Exercise42
import Data.List (union)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Lib.Graph
import Lib.Preorder as PO
import Test.Tasty
import Test.Tasty.Hedgehog

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

prop_identity ::
  (Show p, Monoid q, Preorder q) =>
  Gen p ->
  (p -> p -> q) ->
  Property
prop_identity gen hom = property $ do
  -- set up
  x <- forAll gen

  -- exercise and verify
  H.assert $ (hom x x) PO.<= mempty

prop_mplus ::
  (Show p, Monoid q, Preorder q) =>
  Gen p ->
  (p -> p -> q) ->
  Property
prop_mplus gen hom = property $ do
  -- set up
  x <- forAll gen
  y <- forAll gen
  z <- forAll gen

  -- exercise and verify
  H.assert $ ((hom x y) <> (hom y z)) PO.<= (hom x z)

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec3.Exercise42Test"
    [ testProperty "identity" $ prop_identity genCity cityToTransports,
      testProperty "monad operation" $ prop_mplus genCity cityToTransports
    ]
