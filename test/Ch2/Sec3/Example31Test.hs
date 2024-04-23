module Ch2.Sec3.Example31Test (tests) where

import Ch2.Sec3.Example31
import Graph.Path (isPath)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Monoid.BooleanMonoids (PartialOrdAll (PartialOrdAll))
import qualified Properties.VCategoryProperties as VC
import Test.Tasty
import Data.Monoid (All(All))

genVertex :: Gen Vertex
genVertex = Gen.element vertices

toPartialOrdAll :: Vertex -> Vertex -> PartialOrdAll
toPartialOrdAll x y = PartialOrdAll . All $ isPath arrowsFrom x y

tests :: TestTree
tests =
  VC.vCategoryTests
    "Ch2.Sec3.Example31Test"
    genVertex
    toPartialOrdAll
