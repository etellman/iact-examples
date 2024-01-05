module Ch2.Sec3.Example31Test (tests) where

import Ch2.Sec2.BooleanMonoids (BooleanAnd (..))
import Ch2.Sec3.Example31
import qualified Ch2.Sec3.VCategoryProperties as VC
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Lib.Graph (isPath)
import Test.Tasty

genVertex :: Gen Vertex
genVertex = Gen.element vertices

toBooleanAnd :: Vertex -> Vertex -> BooleanAnd
toBooleanAnd x y = BooleanAnd $ isPath arrowsFrom x y

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec3.Example31Test"
    [ VC.tests genVertex toBooleanAnd
    ]
