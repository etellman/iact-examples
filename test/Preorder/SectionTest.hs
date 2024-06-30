module Preorder.SectionTest (sectionTests) where

import Preorder.MatrixTest
import Preorder.PreorderTest
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Preorder.SectionTest"
    [ Preorder.PreorderTest.tests,
      Preorder.MatrixTest.tests
    ]
