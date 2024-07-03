module Preorder.SectionTest (sectionTests) where

import Preorder.PreorderTest
import Preorder.QuantaleTest
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Preorder.SectionTest"
    [ Preorder.PreorderTest.tests,
      Preorder.QuantaleTest.tests
    ]
