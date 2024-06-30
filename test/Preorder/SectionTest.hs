module Preorder.SectionTest (sectionTests) where

import Preorder.PreorderTest
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Preorder.SectionTest"
    [ Preorder.PreorderTest.tests
    ]
