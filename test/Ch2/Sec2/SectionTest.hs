module Ch2.Sec2.SectionTest (sectionTests) where

import Ch2.Sec2.Example3Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch2.Sec2.SectionTest"
    [ Ch2.Sec2.Example3Test.tests
    ]
