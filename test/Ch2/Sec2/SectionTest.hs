module Ch2.Sec2.SectionTest (sectionTests) where

import Ch2.Sec2.Example3Test
import Ch2.Sec2.Exercise4Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch2.Sec2.SectionTest"
    [ Ch2.Sec2.Example3Test.tests,
      Ch2.Sec2.Exercise4Test.tests
    ]
