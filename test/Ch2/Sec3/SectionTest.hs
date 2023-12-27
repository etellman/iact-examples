module Ch2.Sec3.SectionTest (sectionTests) where

import Ch2.Sec3.Example31Test
import Ch2.Sec3.Example37Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch2.Sec3.SectionTest"
    [ Ch2.Sec3.Example31Test.tests,
      Ch2.Sec3.Example37Test.tests
    ]
