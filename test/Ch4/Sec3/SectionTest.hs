module Ch4.Sec3.SectionTest (sectionTests) where

import Ch4.Sec3.Exercise33Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch4.Sec3.SectionTest"
    [ Ch4.Sec3.Exercise33Test.tests
    ]
