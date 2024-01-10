module Ch2.Sec5.SectionTest (sectionTests) where

import Ch2.Sec5.Example55Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch2.Sec5.SectionTest"
    [ Ch2.Sec5.Example55Test.tests
    ]
