module Ch3.Sec5.SectionTest (sectionTests) where

import Ch3.Sec5.Example65Test
import Ch3.Sec5.Example72Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch3.Sec5.SectionTest"
    [ Ch3.Sec5.Example65Test.tests,
      Ch3.Sec5.Example72Test.tests
    ]
