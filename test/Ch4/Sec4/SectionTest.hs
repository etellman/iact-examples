module Ch4.Sec4.SectionTest (sectionTests) where

import Ch4.Sec4.Exercise39Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch4.Sec4.SectionTest"
    [ Ch4.Sec4.Exercise39Test.tests
    ]
