module Ch4.Sec1.SectionTest (sectionTests) where

import Ch4.Sec1.Definition1Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch4.Sec1.SectionTest"
    [ Ch4.Sec1.Definition1Test.tests
    ]
