module Ch3.Sec3.SectionTest (sectionTests) where

import Ch3.Sec3.Definition41Test
import Ch3.Sec3.Example39Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch3.Sec3.SectionTest"
    [ Ch3.Sec3.Example39Test.tests,
      Ch3.Sec3.Definition41Test.tests
    ]
