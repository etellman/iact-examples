module Ch01.Sec3.SectionTest (sectionTests) where

import Ch01.Sec3.Definition76Test
import Ch01.Sec3.Example79Test
import Ch01.Sec3.Exercise80Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch01.Sec3.SectionTest"
    [ Ch01.Sec3.Definition76Test.tests,
      Ch01.Sec3.Example79Test.tests,
      Ch01.Sec3.Exercise80Test.tests
    ]
