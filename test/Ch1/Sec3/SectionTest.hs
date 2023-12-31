module Ch1.Sec3.SectionTest (sectionTests) where

import Ch1.Sec3.Definition76Test
import Ch1.Sec3.Example79Test
import Ch1.Sec3.Example82Test
import Ch1.Sec3.Example83Test
import Ch1.Sec3.Example84Test
import Ch1.Sec3.Exercise80Test
import Ch1.Sec3.Exercise85Test
import Ch1.Sec3.Proposition86Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch1.Sec3.SectionTest"
    [ Ch1.Sec3.Definition76Test.tests,
      Ch1.Sec3.Example79Test.tests,
      Ch1.Sec3.Exercise80Test.tests,
      Ch1.Sec3.Example82Test.tests,
      Ch1.Sec3.Example83Test.tests,
      Ch1.Sec3.Example84Test.tests,
      Ch1.Sec3.Exercise85Test.tests,
      Ch1.Sec3.Proposition86Test.tests
    ]
