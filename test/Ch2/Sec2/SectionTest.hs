module Ch2.Sec2.SectionTest (sectionTests) where

import Ch2.Sec2.Example12Test
import Ch2.Sec2.Example16Test
import Ch2.Sec2.Example3Test
import Ch2.Sec2.Exercise10Test
import Ch2.Sec2.Exercise13Test
import Ch2.Sec2.Exercise15Test
import Ch2.Sec2.Exercise17Test
import Ch2.Sec2.Exercise4Test
import Ch2.Sec2.Exercise6Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch2.Sec2.SectionTest"
    [ Ch2.Sec2.Example3Test.tests,
      Ch2.Sec2.Exercise4Test.tests,
      Ch2.Sec2.Exercise6Test.tests,
      Ch2.Sec2.Exercise10Test.tests,
      Ch2.Sec2.Example12Test.tests,
      Ch2.Sec2.Exercise13Test.tests,
      Ch2.Sec2.Exercise15Test.tests,
      Ch2.Sec2.Example16Test.tests,
      Ch2.Sec2.Exercise17Test.tests
    ]
