module Ch1.Sec1.SectionTest (sectionTests) where

import Ch1.Sec1.Example47Test
import Ch1.Sec1.Example48Test
import Ch1.Sec1.Exercise02Test
import Ch1.Sec1.Exercise03Test
import Ch1.Sec1.Exercise37Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch1.Sec1.SectionTest"
    [ Ch1.Sec1.Exercise02Test.tests,
      Ch1.Sec1.Exercise03Test.tests,
      Ch1.Sec1.Exercise37Test.tests,
      Ch1.Sec1.Example47Test.tests,
      Ch1.Sec1.Example48Test.tests
    ]
