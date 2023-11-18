module Ch01.Sec1.SectionTest (sectionTests) where

import Ch01.Sec1.Example47Test
import Ch01.Sec1.Example48Test
import Ch01.Sec1.Exercise02Test
import Ch01.Sec1.Exercise03Test
import Ch01.Sec1.Exercise37Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch01.Sec1.SectionTest"
    [ Ch01.Sec1.Exercise02Test.tests,
      Ch01.Sec1.Exercise03Test.tests,
      Ch01.Sec1.Exercise37Test.tests,
      Ch01.Sec1.Example47Test.tests,
      Ch01.Sec1.Example48Test.tests
    ]
