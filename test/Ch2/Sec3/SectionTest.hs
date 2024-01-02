module Ch2.Sec3.SectionTest (sectionTests) where

import Ch2.Sec3.Example31Test
import Ch2.Sec3.Example37Test
import Ch2.Sec3.Exercise39Test
import Ch2.Sec3.Exercise41Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch2.Sec3.SectionTest"
    [ Ch2.Sec3.Example31Test.tests,
      Ch2.Sec3.Example37Test.tests,
      Ch2.Sec3.Exercise39Test.tests,
      Ch2.Sec3.Exercise41Test.tests
    ]
