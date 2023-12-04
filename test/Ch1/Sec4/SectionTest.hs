module Ch1.Sec4.SectionTest (sectionTests) where

import Ch1.Sec4.Example91Test
import Ch1.Sec4.Exercise92Test
import Ch1.Sec4.Exercise93Test
import Ch1.Sec4.Exercise97Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch1.Sec4.SectionTest"
    [ Ch1.Sec4.Example91Test.tests,
      Ch1.Sec4.Exercise92Test.tests,
      Ch1.Sec4.Exercise93Test.tests,
      Ch1.Sec4.Exercise97Test.tests
    ]