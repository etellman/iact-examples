module Ch1.Sec4.SectionTest (sectionTests) where

import Ch1.Sec4.Example109Test
import Ch1.Sec4.Example91Test
import Ch1.Sec4.Example96Test
import Ch1.Sec4.Exercise100Test
import Ch1.Sec4.Exercise111Test
import Ch1.Sec4.Exercise117Test
import Ch1.Sec4.Exercise92Test
import Ch1.Sec4.Exercise93Test
import Ch1.Sec4.Exercise97Test
import Ch1.Sec4.Proposition101Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch1.Sec4.SectionTest"
    [ Ch1.Sec4.Example91Test.tests,
      Ch1.Sec4.Exercise92Test.tests,
      Ch1.Sec4.Exercise93Test.tests,
      Ch1.Sec4.Example96Test.tests,
      Ch1.Sec4.Exercise97Test.tests,
      Ch1.Sec4.Exercise100Test.tests,
      Ch1.Sec4.Proposition101Test.tests,
      Ch1.Sec4.Example109Test.tests,
      Ch1.Sec4.Exercise111Test.tests,
      Ch1.Sec4.Exercise117Test.tests
    ]
