module Ch4.Sec3.SectionTest (sectionTests) where

import Ch4.Sec3.Example32.Example32Test
import Ch4.Sec3.Exercise15.Exercise15Test
import Ch4.Sec3.Exercise28Test
import Ch4.Sec3.Exercise33Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch4.Sec3.SectionTest"
    [ Ch4.Sec3.Exercise15.Exercise15Test.exerciseTests,
      Ch4.Sec3.Exercise28Test.tests,
      Ch4.Sec3.Example32.Example32Test.exampleTests,
      Ch4.Sec3.Exercise33Test.tests
    ]
