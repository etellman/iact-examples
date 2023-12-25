module Ch1.Sec2.SectionTest (sectionTests) where

import Ch1.Sec2.Example21Test
import Ch1.Sec2.Example55Test
import Ch1.Sec2.Example57Test
import Ch1.Sec2.Exercise52Test
import Ch1.Sec2.Exercise61Test
import Ch1.Sec2.Exercise62Test
import Ch1.Sec2.Exercise64Test
import Ch1.Sec2.Exercise72Test
import Ch1.Sec2.Exercise74Test
import Ch1.Sec2.Proposition73Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch1.Sec2.SectionTest"
    [ Ch1.Sec2.Example21Test.tests,
      Ch1.Sec2.Exercise52Test.tests,
      Ch1.Sec2.Example55Test.tests,
      Ch1.Sec2.Example57Test.tests,
      Ch1.Sec2.Exercise61Test.tests,
      Ch1.Sec2.Exercise62Test.tests,
      Ch1.Sec2.Exercise64Test.tests,
      Ch1.Sec2.Exercise72Test.tests,
      Ch1.Sec2.Proposition73Test.tests,
      Ch1.Sec2.Exercise74Test.tests
    ]
