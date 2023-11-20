module Ch01.Sec2.SectionTest (sectionTests) where

import Ch01.Sec2.Example21Test
import Ch01.Sec2.Example32Test
import Ch01.Sec2.Example55Test
import Ch01.Sec2.Example57Test
import Ch01.Sec2.Exercise52Test
import Ch01.Sec2.Exercise61Test
import Ch01.Sec2.Exercise62Test
import Ch01.Sec2.Exercise64Test
import Ch01.Sec2.Exercise72Test
import Ch01.Sec2.Exercise74Test
import Ch01.Sec2.Proposition73Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch01.Sec2.SectionTest"
    [ Ch01.Sec2.Example21Test.tests,
      Ch01.Sec2.Example32Test.tests,
      Ch01.Sec2.Exercise52Test.tests,
      Ch01.Sec2.Example55Test.tests,
      Ch01.Sec2.Example57Test.tests,
      Ch01.Sec2.Exercise61Test.tests,
      Ch01.Sec2.Exercise62Test.tests,
      Ch01.Sec2.Exercise64Test.tests,
      Ch01.Sec2.Exercise72Test.tests,
      Ch01.Sec2.Proposition73Test.tests,
      Ch01.Sec2.Exercise74Test.tests
    ]
