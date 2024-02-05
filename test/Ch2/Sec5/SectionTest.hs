module Ch2.Sec5.SectionTest (sectionTests) where

import Ch2.Sec5.Example60Test
import Ch2.Sec5.Exercise59Test
import Ch2.Sec5.Exercise61Test
import Ch2.Sec5.Proposition64Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch2.Sec5.SectionTest"
    [ Ch2.Sec5.Exercise59Test.tests,
      Ch2.Sec5.Example60Test.tests,
      Ch2.Sec5.Exercise61Test.tests,
      Ch2.Sec5.Proposition64Test.tests
    ]
