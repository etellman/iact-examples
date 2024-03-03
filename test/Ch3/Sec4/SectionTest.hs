module Ch3.Sec4.SectionTest (sectionTests) where

import Ch3.Sec4.DistributiveTest
import Ch3.Sec4.SubSection1Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch3.Sec4.SectionTest"
    [ Ch3.Sec4.SubSection1Test.tests,
      Ch3.Sec4.DistributiveTest.tests
    ]
