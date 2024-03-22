module Ch3.Sec4.SectionTest (sectionTests) where

import Ch3.Sec4.AdjunctionExampleTest
import Ch3.Sec4.CurryingAdjunctionTest
import Ch3.Sec4.DistributiveTest
import Ch3.Sec4.RepresentableTest
import Ch3.Sec4.SubSection1Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch3.Sec4.SectionTest"
    [ Ch3.Sec4.SubSection1Test.tests,
      Ch3.Sec4.DistributiveTest.tests,
      Ch3.Sec4.RepresentableTest.tests,
      Ch3.Sec4.AdjunctionExampleTest.tests,
      Ch3.Sec4.CurryingAdjunctionTest.tests
    ]
