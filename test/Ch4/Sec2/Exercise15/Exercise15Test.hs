module Ch4.Sec2.Exercise15.Exercise15Test (exerciseTests) where

import Ch4.Sec2.Exercise15.XTest
import Ch4.Sec2.Exercise15.XYTest
import Ch4.Sec2.Exercise15.XZTest
import Ch4.Sec2.Exercise15.YTest
import Ch4.Sec2.Exercise15.ZTest
import Test.Tasty

exerciseTests :: TestTree
exerciseTests =
  testGroup
    "Ch4.Sec2.Exercise15.Exercise15Test"
    [ Ch4.Sec2.Exercise15.XTest.tests,
      Ch4.Sec2.Exercise15.YTest.tests,
      Ch4.Sec2.Exercise15.ZTest.tests,
      Ch4.Sec2.Exercise15.XYTest.tests,
      Ch4.Sec2.Exercise15.XZTest.tests
    ]
