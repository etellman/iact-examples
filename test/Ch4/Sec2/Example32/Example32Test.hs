module Ch4.Sec2.Example32.Example32Test (exampleTests) where

import Ch4.Sec2.Example32.XTest
import Ch4.Sec2.Example32.XYTest
import Ch4.Sec2.Example32.CollageTest
import Ch4.Sec2.Example32.YTest
import Ch4.Sec2.Example32.ZTest
import Test.Tasty

exampleTests :: TestTree
exampleTests =
  testGroup
    "Ch4.Sec2.Example32.Example32Test"
    [ Ch4.Sec2.Example32.XTest.tests,
      Ch4.Sec2.Example32.YTest.tests,
      Ch4.Sec2.Example32.ZTest.tests,
      Ch4.Sec2.Example32.XYTest.tests,
      Ch4.Sec2.Example32.CollageTest.tests
    ]
