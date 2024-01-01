module Lib.LibTest (libTests) where

import Lib.GraphTest
import Lib.PreorderTest
import Test.Tasty

libTests :: TestTree
libTests =
  testGroup
    "Lib.LibTest"
    [ Lib.PreorderTest.tests,
      Lib.GraphTest.tests
    ]
