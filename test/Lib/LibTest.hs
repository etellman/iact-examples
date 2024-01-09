module Lib.LibTest (libTests) where

import Graph.PathTest
import Lib.PreorderTest
import Test.Tasty

libTests :: TestTree
libTests =
  testGroup
    "Lib.LibTest"
    [ Lib.PreorderTest.tests,
      Graph.PathTest.tests
    ]
