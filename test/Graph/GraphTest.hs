module Graph.GraphTest (graphTests) where

import Graph.PathTest
import Test.Tasty

graphTests :: TestTree
graphTests =
  testGroup
    "Graph.GraphTest"
    [ Graph.PathTest.tests
    ]
