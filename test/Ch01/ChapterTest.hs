module Ch01.ChapterTest (chapterTests) where

import Ch01.BooleanSystemTest
import Ch01.Exercise61Test
import Ch01.Exercise62Test
import Ch01.Exercise64Test
import Ch01.Exercise72Test
import Ch01.GraphTest
import Ch01.MonotoneMapTest
import Ch01.PartitionTest
import Ch01.PreorderTest
import Ch01.PreservingTest
import Ch01.SetSystemTest
import Ch01.SetTest
import Ch01.UpperSetTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch01.ChapterTest"
    [ Ch01.PreservingTest.tests,
      Ch01.SetSystemTest.tests,
      Ch01.BooleanSystemTest.tests,
      Ch01.SetTest.tests,
      Ch01.GraphTest.tests,
      Ch01.UpperSetTest.tests,
      Ch01.PreorderTest.tests,
      Ch01.MonotoneMapTest.tests,
      Ch01.Exercise61Test.tests,
      Ch01.Exercise62Test.tests,
      Ch01.Exercise64Test.tests,
      Ch01.Exercise72Test.tests,
      Ch01.PartitionTest.tests
    ]
