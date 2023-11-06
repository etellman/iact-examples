module Ch01.ChapterTest (chapterTests) where

import Ch01.BooleanSystemTest
import Ch01.GraphTest
import Ch01.MonotoneMapTest
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
      Ch01.MonotoneMapTest.tests
    ]
