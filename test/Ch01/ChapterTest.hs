module Ch01.ChapterTest (chapterTests) where

import Ch01.BooleanSystemTest
import Ch01.PreservingTest
import Ch01.SetSystemTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch01.ChapterTest"
    [ Ch01.PreservingTest.tests,
      Ch01.SetSystemTest.tests,
      Ch01.BooleanSystemTest.tests
    ]
