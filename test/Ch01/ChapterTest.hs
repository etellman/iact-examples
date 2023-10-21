module Ch01.ChapterTest (chapterTests) where

import Ch01.PreservingTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch01.ChapterTest"
    [ Ch01.PreservingTest.tests
    ]
