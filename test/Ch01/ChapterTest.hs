module Ch01.ChapterTest (chapterTests) where

import Ch01.AndMonoidTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch01.ChapterTest"
    [ Ch01.AndMonoidTest.tests
    ]
