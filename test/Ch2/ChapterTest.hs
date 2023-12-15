module Ch2.ChapterTest (chapterTests) where

import Ch2.Sec2.SectionTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch2.ChapterTest"
    [ Ch2.Sec2.SectionTest.sectionTests
    ]
