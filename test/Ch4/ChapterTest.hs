module Ch4.ChapterTest (chapterTests) where

import Ch4.Sec1.SectionTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch4.ChapterTest"
    [ Ch4.Sec1.SectionTest.sectionTests
    ]
