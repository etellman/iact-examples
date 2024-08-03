module Ch4.ChapterTest (chapterTests) where

import Ch4.Sec2.SectionTest
import Ch4.Sec3.SectionTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch4.ChapterTest"
    [ Ch4.Sec2.SectionTest.sectionTests,
      Ch4.Sec3.SectionTest.sectionTests
    ]
