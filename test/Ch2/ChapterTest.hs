module Ch2.ChapterTest (chapterTests) where

import Ch2.Sec2.SectionTest
import Ch2.Sec3.SectionTest
import Ch2.Sec4.SectionTest
import Ch2.Sec5.SectionTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch2.ChapterTest"
    [ Ch2.Sec2.SectionTest.sectionTests,
      Ch2.Sec3.SectionTest.sectionTests,
      Ch2.Sec4.SectionTest.sectionTests,
      Ch2.Sec5.SectionTest.sectionTests
    ]
