module Ch3.ChapterTest (chapterTests) where

import Ch3.Sec3.SectionTest
import Ch3.Sec4.SectionTest
import Ch3.Sec5.SectionTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch3.ChapterTest"
    [ Ch3.Sec3.SectionTest.sectionTests,
      Ch3.Sec4.SectionTest.sectionTests,
      Ch3.Sec5.SectionTest.sectionTests
    ]
