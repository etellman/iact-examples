module Ch3.ChapterTest (chapterTests) where

import Ch3.Sec3.SectionTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch3.ChapterTest"
    [ Ch3.Sec3.SectionTest.sectionTests
    ]
