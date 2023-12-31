module Ch1.ChapterTest (chapterTests) where

import Ch1.BooleanSystemTest
import Ch1.PartitionTest
import Ch1.PreservingTest
import Ch1.Sec1.SectionTest
import Ch1.Sec2.SectionTest
import Ch1.Sec3.SectionTest
import Ch1.Sec4.SectionTest
import Ch1.SetSystemTest
import Ch1.SetTest
import Ch1.UpperSetTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch1.ChapterTest"
    [ Ch1.Sec1.SectionTest.sectionTests,
      Ch1.Sec2.SectionTest.sectionTests,
      Ch1.Sec3.SectionTest.sectionTests,
      Ch1.Sec4.SectionTest.sectionTests,
      Ch1.PreservingTest.tests,
      Ch1.SetSystemTest.tests,
      Ch1.BooleanSystemTest.tests,
      Ch1.SetTest.tests,
      Ch1.UpperSetTest.tests,
      Ch1.PartitionTest.tests
    ]
