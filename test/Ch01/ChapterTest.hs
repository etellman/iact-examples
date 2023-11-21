module Ch01.ChapterTest (chapterTests) where

import Ch01.BooleanSystemTest
import Ch01.GraphTest
import Ch01.PartitionTest
import Ch01.PreorderTest
import Ch01.PreservingTest
import Ch01.Sec1.SectionTest
import Ch01.Sec2.SectionTest
import Ch01.Sec3.SectionTest
import Ch01.SetSystemTest
import Ch01.SetTest
import Ch01.UpperSetTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch01.ChapterTest"
    [ Ch01.Sec1.SectionTest.sectionTests,
      Ch01.Sec2.SectionTest.sectionTests,
      Ch01.Sec3.SectionTest.sectionTests,
      Ch01.PreservingTest.tests,
      Ch01.SetSystemTest.tests,
      Ch01.BooleanSystemTest.tests,
      Ch01.SetTest.tests,
      Ch01.GraphTest.tests,
      Ch01.UpperSetTest.tests,
      Ch01.PreorderTest.tests,
      Ch01.PartitionTest.tests
    ]
