import Ch1.ChapterTest
import Ch2.ChapterTest
import Ch3.ChapterTest
import Ch4.ChapterTest
import Graph.GraphTest
import Monoid.MonoidTest
import Preorder.SectionTest
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain
    ( testGroup
        "An Introduction to Applied Category Theory"
        [ Graph.GraphTest.graphTests,
          Monoid.MonoidTest.monoidTests,
          Preorder.SectionTest.sectionTests,
          Ch1.ChapterTest.chapterTests,
          Ch2.ChapterTest.chapterTests,
          Ch3.ChapterTest.chapterTests,
          Ch4.ChapterTest.chapterTests
        ]
    )
