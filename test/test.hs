import Ch1.ChapterTest
import Ch2.ChapterTest
import Ch3.ChapterTest
import Graph.GraphTest
import Lib.LibTest
import Monoid.MonoidTest
import Preorder.PreorderTest
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain
    ( testGroup
        "An Introduction to Applied Category Theory"
        [ Graph.GraphTest.graphTests,
          Lib.LibTest.libTests,
          Monoid.MonoidTest.monoidTests,
          Preorder.PreorderTest.tests,
          Ch1.ChapterTest.chapterTests,
          Ch2.ChapterTest.chapterTests,
          Ch3.ChapterTest.chapterTests
        ]
    )
