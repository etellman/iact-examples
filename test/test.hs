import Ch1.ChapterTest
import Ch2.ChapterTest
import Lib.LibTest
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain
    ( testGroup
        "An Introduction to Applied Category Theory"
        [ Lib.LibTest.libTests,
          Ch1.ChapterTest.chapterTests,
          Ch2.ChapterTest.chapterTests
        ]
    )
