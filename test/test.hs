import Ch1.ChapterTest
import Lib.LibTest
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain
    ( testGroup
        "An Introduction to Applied Category Theory"
        [ Ch1.ChapterTest.chapterTests,
          Lib.LibTest.libTests
        ]
    )
