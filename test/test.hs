import Ch01.ChapterTest
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain
    ( testGroup
        "An Introduction to Applied Category Theory"
        [ Ch01.ChapterTest.chapterTests
        ]
    )
