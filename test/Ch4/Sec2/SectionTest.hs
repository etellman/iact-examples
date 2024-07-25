module Ch4.Sec2.SectionTest (sectionTests) where

import Ch4.Sec2.Definition1Test
import Ch4.Sec2.Definition4Test
import Ch4.Sec2.Example7Test
import Ch4.Sec2.Example9Test
import Ch4.Sec2.Exercise10Test
import Ch4.Sec2.Exercise13Test
import Ch4.Sec2.Exercise15.Exercise15Test
import Ch4.Sec2.Exercise2Test
import Ch4.Sec2.Exercise5Test
import Ch4.Sec2.Exercise8Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch4.Sec2.SectionTest"
    [ Ch4.Sec2.Definition1Test.tests,
      Ch4.Sec2.Exercise2Test.tests,
      Ch4.Sec2.Definition4Test.tests,
      Ch4.Sec2.Exercise5Test.tests,
      Ch4.Sec2.Example7Test.tests,
      Ch4.Sec2.Exercise8Test.tests,
      Ch4.Sec2.Example9Test.tests,
      Ch4.Sec2.Exercise10Test.tests,
      Ch4.Sec2.Exercise13Test.tests,
      Ch4.Sec2.Exercise15.Exercise15Test.exerciseTests
    ]
