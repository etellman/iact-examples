module Ch4.Sec1.SectionTest (sectionTests) where

import Ch4.Sec1.Definition1Test
import Ch4.Sec1.Definition4Test
import Ch4.Sec1.Example7Test
import Ch4.Sec1.Example9Test
import Ch4.Sec1.Exercise2Test
import Ch4.Sec1.Exercise5Test
import Ch4.Sec1.Exercise8Test
import Ch4.Sec1.Exercise10Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch4.Sec1.SectionTest"
    [ Ch4.Sec1.Definition1Test.tests,
      Ch4.Sec1.Exercise2Test.tests,
      Ch4.Sec1.Definition4Test.tests,
      Ch4.Sec1.Exercise5Test.tests,
      Ch4.Sec1.Example7Test.tests,
      Ch4.Sec1.Exercise8Test.tests,
      Ch4.Sec1.Example9Test.tests,
      Ch4.Sec1.Exercise10Test.tests
    ]