module Ch2.Sec4.SectionTest (sectionTests) where

import Ch2.Sec4.Example45Test
import Ch2.Sec4.Example55Test
import Test.Tasty

sectionTests :: TestTree
sectionTests =
  testGroup
    "Ch2.Sec4.SectionTest"
    [ Ch2.Sec4.Example45Test.tests,
      Ch2.Sec4.Example55Test.tests
    ]
