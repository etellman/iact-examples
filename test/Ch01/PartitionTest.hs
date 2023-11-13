module Ch01.PartitionTest (tests) where

import Ch01.Partition
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Ch01.PartitionTest"
    [ testCase "partition" $ do
        let xs = [1 .. 4] :: [Int]
        (length $ partitions xs) @?= 15,
      testGroup
        "distribute"
        [ testCase "non-empty" $
            distribute 'a' ["b", "c", "de"]
              @=? [["ab", "c", "de"], ["b", "ac", "de"], ["b", "c", "ade"], ["b", "c", "de", "a"]],
          testCase "empty" $ distribute 'a' [] @=? [["a"]],
          testCase "bind" $
            ([["bc", "e"], ["d"]] >>= distribute 'a')
              @?= [["abc", "e"], ["bc", "ae"], ["bc", "e", "a"], ["ad"], ["d", "a"]]
        ],
      testCase "isFiner" $
        do
          assertBool "finer 1" $ ["a", "b"] `isFiner` ["ab"]
          assertBool "finer 2" $ ["a", "bc"] `isFiner` ["abc"]
          assertBool "not finer" $ not $ ["a", "bc"] `isFiner` ["ab", "c"],
      testGroup
        "toPartition"
        [ testCase "empty" $ assertBool "[]" $ null $ toPartition odd ([] :: [Int]),
          testCase "non-empty" $ do
            let xs = [1 .. 5] :: [Int]
            toPartition odd xs @?= [[1, 3, 5], [2, 4]]
            toPartition (`rem` 3) xs @?= [[1, 4], [2, 5], [3]]
        ]
    ]
