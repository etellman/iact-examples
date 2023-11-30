module Ch1.Sec4.Exercise93Test (tests) where

import Control.Monad (guard)
import Test.Tasty
import Test.Tasty.HUnit

newtype P = P Int deriving (Show, Eq, Ord)

newtype Q = Q Int deriving (Show, Eq, Ord)

checkAdjunct :: (P -> Q) -> (Q -> P) -> [(P, Q)]
checkAdjunct f g = do
  p <- fmap P [1 .. 3]
  q <- fmap Q [1 .. 3]

  guard $ (f p <= q) /= (p <= g q)

  return (p, q)

tests :: TestTree
tests =
  testGroup
    "Ch1.Sec4.Exercise93Test"
    [ testCase "part 1" $ do
        let f (P 2) = Q 1
            f (P x) = Q x
            g (Q 1) = P 2
            g (Q x) = P x

        checkAdjunct f g @?= [],
      testCase "part 2" $ do
        let f (P x) = Q x
            g (Q 1) = P 2
            g (Q x) = P x

        checkAdjunct f g @?= [(P 2, Q 1)]
    ]
