module Ch2.Sec2.Example26Test (tests) where

import Ch2.Sec2.MonoidalMapProperties
import Monoid.NaturalMonoids
import Monoid.RealMonoids
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty

genNatural :: Gen NaturalPlus
genNatural = NaturalPlus <$> Gen.int (Range.linear 0 1000)

genReal :: Gen RealPlus
genReal = RealPlus <$> Gen.realFloat (Range.exponentialFloat (-1000) 1000)

natToReal :: NaturalPlus -> RealPlus
natToReal (NaturalPlus x) = RealPlus (fromIntegral x)

realToNat :: RealPlus -> NaturalPlus
realToNat (RealPlus x) = NaturalPlus (floor x)

tests :: TestTree
tests =
  testGroup
    "Ch2.Sec2.Example26Test"
    [ testGroup
        "Natural -> Real"
        [ laxMonotoneMap genNatural natToReal,
          strongMonotoneMap genNatural natToReal,
          strictMonotoneMap genNatural natToReal
        ],
      testGroup
        "Real -> Natural"
        [ laxMonotoneMap genReal realToNat
        ]
    ]
