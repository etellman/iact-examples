module Ch4.Sec2.Exercise13
  ( Tone (..),
    Cost (..),
    Value (..),
    Entertainment (..),
    feasible,
  )
where

import Data.Matrix
import Data.Monoid (All (..))
import Data.PartialOrd as PO
import Lib.VCategory (VCategory (..))
import Monoid.BooleanMonoids (PartialOrdAll (..))
import Preorder.Quantale
import Text.Printf

data Tone = MeanSpirited | GoodNatured deriving (Eq, Show)

toneIndex :: Tone -> Int
toneIndex MeanSpirited = 1
toneIndex GoodNatured = 2

toneEdges :: Matrix BoolWeight
toneEdges =
  fromLists $
    (fmap . fmap)
      BoolWeight
      [ [True, True],
        [False, True]
      ]

toneDistance :: Tone -> Tone -> BoolWeight
toneDistance = distanceFunc toneEdges toneIndex toneIndex

instance PartialOrd Tone where
  x <= y = let BoolWeight w = toneDistance x y in w

instance VCategory Tone PartialOrdAll where
  hom x x' = PartialOrdAll $ All (x PO.<= x')

data Entertainment = Boring | Funny deriving (Eq, Show)

entertainmentIndex :: Entertainment -> Int
entertainmentIndex Boring = 1
entertainmentIndex Funny = 2

entertainmentEdges :: Matrix BoolWeight
entertainmentEdges =
  fromLists $
    (fmap . fmap)
      BoolWeight
      [ [True, True],
        [False, True]
      ]

entertainmentDistance :: Entertainment -> Entertainment -> BoolWeight
entertainmentDistance = distanceFunc entertainmentEdges entertainmentIndex entertainmentIndex

instance PartialOrd Entertainment where
  x <= y = let BoolWeight w = entertainmentDistance x y in w

instance VCategory Entertainment PartialOrdAll where
  hom x x' = PartialOrdAll $ All (x PO.<= x')

data Value = Value !Tone !Entertainment deriving (Eq)

instance Show Value where
  show (Value t e) = printf "%s/%s" (show t) (show e)

valueDistances :: Matrix BoolWeight
valueDistances =
  let values = do
        t <- [MeanSpirited, GoodNatured]
        e <- [Boring, Funny]
        return $ Value t e
      weights = do
        (Value t e) <- values
        (Value t' e') <- values
        return $ toneDistance t t' <> entertainmentDistance e e'
   in fromList 4 4 weights

valueIndex :: Value -> Int
valueIndex (Value MeanSpirited Boring) = 1
valueIndex (Value MeanSpirited Funny) = 2
valueIndex (Value GoodNatured Boring) = 3
valueIndex (Value GoodNatured Funny) = 4

instance PartialOrd Value where
  x <= y =
    let BoolWeight w = getElem (valueIndex x) (valueIndex y) valueDistances
     in w

instance VCategory Value PartialOrdAll where
  hom x x' = PartialOrdAll $ All (x PO.<= x')

data Cost = Cost Int deriving (Eq, Show)

costIndex :: Cost -> Int
costIndex (Cost x) = x

costEdges :: Matrix BoolWeight
costEdges =
  fromLists $
    (fmap . fmap)
      BoolWeight
      [ [True, True, False],
        [False, True, True],
        [False, False, True]
      ]

costDistance :: Cost -> Cost -> BoolWeight
costDistance = distanceFunc costEdges costIndex costIndex

instance PartialOrd Cost where
  x <= y = let BoolWeight w = costDistance x y in w

instance VCategory Cost PartialOrdAll where
  hom x x' = PartialOrdAll $ All (x PO.<= x')

-- -- | is y reachable from x
feasible :: Value -> Cost -> Bool
feasible v c =
  let bridges =
        fromLists $
          (fmap . fmap)
            BoolWeight
            -- MS/B, MS/F, GN/B, GN/F
            [ [True, False, False],
              [False, False, True],
              [False, True, False],
              [False, False, False]
            ]
      ds = valueDistances `quantMult` bridges `quantMult` (distances costEdges)
      (BoolWeight d) = getElem (valueIndex v) (costIndex c) ds
   in d
