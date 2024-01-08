module Ch2.Sec4.Example55
  ( XVertex (..),
    XArrow (..),
    xarrows,
    xvertices,
    YVertex (..),
    YArrow (..),
    yarrows,
    yvertices,
    XYVertex (..),
    XYArrow (..),
    xyarrows,
  )
where

import Lib.Graph

data XVertex = A | B | C deriving (Eq, Show)

data XArrow = XArrow
  { xsource :: XVertex,
    xtarget :: XVertex,
    xweight :: Int
  }

instance Arrow XArrow XVertex IntWeight where
  source = xsource
  target = xtarget
  weight = toIntWeight . xweight

xvertices :: [XVertex]
xvertices = [A, B, C]

xarrows :: XVertex -> [XArrow]
xarrows A = [XArrow A B 2]
xarrows B = [XArrow B C 3]
xarrows C = []

data YVertex = P | Q deriving (Eq, Show)

data YArrow = YArrow
  { ysource :: YVertex,
    ytarget :: YVertex,
    yweight :: Int
  }

instance Arrow YArrow YVertex IntWeight where
  source = ysource
  target = ytarget
  weight = toIntWeight . yweight

yvertices :: [YVertex]
yvertices = [P, Q]

yarrows :: YVertex -> [YArrow]
yarrows P = [YArrow P Q 5]
yarrows Q = [YArrow Q P 8]

newtype XYVertex = XYVertex (XVertex, YVertex) deriving (Eq, Show)

data XYArrow = XYArrow
  { xysource :: XYVertex,
    xytarget :: XYVertex,
    xyweight :: Int
  }
  deriving (Eq, Show)

xyarrows :: XYVertex -> [XYArrow]
xyarrows (XYVertex (x, y)) =
  let fromx = do
        xa <- xarrows x
        return $
          XYArrow
            (XYVertex $ ((xsource xa), y))
            (XYVertex $ ((xtarget xa), y))
            (xweight xa)
      fromy = do
        ya <- yarrows y
        return $
          XYArrow
            (XYVertex $ (x, (ysource ya)))
            (XYVertex $ (x, (ytarget ya)))
            (yweight ya)
   in fromx ++ fromy
