module Ch3.Sec4.SubSection1
  ( VertexGR (..),
    ArrowGR (..),
    VertexDDS (..),
    iArrow,
    fArrow,
    iVertex,
    fVertex,
  )
where

import Graph.Arrow
import Graph.IntWeight

-- GR --

data VertexGR = GrArrow | GrVertex deriving (Eq, Show)

data ArrowGR = ArrowGR !String !(VertexGR, VertexGR) deriving (Eq, Show)

-- DDS --

data VertexDDS = State deriving (Eq, Show)

data ArrowDDS = ArrowDDS !String !(VertexDDS, VertexDDS) deriving (Eq, Show)

instance Arrow ArrowDDS VertexDDS IntWeight where
  source (ArrowDDS _ (_, _)) = State
  target (ArrowDDS _ (_, _)) = State
  weight = const unitWeight

iVertex :: VertexDDS -> [Int]
iVertex State = [1 .. 7]

iArrow :: ArrowDDS -> Int -> Int
iArrow (ArrowDDS "id" (State, State)) state = state
iArrow (ArrowDDS "next" (State, State)) 1 = 4
iArrow (ArrowDDS "next" (State, State)) 2 = 4
iArrow (ArrowDDS "next" (State, State)) 3 = 5
iArrow (ArrowDDS "next" (State, State)) 4 = 5
iArrow (ArrowDDS "next" (State, State)) 5 = 5
iArrow (ArrowDDS "next" (State, State)) 6 = 7
iArrow (ArrowDDS "next" (State, State)) 7 = 6
iArrow _ _ = error "unexpected state"

fVertex :: VertexGR -> VertexDDS
fVertex _ = State

fArrow :: ArrowGR -> ArrowDDS
fArrow (ArrowGR "target" (GrArrow, GrVertex)) = ArrowDDS "next" (State, State)
fArrow (ArrowGR "source" (GrArrow, GrVertex)) = ArrowDDS "id" (State, State)
fArrow _ = error "unexpected arrow"

