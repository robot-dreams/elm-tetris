module Types exposing (..)


import Array exposing (Array)



type alias Cell = Maybe String


type alias Row = Array Cell


type alias Grid = Array Row


type alias Point = (Int, Int)


expand : Int -> Point -> Point
expand c (i, j) =
  (c * i, c * j)


contract : Int -> Point -> Point
contract c (i, j) =
  (i // c, j // c)


add : Point -> Point -> Point
add (i1, j1) (i2, j2) =
  (i1 + i2, j1 + j2)


type Center
  = Cell
  | Vertex


type alias Piece =
  { origin : Point
  , offsets : List Point
  , center : Center
  , color : String
  }


type Direction = Down | Left | Right


type Rotation = CW | CCW
