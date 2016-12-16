module Types exposing (..)


import Array exposing (Array)



type alias Row = Array Bool


type alias Grid = Array Row


type alias Point = (Int, Int)


add : Point -> Point -> Point
add (i1, j1) (i2, j2) =
  (i1 + i2, j1 + j2)


type alias Piece =
  { origin : Point
  , offsets : List Point
  }


type Direction = Down | Left | Right


type Rotation = CW | CCW
