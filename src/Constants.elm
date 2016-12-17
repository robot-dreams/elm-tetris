module Constants exposing (..)


import Array exposing (Array)


import Types exposing (..)



gridWidth : Int
gridWidth = 10


gridHeight : Int
gridHeight = 22


emptyRow : Row
emptyRow = Array.repeat gridWidth Nothing


emptyGrid : Grid
emptyGrid = Array.repeat gridHeight emptyRow


nextLevel : Int
nextLevel = 2^gridWidth


points : Int -> Int
points numCleared =
  case numCleared of
    1 ->
      8

    2 ->
      20

    3 ->
      60

    4 ->
      240

    _ ->
      0


pieces : Array Piece
pieces =
  List.map2
    (Piece (2, 4))
    [ [ (-2, 0), (-1, 0), (0, 0), (1, 0) ] -- I
    , [ (-1, 1), ( 0, 1), (1, 1), (1, 0) ] -- J
    , [ (-1, 0), ( 0, 0), (1, 0), (1, 1) ] -- L
    , [ (-1, 0), (-1, 1), (0, 0), (0, 1) ] -- O
    , [ (-1, 0), ( 0, 0), (0, 1), (1, 1) ] -- S
    , [ (-1, 0), ( 0, 0), (0, 1), (1, 0) ] -- T
    , [ (-1, 1), ( 0, 0), (0, 1), (1, 0) ] -- Z
    ]
    [ "#3399CC"
    , "#CC33CC"
    , "#CC3366"
    , "#888888"
    , "#66CC33"
    , "#CC9900"
    , "#CC3333"
    ]
    |> Array.fromList
