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


linesPerLevel : Int
linesPerLevel = 10


points : Int -> Int
points numCleared =
  case numCleared of
    1 ->
      40

    2 ->
      100

    3 ->
      300

    4 ->
      1200

    _ ->
      0


pieces : Array Piece
pieces =
  List.map4
    Piece
    [ (2, 4)
    , (2, 5)
    , (2, 4)
    , (2, 4)
    , (2, 4)
    , (2, 4)
    , (2, 4)
    ]
    [ [ (-2, 0), (-1, 0), (0, 0), (1,  0) ] -- I
    , [ (-1, 0), ( 0, 0), (1, 0), (1, -1) ] -- J
    , [ (-1, 0), ( 0, 0), (1, 0), (1,  1) ] -- L
    , [ (-1, 0), (-1, 1), (0, 0), (0,  1) ] -- O
    , [ (-1, 0), ( 0, 0), (0, 1), (1,  1) ] -- S
    , [ (-1, 0), ( 0, 0), (0, 1), (1,  0) ] -- T
    , [ (-1, 1), ( 0, 0), (0, 1), (1,  0) ] -- Z
    ]
    [ Vertex
    , Cell
    , Cell
    , Vertex
    , Cell
    , Cell
    , Cell
    ]
    [ "#3399CC"
    , "#9933CC"
    , "#CC3399"
    , "#888888"
    , "#66CC33"
    , "#CC9900"
    , "#CC3333"
    ]
    |> Array.fromList
