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


-- All following lists correspond to tetrominoes in the order I, J, L, O, S, T, Z


-- The J tetromino needs be centered slightly differently when it first enters
-- the board.
initialPositions : List Point
initialPositions =
  [ (2, 4)
  , (2, 5)
  , (2, 4)
  , (2, 4)
  , (2, 4)
  , (2, 4)
  , (2, 4)
  ]


offsets : List (List Point)
offsets =
  [ [ (-2, 0), (-1, 0), (0, 0), (1,  0) ]
  , [ (-1, 0), ( 0, 0), (1, 0), (1, -1) ]
  , [ (-1, 0), ( 0, 0), (1, 0), (1,  1) ]
  , [ (-1, 0), (-1, 1), (0, 0), (0,  1) ]
  , [ (-1, 0), ( 0, 0), (0, 1), (1,  1) ]
  , [ (-1, 0), ( 0, 0), (0, 1), (1,  0) ]
  , [ (-1, 1), ( 0, 0), (0, 1), (1,  0) ]
  ]


-- I and O have slightly different rotation behavior from the other tetrominoes:
-- their axis of rotation is on the intersection between two grid lines, as
-- opposed to the middle of a grid cell.
rotationAxes : List Axis
rotationAxes =
  [ Vertex
  , Cell
  , Cell
  , Vertex
  , Cell
  , Cell
  , Cell
  ]


colors : List String
colors =
  [ "#3399CC"
  , "#9933CC"
  , "#CC3399"
  , "#888888"
  , "#66CC33"
  , "#CC9900"
  , "#CC3333"
  ]


pieces : Array Piece
pieces =
  List.map4 Piece initialPositions offsets rotationAxes colors
    |> Array.fromList
