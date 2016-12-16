module Grid exposing (..)


import Array exposing (Array)



gridWidth : Int
gridWidth = 10


gridHeight : Int
gridHeight = 22


nextLevel : Int
nextLevel = 2^gridWidth


type alias Row = Array Bool


emptyRow : Row
emptyRow = Array.repeat gridWidth False


isRowFull : Row -> Bool
isRowFull row =
  Array.foldr (&&) True row


type alias Grid = Array Row


emptyGrid : Grid
emptyGrid = Array.repeat gridHeight emptyRow


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

clearFullRows : Grid -> (Int, Grid)
clearFullRows grid =
  let
    remaining = Array.filter (not << isRowFull) grid
    numCleared = gridHeight - Array.length remaining
  in
    ( points numCleared
    , Array.append
        (Array.repeat numCleared emptyRow)
        remaining
    )


type alias Point = (Int, Int)


canAcceptPoint : Grid -> Point -> Bool
canAcceptPoint grid (i, j) =
  case Array.get i grid of
    Nothing ->
      False

    Just row ->
      case Array.get j row of
        Nothing ->
          False

        Just occupied ->
          not occupied


acceptPoint : Point -> Grid -> Grid
acceptPoint (i, j) grid =
  case Array.get i grid of
    Nothing ->
      grid

    Just row ->
      Array.set i (Array.set j True row) grid


type alias Piece =
  { origin : Point
  , offsets : List Point
  }


pieces : Array Piece
pieces =
  let
    piecesList =
      List.map
        (Piece (2, 4))
        [ [ (-2, 0), (-1, 0), (0, 0), (1, 0) ] -- I
        , [ (-1, 0), (0, 0), (1, 0), (1, 1) ] -- L
        , [ (-1, 1), (0, 1), (1, 1), (1, 0) ] -- J
        , [ (-1, 0), (-1, 1), (0, 0), (0, 1) ] -- O
        , [ (-1, 0), (0, 0), (0, 1), (1, 0) ] -- T
        , [ (-1, 0), (0, 0), (0, 1), (1, 1) ] -- S
        , [ (-1, 1), (0, 0), (0, 1), (1, 0) ] -- Z
        ]
  in
    Array.fromList piecesList


type Direction = Down | Left | Right


add : Point -> Point -> Point
add (i1, j1) (i2, j2) =
  (i1 + i2, j1 + j2)


movePiece : Direction -> Piece -> Piece
movePiece direction piece =
  let
    delta =
      case direction of
        Down ->
          (1, 0)

        Left ->
          (0, -1)

        Right ->
          (0, 1)
  in
    { piece | origin = add delta piece.origin }


dropPiece : Grid -> Piece -> Piece
dropPiece grid piece =
  let
    step = movePiece Down piece
  in
    if canAcceptPiece grid step then
      dropPiece grid step
    else
      piece


renderPiece : Piece -> List Point
renderPiece piece =
  List.map (add piece.origin) piece.offsets


canAcceptPiece : Grid -> Piece -> Bool
canAcceptPiece grid piece =
  List.all (canAcceptPoint grid) (renderPiece piece)


acceptPiece : Grid -> Piece -> Grid
acceptPiece grid piece =
  List.foldr acceptPoint grid (renderPiece piece)


type Rotation = CW | CCW


rotatePoint : Rotation -> Point -> Point
rotatePoint rotation (i, j) =
  case rotation of
    CW ->
      -- (1, 0) -> ( 0, 1)
      -- (0, 1) -> (-1, 0)
      (j, -i)

    CCW ->
      -- (1, 0) -> (0, -1)
      -- (0, 1) -> (1,  0)
      (-j, i)


rotatePiece : Rotation -> Piece -> Piece
rotatePiece rotation piece =
  { piece | offsets = List.map (rotatePoint rotation) piece.offsets }
