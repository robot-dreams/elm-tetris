module Utils exposing (..)


import Array exposing (Array)


import Constants exposing (..)
import Types exposing (..)



isRowFull : Row -> Bool
isRowFull row =
  Array.foldr ((&&) << (/=) Nothing) True row


clearFullRows : Grid -> (Int, Grid)
clearFullRows grid =
  let
    remaining = Array.filter (not << isRowFull) grid
    numCleared = gridHeight - Array.length remaining
  in
    ( numCleared
    , Array.append
        (Array.repeat numCleared emptyRow)
        remaining
    )


canAcceptPoint : Grid -> Point -> Bool
canAcceptPoint grid (i, j) =
  case Array.get i grid of
    Nothing ->
      False

    Just row ->
      case Array.get j row of
        Nothing ->
          False

        Just cell ->
          cell == Nothing


acceptPoint : String -> Point -> Grid -> Grid
acceptPoint color (i, j) grid =
  case Array.get i grid of
    Nothing ->
      grid

    Just row ->
      Array.set i (Array.set j (Just color) row) grid


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
  List.foldr (acceptPoint piece.color) grid (renderPiece piece)


standardRotate : Rotation -> Point -> Point
standardRotate rotation (i, j) =
  case rotation of
    CW ->
      -- (1, 0) -> ( 0, 1)
      -- (0, 1) -> (-1, 0)
      (j, -i)

    CCW ->
      -- (1, 0) -> (0, -1)
      -- (0, 1) -> (1,  0)
      (-j, i)


rotatePoint : Center -> Rotation -> Point -> Point
rotatePoint center rotation (i, j) =
  case center of
    Cell ->
      standardRotate rotation (i, j)

    Vertex ->
      (i, j)
        |> expand 2
        |> add (1, -1)
        |> standardRotate rotation
        |> add (-1, 1)
        |> contract 2


rotatePiece : Rotation -> Piece -> Piece
rotatePiece rotation piece =
  { piece | offsets = List.map (rotatePoint piece.center rotation) piece.offsets }
