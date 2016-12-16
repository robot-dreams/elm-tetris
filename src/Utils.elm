module Utils exposing (..)


import Array exposing (Array)


import Constants exposing (..)
import Types exposing (..)



toBinaryRow : Int -> Row
toBinaryRow score =
  let
    toBinaryList rest =
      if rest == 0 then
        []
      else if rest % 2 == 0 then
        False :: toBinaryList (rest // 2)
      else
        True :: toBinaryList (rest // 2)

    raw =
      toBinaryList score
        |> List.reverse
        |> Array.fromList
    padding =
      Array.repeat (gridWidth - (Array.length raw)) False
  in
    Array.append padding raw


isRowFull : Row -> Bool
isRowFull row =
  Array.foldr (&&) True row


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
