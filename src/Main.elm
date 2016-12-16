import Array exposing (Array)
import Html exposing (br, div, Html, program, text)
import Keyboard exposing (KeyCode)
import Random
import Svg exposing (rect, svg)
import Svg.Attributes exposing (fill, height, viewBox, width, x, y)
import Task
import Time exposing (millisecond)


import Grid exposing (..)



main = program
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }



-- MODEL



-- Invariants;
-- case model.piece of
--   Nothing ->
--     True
--   Just piece ->
--     canAcceptPiece model.grid model.piece
type alias Model =
  { piece : Maybe Piece
  , grid : Grid
  , level : Int
  , score : Int
  , gameOver : Bool
  }


init : (Model, Cmd Msg)
init =
  ( { piece = Nothing
    , grid = emptyGrid
    , score = 0
    , level = 0
    , gameOver = False
    }
  , getRandomPiece
  )



-- UPDATE


type Msg
  = NoOp
  | Rotate Rotation
  | Move Direction
  | Fall
  | Drop
  | RandomPiece
  | AddPiece (Maybe Piece)


type Kind
  = Regular
  | Sticky (Cmd Msg)
  | Snap (Cmd Msg)


tryTransform : Kind -> (Piece -> Piece) -> Model -> (Model, Cmd Msg)
tryTransform kind transform model =
  case model.piece of
    Nothing ->
      (model, Cmd.none)

    Just piece ->
      let
        transformed = transform piece
      in
        if canAcceptPiece model.grid transformed then
          case kind of
            Snap cmd ->
              let
                (scoreDelta, cleared) =
                  acceptPiece model.grid transformed |> clearFullRows
                score = model.score + scoreDelta
              in
                if score >= nextLevel then
                  ( { model
                      | grid = cleared
                      , piece = Nothing
                      , level = model.level + 1
                      , score = 0
                    }
                  , cmd
                  )
                else
                  ( { model
                      | grid = cleared
                      , piece = Nothing
                      , score = model.score + scoreDelta
                    }
                  , cmd
                  )

            _ ->
              ( { model | piece = Just transformed }
              , Cmd.none
              )
        else
          case kind of
            Sticky cmd ->
              let
                (scoreDelta, cleared) =
                  acceptPiece model.grid piece |> clearFullRows
              in
                ( { model
                    | grid = cleared
                    , piece = Nothing
                    , score = model.score + scoreDelta
                  }
                , cmd
                )

            _ ->
              (model, Cmd.none)


getRandomPiece : Cmd Msg
getRandomPiece =
  let
    index = Random.int 0 ((Array.length pieces) - 1)
    maybePiece = Random.map (\i -> Array.get i pieces) index
  in
    Random.generate AddPiece maybePiece


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)

    Rotate rotation ->
      tryTransform Regular (rotatePiece rotation) model

    Move direction ->
      tryTransform Regular (movePiece direction) model

    Fall ->
      tryTransform (Sticky getRandomPiece) (movePiece Down) model

    Drop ->
      tryTransform (Snap getRandomPiece) (dropPiece model.grid) model

    RandomPiece ->
      (model, getRandomPiece)

    AddPiece maybePiece ->
      case maybePiece of
        -- This case should never happen.
        Nothing ->
          (model, Cmd.none)

        Just piece ->
          if canAcceptPiece model.grid piece then
            ( { model | piece = Just piece }
            , Cmd.none
            )
          else
            ( { model | gameOver = True }
            , Cmd.none
            )



-- SUBSCRIPTIONS


handleKeyboardInput : KeyCode -> Msg
handleKeyboardInput keyCode =
  case keyCode of
    32 -> -- spacebar
      Drop

    38 -> -- up
      Rotate CW

    40 -> -- down
      Fall

    37 -> -- left
      Move Left

    39 -> -- right
      Move Right

    _ ->
      NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
  if model.gameOver then
    Sub.none
  else
    let
      freq =
        500 - 50 * model.level
          |> toFloat
          |> (*) millisecond
          |> max 25
    in
      Sub.batch
        [ Keyboard.downs handleKeyboardInput
        , Time.every freq (\_ -> Fall)
        ]



-- VIEW


view : Model -> Html Msg
view model =
  case model.piece of
    Nothing ->
      renderGrid model.grid model.score

    Just piece ->
      renderGrid (acceptPiece model.grid piece) model.score


toBinaryList : Int -> List Bool
toBinaryList score =
  if score == 0 then
    []
  else if score % 2 == 0 then
    False :: toBinaryList (score // 2)
  else
    True :: toBinaryList (score // 2)


toBinary : Int -> Row
toBinary score =
  let
    raw =
      toBinaryList score
        |> List.reverse
        |> Array.fromList
    padding =
      Array.repeat (gridWidth - (Array.length raw)) False
  in
    Array.append padding raw


renderGrid : Grid -> Int -> Html Msg
renderGrid grid score =
  let
    renderCell on off i j cell =
      rect
        [ width "18"
        , height "18"
        , (i + 2) * 20 |> toString |> y
        , (j + 1) * 20 |> toString |> x
        , fill (if cell then off else on)
        ]
        []

    renderRow on off i row =
      Array.indexedMap (renderCell on off i) row
        |> Array.toList

    scoreRow =
      toBinary score
        |> renderRow "#EEEEEE" "#AAAAAA" -1
  in
    Array.indexedMap (renderRow "#CCCCCC" "#666666") grid
      |> Array.toList
      |> (::) scoreRow
      |> List.concat
      |> svg [ viewBox "0 0 240 500", width "240px", height "500" ]
