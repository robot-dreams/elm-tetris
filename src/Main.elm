import Array exposing (Array)
import Html exposing (br, div, Html, program, text)
import Keyboard exposing (KeyCode)
import Random
import Svg exposing (rect, svg)
import Svg.Attributes exposing (fill, height, viewBox, width, x, y)
import Task
import Time exposing (millisecond)


import Constants exposing (..)
import Types exposing (..)
import Utils exposing (..)



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


tryLevelUp : Model -> Model
tryLevelUp model =
  if model.score >= nextLevel then
    tryLevelUp
      { model
        | score = model.score - nextLevel
        , level = model.level + 1
      }
  else
    model


acceptAndHandle : Model -> Piece -> Model
acceptAndHandle model piece =
  let
    (scoreDelta, newGrid) =
      acceptPiece model.grid piece
        |> clearFullRows
  in
    tryLevelUp
      { model
        | grid = newGrid
        , piece = Nothing
        , score = model.score + scoreDelta
      }


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
              ( acceptAndHandle model transformed
              , cmd
              )

            _ ->
              ( { model | piece = Just transformed }
              , Cmd.none
              )
        else
          case kind of
            Sticky cmd ->
              ( acceptAndHandle model piece
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
      gridView model.grid model.score

    Just piece ->
      gridView (acceptPiece model.grid piece) model.score


gridView : Grid -> Int -> Html Msg
gridView grid score =
  let
    cellView off i j cell =
      let color =
        case cell of
          Nothing ->
            off

          Just on ->
            on
      in
        rect
          [ width "18"
          , height "18"
          , (i + 1) * 20 |> toString |> y
          , (j + 1) * 20 |> toString |> x
          , fill color
          ]
          []

    rowView off i row =
      Array.indexedMap (cellView off i) row
        |> Array.toList

    scoreView =
      toBinaryRow "#AAAAAA" score
        |> rowView "#EEEEEE" (gridHeight - 2)
  in
    Array.slice 2 gridHeight grid
      |> Array.indexedMap (rowView "#DDDDDD")
      |> Array.toList
      |> (::) scoreView
      |> List.concat
      |> svg [ viewBox "0 0 240 500", width "240px", height "500" ]
