module Tetris exposing (..)


import Array exposing (Array)
import Html exposing (br, div, Html, program)
import Keyboard exposing (KeyCode)
import Random
import Svg exposing (rect, Svg, svg, text, text_)
import Svg.Attributes exposing
  (fill, fontFamily, fontSize, height, textAnchor, viewBox, width, x, y)
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
  , score : Int
  , totalCleared : Int
  , paused : Bool
  , gameOver : Bool
  }


init : (Model, Cmd Msg)
init =
  ( { piece = Nothing
    , grid = emptyGrid
    , score = 0
    , totalCleared = 0
    , paused = False
    , gameOver = False
    }
  , getRandomPiece
  )


toLevel : Model -> Int
toLevel model =
  model.totalCleared // linesPerLevel



-- UPDATE


type Msg
  = NoOp
  | Rotate Rotation
  | Move Direction
  | Fall
  | Drop
  | RandomPiece
  | AddPiece (Maybe Piece)
  | TogglePause


type Kind
  = Regular
  | Sticky (Cmd Msg)
  | Snap (Cmd Msg)


acceptAndHandle : Model -> Piece -> Model
acceptAndHandle model piece =
  let
    (numCleared, newGrid) =
      acceptPiece model.grid piece
        |> clearFullRows

    scoreDelta = points numCleared
  in
    { model
      | grid = newGrid
      , piece = Nothing
      , score = model.score + scoreDelta
      , totalCleared = model.totalCleared + numCleared
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

    TogglePause ->
      ( { model | paused = not model.paused }
      , Cmd.none
      )



-- SUBSCRIPTIONS


handleKeyboardInput : KeyCode -> Msg
handleKeyboardInput keyCode =
  case keyCode of
    32 -> -- spacebar
      Drop

    38 -> -- up
      Rotate CCW

    40 -> -- down
      Fall

    37 -> -- left
      Move Left

    39 -> -- right
      Move Right

    27 -> -- esc
      TogglePause

    _ ->
      NoOp


waitForUnpause : KeyCode -> Msg
waitForUnpause keyCode =
  case keyCode of
    27 -> -- esc
      TogglePause

    _ ->
      NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
  if model.gameOver then
    Sub.none
  else if model.paused then
    Keyboard.downs waitForUnpause
  else
    let
      freq =
        500 - 50 * (toLevel model)
          |> toFloat
          |> max 25
          |> (*) millisecond
    in
      Sub.batch
        [ Keyboard.downs handleKeyboardInput
        , Time.every freq (\_ -> Fall)
        ]



-- VIEW


cellView : String -> Int -> Int -> Cell -> Svg Msg
cellView offColor i j cell =
  let color =
    case cell of
      Nothing ->
        offColor

      Just cellColor ->
        cellColor
  in
    rect
      [ width "18"
      , height "18"
      , (i + 3) * 20 |> toString |> y
      , (j + 1) * 20 |> toString |> x
      , fill color
      ]
      []


rowView : String -> Int -> Row -> List (Svg Msg)
rowView offColor i row =
  Array.indexedMap (cellView offColor i) row
    |> Array.toList


view : Model -> Html Msg
view model =
  let
    grid =
      case model.piece of
        Nothing ->
          model.grid

        Just piece ->
          acceptPiece model.grid piece

    levelView =
      text_
        [ x "20", y "30", fill "#666666", fontFamily "Courier", fontSize "14" ]
        [ "Level: " ++ (model |> toLevel |> toString) |> text ]

    scoreView =
      text_
        [ x "20", y "50", fill "#666666", fontFamily "Courier", fontSize "14" ]
        [ "Score: " ++ (model.score |> toString) |> text ]

    statusView =
      if model.gameOver then
        text_
          [ x "220", y "30", fill "#666666", fontFamily "Courier", fontSize "14", textAnchor "end" ]
          [ text "Game over!" ]
      else if model.paused then
        text_
          [ x "220", y "30", fill "#666666", fontFamily "Courier", fontSize "14", textAnchor "end" ]
          [ text "Paused [ESC]" ]
      else
        text ""

  in
    Array.slice 2 gridHeight grid
      |> Array.indexedMap (rowView "#DDDDDD")
      |> Array.toList
      |> List.concat
      |> (::) levelView
      |> (::) scoreView
      |> (::) statusView
      |> svg [ viewBox "0 0 240 500", width "240px", height "500px" ]
