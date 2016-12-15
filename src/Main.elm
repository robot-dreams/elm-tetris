import Array exposing (Array)
import Html exposing (Html, program, text)
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



type alias Model =
  { piece : Maybe Piece
  , grid : Grid
  , gameOver : Bool
  }


init : (Model, Cmd Msg)
init =
  ( { piece = Nothing
    , grid = emptyGrid
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
  | RandomPiece
  | AddPiece (Maybe Piece)


type Kind
  = Regular
  | Sticky (Cmd Msg)


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
          ( { model | piece = Just transformed }
          , Cmd.none
          )
        else
          case kind of
            Regular ->
              (model, Cmd.none)

            Sticky cmd ->
              ( { model
                    | grid = acceptPiece model.grid piece |> clearFullRows
                    , piece = Nothing
                }
              , cmd
              )


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
      Rotate CCW

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
    Sub.batch
      [ Keyboard.downs handleKeyboardInput
      , Time.every (500 * millisecond) (\_ -> Fall)
      ]



-- VIEW


view : Model -> Html Msg
view model =
  case model.piece of
    Nothing ->
      renderGrid model.grid

    Just piece ->
      renderGrid (acceptPiece model.grid piece)


renderGrid : Grid -> Html Msg
renderGrid grid =
  let
    renderCell i j cell =
      rect
        [ width "18"
        , height "18"
        , (i + 1) * 20 |> toString |> y
        , (j + 1) * 20 |> toString |> x
        , fill (if cell then "#666666" else "#CCCCCC")
        ]
        []

    renderRow i row =
      Array.indexedMap (renderCell i) row
        |> Array.toList
  in
    Array.indexedMap renderRow grid
      |> Array.toList
      |> List.concat
      |> svg [ viewBox "0 0 400 600", width "400px", height "600px" ]
