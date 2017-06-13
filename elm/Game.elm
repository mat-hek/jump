module Game exposing (..)

import AnimationFrame exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Debug exposing (log)
import Element exposing (..)
import Html exposing (Html)
import Keyboard
import Task
import Time exposing (..)
import Window

-- MODEL

type alias Model =
    { x   : Float
    , y   : Float
    , vx  : Float
    , vy  : Float
    , dir : Direction
    , windowSize : Window.Size
    , keys : Keys
    , size : ModelSize
    }

type Direction
    = Left
    | Right

type alias Keys =
    { x : Int
    , y : Int }

type alias ModelSize =
    { x : Int
    , y : Int }

chicken : Model
chicken =
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , dir = Right
    , windowSize =
        { width = 0
        , height = 0 }
    , keys =
        { x = 0
        , y = 0 }
    , size =
        { x = 320
        , y = 320 }
    }


-- UPDATE

applyKey : Int -> Keyboard.KeyCode -> Keys -> Keys
applyKey scale key keys =
    case key of
        37 ->
            { keys | x = -scale }
        38 ->
            { keys | y = scale }
        39 ->
            { keys | x = scale }
        40 ->
            { keys | y = -scale }
        _ ->
            keys

step : Msg -> Model -> ( Model, Cmd Msg )
step msg model =
    case msg of
        Frame dt ->
            ( model
                |> gravity (dt / 10)
                |> jump model.keys
                |> direction model.keys
                |> physics (dt / 8)
            , Cmd.none
            )
        KeyDown key ->
            ( { model | keys = applyKey 1 key model.keys }
            , Cmd.none
            )
        KeyUp key ->
            ( { model | keys = applyKey 0 key model.keys }
            , Cmd.none
            )
        WindowSize size ->
            ( { model | windowSize = size }
            , Cmd.none
            )

jump : Keys -> Model -> Model
jump keys model =
    if keys.y > 0 && model.vy == 0 then
        { model | vy = 10.0 }
    else
        model

gravity : Float -> Model -> Model
gravity dt model =
    { model |
        vy =
            if model.y > 0 then
                model.vy - dt / 4
            else
                0
    }

physics : Float -> Model -> Model
physics dt model =
    { model |
        x = walk dt model,
        y = max 0 ( model.y + dt * model.vy )
    }

walk : Float -> Model -> Float
walk dt model =
    if model.x > ( toFloat -model.windowSize.width ) / 2 && model.x < ( toFloat model.windowSize.width ) / 2 then
        model.x + 2 * dt * model.vx
    else if model.x > 0 then
        model.x - 1.0
    else
        model.x + 1.0


direction : Keys -> Model -> Model
direction keys model =
    { model |
        vx = toFloat keys.x,
        dir =
            if keys.x < 0 then
                Left
            else if keys.x > 0 then
                Right
            else
                model.dir
    }

-- VIEW

display : Model -> Html Msg
display model =
    let
        ( w, h ) =
            ( toFloat model.windowSize.width
            , toFloat model.windowSize.height
            )

        verb =
            if model.y > 0 then
                "jump"
            else if model.vx /= 0 then
                "move"
            else
                "stand"

        dir =
            case model.dir of
                Left -> "left"
                Right -> "right"

        src =
            "/resources/"++ verb ++ "_" ++ dir ++ ".gif"

        modelImage =
            image model.size.x model.size.y src

        groundY = 380 - h / 2

    in
        collage
            model.windowSize.width
            model.windowSize.height
                [ rect w h
                    |> filled ( rgb 174 238 238 )
                , rect w 250
                    |> filled ( rgb 74 167 43 )
                    |> move ( 0, 125 - h / 2 )
                , modelImage
                    |> toForm
                    |> move ( model.x, model.y + groundY )
                ]
                |> Element.toHtml


-- MAIN

type Msg
    = Frame Time
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | WindowSize Window.Size

main : Program Never Model Msg
main =
    Html.program
        { init =
            ( chicken
            , Window.size
                |> Task.perform WindowSize
            )
        , view = display
        , update = step
        , subscriptions =
            \model ->
                Sub.batch
                    [ Keyboard.downs KeyDown
                    , Keyboard.ups KeyUp
                    , AnimationFrame.diffs Frame
                    , Window.resizes WindowSize
                    ]
        }
