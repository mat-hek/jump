module Update exposing (..)

import Actions exposing (Action)
import Model exposing (Model, State(..), Hero)
import Time exposing (Time)
import Window


{-| Update flow for each message/event invoked in application

    Generally its all about updating game god-object holding all current state
-}
update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        Actions.WindowSize size ->
            ( { model
                | windowSize = size }
            , Cmd.none
            )

        Actions.ChangeState onOff ->
            ( if onOff then
                { model
                    | state =
                        if model.state == Playing then
                            Paused
                        else
                            Playing
                }
              else
                model
            , Cmd.none
            )

        Actions.SocketMessage message ->
            ( model |> processSocketMessage message
            , Cmd.none )

        Actions.Start ->
            ( { model
                | state = Playing }
            , Cmd.none
            )

        Actions.GameOver ->
            ( { model
                | state = Paused }
            , Cmd.none
            )

        Actions.Move onOff ->
            ( { model
                | move = onOff }
            , Cmd.none
            )

        Actions.Jump onOff ->
            ( { model
                | jump = onOff }
            , Cmd.none
            )

        Actions.Tick time ->
            ( model
                |> animate (min time 25)
            , Cmd.none
            )

        Actions.Noop ->
            ( model
            , Cmd.none
            )

processSocketMessage : String -> Model ->  Model
processSocketMessage  message model =
    case String.toLower message of
        "stopped" -> { model
                        | move = False
                        , jump = False }
        "running" -> { model
                        | move = True
                        , jump = False }
        "jumping" -> { model
                        | jump = True }
        _         -> { model
                        | move = False
                        , jump = False }

animate : Time -> Model -> Model
animate dt model =
    model
        |> spawnEnemies
        |> moveEnemies dt
        |> destroyEnemies
        |> moveHero dt
        |> checkEndGame


spawnEnemies : Model -> Model
spawnEnemies model =
    model

moveEnemies : Time -> Model -> Model
moveEnemies time model =
    model

destroyEnemies : Model -> Model
destroyEnemies model =
    model

moveHero : Time -> Model -> Model
moveHero dt model =
    { model | hero =
                model.hero
                |> gravity dt
                |> jump model.jump
                |> move model.move
                |> physics dt model.windowSize
    }

move : Bool -> Hero -> Hero
move move hero =
    { hero | vx =
                if move then
                    0.2
                else
                    -0.1
    }

physics : Time -> Window.Size -> Hero -> Hero
physics dt size hero =
    { hero |
        x = walk dt size hero,
        y = max 0 ( hero.y + (dt / 8) * hero.vy )
    }

walk : Time -> Window.Size -> Hero -> Float
walk dt size hero =
    if hero.x - (toFloat hero.size.x) / 3 < (toFloat -size.width) / 2 && hero.vx < 0 then
        hero.x
    else if hero.x + (toFloat hero.size.x) / 3 > (toFloat size.width) / 2 && hero.vx > 0 then
        hero.x
    else
        hero.x + 2 * dt * hero.vx

jump : Bool -> Hero -> Hero
jump jump hero =
    if jump && hero.vy == 0 then
       { hero | vy = 20.0 }
    else
        hero

gravity : Time -> Hero -> Hero
gravity dt hero =
    { hero | vy =
                if hero.y > 0 then
                    hero.vy - dt / 12
                else
                    0
    }

checkEndGame : Model -> Model
checkEndGame model =
    model
