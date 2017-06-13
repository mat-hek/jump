module Main exposing (..)

import Actions exposing (Action)
import AnimationFrame
import Html exposing (Html)
import Keyboard exposing (KeyCode)
import Model exposing (Model)
import Task exposing (Task)
import Update
import View
import WebSocket
import Window


subscriptions : Model -> Sub Action
subscriptions model =
    Sub.batch
        [ if model.state == Model.Playing then
            AnimationFrame.diffs Actions.Tick
          else
            Sub.none
        , Keyboard.ups (key False)
        , Keyboard.downs (key True)
        , Window.resizes Actions.WindowSize
        , WebSocket.listen "ws://localhost:9160" Actions.SocketMessage
        ]

key : Bool -> KeyCode -> Action
key onOff keycode =
    case keycode of
        32 ->
            Actions.ChangeState onOff
        39 ->
            Actions.Move onOff
        38 ->
            Actions.Jump onOff
        _ ->
            Actions.Noop

subscribeToServer : Cmd Action
subscribeToServer =
    WebSocket.send "ws://localhost:9160" "receiver"

main : Program Never Model Action
main =
    Html.program
        { init =
            ( Model.initial
            , Cmd.batch
                [ Window.size |> Task.perform Actions.WindowSize
                , subscribeToServer
                ]
            )
        , update = Update.update
        , view = View.view
        , subscriptions = subscriptions
        }
