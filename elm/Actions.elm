module Actions exposing (Action(..))

import Time exposing (Time)
import Window

type Action
    = Tick Time
    | SocketMessage String
    | WindowSize Window.Size
    | Start
    | GameOver
    | Move Bool
    | Jump Bool
    | ChangeState Bool
    | Noop