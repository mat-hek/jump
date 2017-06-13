module Actions exposing (Action(..))

import Time exposing (Time)
import Window

{-| Actions invoked in application -}
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