module Model exposing
    ( Enemy
    , Hero
    , Model
    , State(..)
    , initial
    )

import Window

{-| All models used in application -}

type alias Model =
    { hero : Hero
    , jump : Bool
    , move : Bool
    , enemies : List Enemy
    , windowSize : Window.Size
    , state : State
    , score : Int
    }

type alias Enemy =
    { x : Float
    , y : Float
    , size : Size
    }

type State
    = Paused
    | Playing

stateToString : State -> String
stateToString state =
    case state of
        Paused  -> "Paused"
        Playing -> "Playing"

type alias Hero =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , size : Size
    }

type alias Size =
    { x : Int
    , y : Int
    }

type alias Keys =
    { x : Int
    , y : Int
    }

{-| Initial fetched at the begging of the application lifecycle -}
initial : Model
initial =
    { hero =
        { x = 0
        , y = 0
        , vx = 0
        , vy = 0
        , size =
            { x = 320
            , y = 320
            }
        }
    , move = False
    , jump = False
    , enemies = []
    , windowSize =
        { width = 0
        , height = 0
        }
    , state = Paused
    , score = 0
    }
