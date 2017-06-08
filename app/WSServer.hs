-- websockets example
-- ==================

-- This is the Haskell implementation of the example for the WebSockets library. We
-- implement a simple multi-user chat program. A live demo of the example is
-- available [here](/example/client.html).  In order to understand this example,
-- keep the [reference](/reference/) nearby to check out the functions we use.

{-# LANGUAGE OverloadedStrings, FlexibleInstances, StandaloneDeriving #-}
module WSServer where
-- import Data.Monoid (mappend)
import Data.Maybe
import Data.Text
import Control.Exception (finally)
import Control.Monad
import Control.Concurrent

import qualified Network.WebSockets as WS


-- The main function first creates a new state for the server, then spawns the
-- actual server. For this purpose, we use the simple server provided by
-- `WS.runServer`.

data ServerState = ServerState{
  sender::Maybe WS.Connection,
  receiver::Maybe WS.Connection,
  debugger::Maybe WS.Connection,
  motionCalcState::CalcMotionState
}
initialServerState :: ServerState
initialServerState = ServerState Nothing Nothing Nothing initCalcMotionState

run :: String -> Int -> IO ()
run host port = do
    state <- newMVar initialServerState
    putStrLn $ "WS server is running on " ++ host ++ ":" ++ (show port)
    WS.runServer host port $ application state

(|>) :: a -> (a -> b) -> b
(|>) v f = f v
infixl 1 |>

data WSAction = Send WS.Connection Text

handleEvent::MVar ServerState -> (ServerState -> ([WSAction], ServerState)) -> IO ()
handleEvent state f = do
  s <- takeMVar state
  let  (actions, newstate) = f s
  actions |> mapM_ handleWSAction
  putMVar state newstate

class MessageHandler a where
  handleMessage :: WS.Connection -> MVar ServerState -> a -> IO ()

instance MessageHandler (Text -> ServerState -> ([WSAction], ServerState)) where
  handleMessage conn state f = do
    m <- (WS.receiveData conn)::(IO Text)
    s <- takeMVar state
    let (actions, newstate) = f m s
    actions |> mapM_ handleWSAction
    putMVar state newstate

instance MessageHandler (Text -> ServerState -> [WSAction]) where
  handleMessage conn state f = do
    m <- (WS.receiveData conn)::(IO Text)
    s <- readMVar state
    let actions = f m s
    actions |> mapM_ handleWSAction

handleWSAction::WSAction -> IO ()
handleWSAction (Send conn msg) = WS.sendTextData conn msg

application :: MVar ServerState -> WS.ServerApp
application (state) pending = do
  conn <- WS.acceptRequest pending
  putStrLn "incoming connection"
  WS.forkPingThread conn 30
  msg <- (WS.receiveData conn)::(IO Text)
  case msg of
    "sender" -> flip finally disconnect connect
      where
        disconnect = handleEvent state $ \s -> ([], s {sender = Nothing})
        connect = do
          handleEvent state $ \s -> ([], s {sender = Just conn})
          forever $ handleMessage conn state $
            \m s -> let
              (motion', mcState) = m |> parsePosition |> calcMotion (motionCalcState s)
              m' = motion' |> show |> pack
              d = m' `mappend` ";" `mappend` m
              in (
                [receiver s |> fmap (flip Send $ m'), debugger s |> fmap (flip Send $ d)]
                  |> catMaybes,
                s {motionCalcState = mcState}
              )
    "receiver" -> flip finally disconnect connect
      where
        disconnect = handleEvent state $ \s -> ([], s {receiver = Nothing})
        connect = do
          handleEvent state $ \s -> ([], s {receiver = Just conn})
          dumbReceive conn state
    "debugger" -> flip finally disconnect connect
      where
        disconnect = handleEvent state $ \s -> ([], s {debugger = Nothing})
        connect = do
          handleEvent state $ \s -> ([], s {debugger = Just conn})
          dumbReceive conn state
    _ -> return ()

dumbReceive::WS.Connection -> MVar ServerState -> IO ()
dumbReceive conn state =
  forever $ handleMessage conn state ((\_ _ -> [])::(Text -> ServerState -> [WSAction]))

parsePosition::Text -> [Double]
parsePosition t = t |> splitOn ";" |> fmap (\x -> read (unpack x) :: Double)

data Motion = Stopped | Running | Jumping
deriving instance Show Motion

data MotionBorder = Top | Bottom | None deriving Eq
data MotionScaler = MotionScaler {zeroLvl::Double, lastMotion::Double}
initMotionScaler::MotionScaler
initMotionScaler = MotionScaler 0 0
data CalcMotionState = CalcMotionState {
  motion::Motion, border::MotionBorder, lastChange::Int, lastx::Double, jumpIgnore::Double, scaler::MotionScaler
}

initCalcMotionState::CalcMotionState
initCalcMotionState = CalcMotionState Stopped None 0 0 0 initMotionScaler

-- TODO: split into smaller functions, prevent changing motion state until scaler gets stabilized
calcMotion::CalcMotionState -> [Double] -> (Motion, CalcMotionState)
calcMotion (CalcMotionState motion' border' i lx lj scaler'@(MotionScaler zeroLvl' lastMotion')) [x, _y, _z, _a, _b, _g] =
  case border' of
    _ | lastMotion' > rescaleAfter ->
      nextState {motion = Stopped, border = None, scaler = MotionScaler x 0}
    None | x < bottomLvl -> nextState {motion = Running, border = Bottom}
    b | b `elem` [Top, Bottom] && (x > topJumpLvl || x < bottomJumpLvl) && lj == 0 ->
      nextState {motion = Jumping, jumpIgnore = jumpIgnore'}
    Top | x > topLvl -> nextState {motion = Running}
    Top | x < bottomLvl -> nextState {motion = Running, border = Bottom}
    Top | lx - x >= minDiff -> nextState {motion = Running}
    Top | lx - x < minDiff && i < iMax -> nextState {motion = Running, lastChange = i + 1}
    Bottom | x < bottomLvl -> nextState {motion = Running}
    Bottom | x > topLvl -> nextState {motion = Running, border = Top}
    Bottom | x - lx >= minDiff -> nextState {motion = Running}
    Bottom | x - lx < minDiff && i < iMax -> nextState {motion = Running, lastChange = i + 1}
    _ -> nextState {motion = Stopped, border = None}
  |> \s -> (motion s, s)
  where
    nextState = CalcMotionState motion' border' 0 x (max 0 $ lj-1) motionScaler
    motionScaler = scaler' {lastMotion = if abs (lx - x) > rescaleDiff then 0 else lastMotion' + 1}

    topLvl = zeroLvl' + 3
    bottomLvl = zeroLvl' - 3
    topJumpLvl = zeroLvl' + 10
    bottomJumpLvl = zeroLvl' - 15
    jumpIgnore' = 5
    iMax = 4
    minDiff = 4
    rescaleAfter = 30
    rescaleDiff = 1
calcMotion _ _ = error "invalid position"
