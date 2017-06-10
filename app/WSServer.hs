{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module WSServer (run) where
import Data.Maybe
import Data.Text
import Control.Exception (finally)
import Control.Monad
import Control.Concurrent

import qualified Network.WebSockets as WS

import Utils
import qualified MotionDetector as MD

data ServerState = ServerState{
  sender::Maybe WS.Connection,
  receiver::Maybe WS.Connection,
  debugger::Maybe WS.Connection,
  detectorState::MD.MDState
}
initialServerState :: ServerState
initialServerState = ServerState Nothing Nothing Nothing MD.initMDState

run :: String -> Int -> IO ()
run host port = do
    state <- newMVar initialServerState
    putStrLn $ "WS server is running on " ++ host ++ ":" ++ (show port)
    WS.runServer host port $ application state

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
        disconnect = handleEvent state $ \s -> ([], s {
          sender = Nothing, detectorState = MD.initMDState})
        connect = do
          handleEvent state $ \s -> ([], s {sender = Just conn})
          forever $ handleMessage conn state $
            \m s ->m
              |> parsePosition
              |> fmap (MD.detectMotion $ detectorState s)
              |> fmap (\(motion', mdState) ->
                  let
                    m' = motion' |> show |> pack
                    d = m' `mappend` ";" `mappend` m
                  in (
                    [receiver s |> fmap (flip Send $ m'),
                      debugger s |> fmap (flip Send $ d)]
                      |> catMaybes,
                    s {detectorState = mdState}
                  )
                )
              |> fromMaybe ([], s)
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

parsePosition::Text -> Maybe MD.Acceleration
parsePosition t =
  case t |> splitOn ";" |> fmap (\x -> read (unpack x) :: Double) of
    [x, y, z, a, b, g] -> Just $ MD.Acceleration x y z a b g
    _ -> Nothing
