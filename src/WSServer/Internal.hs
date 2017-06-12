{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module WSServer.Internal where

import Data.Maybe
import Data.Text
import Control.Exception (finally)
import Control.Monad
import Control.Concurrent

import qualified Network.WebSockets as WS

import Utils
import qualified MotionDetector as MD
import WSHelper (WSAction(Send))
import qualified WSHelper as WSH

data ServerState = ServerState{
  sender::Maybe WS.Connection,
  receiver::Maybe WS.Connection,
  debugger::Maybe WS.Connection,
  detectorState::MD.MDState,
  motion::MD.Motion
}
initialServerState :: ServerState
initialServerState = ServerState Nothing Nothing Nothing MD.initMDState MD.Undetected

run :: String -> Int -> IO ()
run host port = do
    state <- newMVar initialServerState
    putStrLn $ "WS server is running on " ++ host ++ ":" ++ (show port)
    WS.runServer host port $ application state


application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  putStrLn "incoming connection"
  WS.forkPingThread conn 30
  msg <- (WS.receiveData conn)::(IO Text)
  case msg of
    "sender" -> flip finally disconnect connect
      where
        disconnect = WSH.handleEvent state $ \s -> (
            [receiver s |> fmap (flip Send $ serializeMotion MD.Undetected)] |> catMaybes,
            s {sender = Nothing, detectorState = MD.initMDState, motion = MD.Undetected}
          )
        connect = do
          WSH.handleEvent state $ \s -> ([], s {sender = Just conn})
          forever $ WSH.handleMessage conn state handleAcceleration
    "receiver" -> flip finally disconnect connect
      where
        disconnect = WSH.handleEvent state $ \s -> ([], s {receiver = Nothing})
        connect = do
          WSH.handleEvent state $ \s -> ([], s {receiver = Just conn})
          WSH.dumbReceive conn state
    "debugger" -> flip finally disconnect connect
      where
        disconnect = WSH.handleEvent state $ \s -> ([], s {debugger = Nothing})
        connect = do
          WSH.handleEvent state $ \s -> ([], s {debugger = Just conn})
          WSH.dumbReceive conn state
    _ -> return ()

handleAcceleration :: Text -> ServerState -> ([WSAction], ServerState)
handleAcceleration m s = m
  |> parseAcceleration
  |> fmap (MD.detectMotion $ detectorState s)
  |> fmap (\(motion', mdState) ->
      let
        m' = motion' |> serializeMotion
        d = m' `mappend` ";" `mappend` m
      in (
        [guard (motion s /= motion') >>= \_ -> receiver s |> fmap (flip Send $ m'),
        debugger s |> fmap (flip Send $ d)]
          |> catMaybes,
        s {detectorState = mdState}
      )
    )
  |> fromMaybe ([], s)

parseAcceleration :: Text -> Maybe MD.Acceleration
parseAcceleration t =
  case t |> splitOn ";" |> fmap (\x -> read (unpack x) :: Double) of
    [x, y, z, a, b, g] -> Just $ MD.Acceleration x y z a b g
    _ -> Nothing

serializeMotion :: MD.Motion -> Text
serializeMotion m = m |> show |> pack
