{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
module WSHelper.Internal where

import Data.Text
import Control.Concurrent
import Control.Monad

import qualified Network.WebSockets as WS

import Utils

{-|
  Represents websocket action.
-}
data WSAction = Send WS.Connection Text

{-|
  Handles websocket event.
  Accepts MVar containing state and event handeler
  Executes event handler and returned 'WSAction's, updates state
-}
handleEvent :: MVar s -> (s -> ([WSAction], s)) -> IO ()
handleEvent state f = do
  s <- takeMVar state
  let  (actions, newstate) = f s
  actions |> mapM_ handleWSAction
  putMVar state newstate

{-|
  Handles websocket message.
  Accepts 'MVar' containing state and message handeler
  Executes message handler and returned 'WSAction's, updates state
-}
class MessageHandler s a where
  handleMessage :: WS.Connection -> MVar s -> a -> IO ()

instance MessageHandler s (Text -> s -> ([WSAction], s)) where
  handleMessage conn state f = do
    m <- (WS.receiveData conn)::(IO Text)
    s <- takeMVar state
    let (actions, newstate) = f m s
    actions |> mapM_ handleWSAction
    putMVar state newstate

instance MessageHandler s (Text -> s -> [WSAction]) where
  handleMessage conn state f = do
    m <- (WS.receiveData conn)::(IO Text)
    s <- readMVar state
    let actions = f m s
    actions |> mapM_ handleWSAction

{-|
  Keeps receiving and ignoring messages.
  Prevents from disconnecting client.
-}
dumbReceive::WS.Connection -> MVar s -> IO ()
dumbReceive conn (state::MVar s) =
  forever $ handleMessage conn state (\(_m::Text) (_s::s) -> []::[WSAction])

{-|
  Executes 'WSAction'
-}
handleWSAction::WSAction -> IO ()
handleWSAction (Send conn msg) = WS.sendTextData conn msg
