{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
module WSHelper (WSAction(Send), handleEvent, handleMessage, dumbReceive) where

import Data.Text
import Control.Concurrent
import Control.Monad

import qualified Network.WebSockets as WS

import Utils

data WSAction = Send WS.Connection Text

handleEvent :: MVar s -> (s -> ([WSAction], s)) -> IO ()
handleEvent state f = do
  s <- takeMVar state
  let  (actions, newstate) = f s
  actions |> mapM_ handleWSAction
  putMVar state newstate

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

dumbReceive::WS.Connection -> MVar s -> IO ()
dumbReceive conn (state::MVar s) =
  forever $ handleMessage conn state (\(_m::Text) (_s::s) -> []::[WSAction])

handleWSAction::WSAction -> IO ()
handleWSAction (Send conn msg) = WS.sendTextData conn msg
