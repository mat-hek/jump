-- websockets example
-- ==================

-- This is the Haskell implementation of the example for the WebSockets library. We
-- implement a simple multi-user chat program. A live demo of the example is
-- available [here](/example/client.html).  In order to understand this example,
-- keep the [reference](/reference/) nearby to check out the functions we use.

{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
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
  receiver::Maybe WS.Connection
}
initialServerState :: ServerState
initialServerState = ServerState Nothing Nothing

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
            \m s -> [receiver s |> (fmap $ flip Send m)] |> catMaybes
    "receiver" -> flip finally disconnect connect
      where
        disconnect = handleEvent state $ \s -> ([], s {receiver = Nothing})
        connect = do
          handleEvent state $ \s -> ([], s {receiver = Just conn})
          forever $ handleMessage conn state ((\_ _ -> [])::(Text -> ServerState -> [WSAction]))
    _ -> return ()


      -- case msg of
      --   "sender" -> do flip finally disconnect connect
      --     where
      --       disconnect = do modifyMVar_ state $ \s -> return $ s {sender = Nothing}
      --       connect = do
      --         modifyMVar_ state $ \s -> return s {sender = Just conn}
      --         forever $ do
      --             m <- (WS.receiveData conn)::(IO Text)
      --             s <- readMVar state
      --             receiver s |> mapM_ (\r -> WS.sendTextData r m)
