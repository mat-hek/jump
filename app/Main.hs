module Main where

import WSServer
import HttpServer

import Control.Concurrent.Async

main :: IO ()
main = do
  ws <- async $ WSServer.run "0.0.0.0" 9160
  http <- async $ HttpServer.run 4000
  mapM_ wait [ws, http]
