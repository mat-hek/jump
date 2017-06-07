module Main where

import WSServer
import HttpServer

import Control.Concurrent.Async

main :: IO ()
main = do
  ws <- async WSServer.run
  http <- async HttpServer.run
  mapM_ wait [ws, http]
