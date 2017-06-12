{-# LANGUAGE OverloadedStrings #-}
module HttpServer (run) where

import Web.Spock
import Web.Spock.Config

import Network.Wai.Middleware.Static

run :: Int -> IO ()
run port =
    do
       spockCfg <- defaultSpockCfg () PCNoDatabase ()
       runSpock port (spock spockCfg app)

app :: SpockM () () () ()
app = middleware $ staticPolicy $ (addBase "static") -- >-> (policy $ \x -> Just $ x ++ ".html")
