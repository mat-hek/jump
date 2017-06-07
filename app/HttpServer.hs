{-# LANGUAGE OverloadedStrings #-}
module HttpServer where

import Web.Spock
import Web.Spock.Config

import Network.Wai.Middleware.Static

data MySession = EmptySession

run :: Int -> IO ()
run port =
    do
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase ()
       runSpock port (spock spockCfg app)

app :: SpockM () MySession () ()
app = middleware $ staticPolicy $ (addBase "static") -- >-> (policy $ \x -> Just $ x ++ ".html")
