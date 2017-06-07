{-# LANGUAGE OverloadedStrings #-}
module HttpServer where

import Web.Spock
import Web.Spock.Config

import Network.Wai.Middleware.Static

data MySession = EmptySession

run :: IO ()
run =
    do
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase ()
       runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession () ()
app = middleware $ staticPolicy $ (addBase "static") >-> (policy $ \x -> Just $ x ++ ".html")
