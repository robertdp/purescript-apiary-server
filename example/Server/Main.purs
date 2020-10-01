module Ads.Server.Main where

import Prelude
import Apiary.Server.Handler as Server
import Apiary.Server.Router as Router
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Example.Server.EchoTest (echoTestHandler)
import Node.Encoding as Encoding
import Node.HTTP as HTTP
import Node.Stream as Stream

main :: Effect Unit
main = do
  server <- Router.createServer notFoundFallback do Server.attachToRouter identity echoTestHandler
  HTTP.listen server { backlog: Nothing, hostname: "127.0.0.1", port: 8000 } do
    Console.log "Listening on port 8000"

notFoundFallback :: HTTP.Request -> HTTP.Response -> Effect Unit
notFoundFallback request response =
  if HTTP.requestMethod request == "OPTIONS" then do
    HTTP.setStatusCode response 204
    HTTP.setStatusMessage response "No Content"
    HTTP.setHeader response "Access-Control-Allow-Origin" "*"
    Stream.end (HTTP.responseAsStream response) mempty
  else do
    HTTP.setStatusCode response 404
    HTTP.setStatusMessage response "Not Found"
    let
      stream = HTTP.responseAsStream response
    void $ Stream.writeString stream Encoding.UTF8 "<h1>Not Found</h1>" mempty
    Stream.end stream do
      Console.log $ "Not found: " <> HTTP.requestMethod request <> " " <> HTTP.requestURL request
