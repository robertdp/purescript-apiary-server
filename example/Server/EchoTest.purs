module Example.Server.EchoTest where

import Prelude
import Example.Interface.EchoTest (EchoTest)
import Apiary.Route (Route(..))
import Apiary.Server (Handler)
import Apiary.Server as Server
import Effect.Aff (Aff)

echoTestHandler :: Handler Aff EchoTest
echoTestHandler =
  Server.makeHandler Route \request respond -> Response.do
    respond.ok { message: "Echo: " <> request.params.first <> " " <> request.params.last }
