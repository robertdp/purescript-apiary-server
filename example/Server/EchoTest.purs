module Example.Server.EchoTest where

import Prelude
import Apiary.Route (Route(..))
import Apiary.Server.Handler (Handler, makeHandler)
import Apiary.Server.Response as Response
import Effect.Aff (Aff)
import Example.Interface.EchoTest (EchoTest)

echoTestHandler :: Handler Aff EchoTest
echoTestHandler =
  makeHandler Route \request respond -> Response.do
    respond.ok { message: "Echo: " <> request.path.first <> " " <> request.path.last }
