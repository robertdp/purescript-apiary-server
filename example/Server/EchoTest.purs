module Example.Server.EchoTest where

import Prelude
import Apiary.Route (Route(..))
import Apiary.Server.Handler (Handler)
import Apiary.Server.Handler as Handler
import Effect.Aff (Aff)
import Example.Interface.EchoTest (EchoTest)

echoTestHandler :: Handler Aff EchoTest
echoTestHandler =
  Handler.makeHandler Route \request respond -> Server.do
    respond.ok { message: "Echo: " <> request.path.first <> " " <> request.path.last }
