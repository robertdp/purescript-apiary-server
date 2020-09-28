module Example.Client.EchoTest where

import Prelude
import Example.Interface.EchoTest (EchoTest)
import Apiary.Client (Error, Request)
import Apiary.Client as Client
import Apiary.Route (Route(..))
import Data.Either (Either)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Milkis (URL(..))

echoTest ::
  String ->
  String ->
  Aff
    ( Either Error
        ( Variant
            ( ok ::
                { message :: String
                }
            )
        )
    )
echoTest first last = Client.makeRequest (Route :: EchoTest) setBaseUrl { first, last } unit

setBaseUrl :: Request -> Request
setBaseUrl request@{ url: URL url } = request { url = URL $ "http://localhost:8000" <> url }
