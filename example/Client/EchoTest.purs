module Example.Client.EchoTest where

import Prelude
import Apiary (Error, Route(..), Request, makeRequest, none)
import Data.Either (Either)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Example.Interface.EchoTest (EchoTest)

echoTest ::
  String ->
  String ->
  Aff
    ( Either Error
        ( Variant
            ( ok :: { message :: String }
            )
        )
    )
echoTest first last = makeRequest (Route :: EchoTest) setBaseUrl { first, last } none none

setBaseUrl :: Request -> Request
setBaseUrl request@{ url } = request { url = "http://localhost:8000" <> url }
