module Apiary.Server.Request where

import Prelude
import Apiary (class DecodeMedia, Route, decodeMedia)
import Apiary.Route (class PrepareSpec)
import Apiary.Server.Url (class DecodePathParams, class DecodeQueryParams, PathParams, QueryParams, decodePathParams, decodeQueryParams)
import Data.Either (Either(..), either)
import Data.Maybe (maybe)
import Data.Nullable as Nullable
import Effect.Aff (Aff, launchAff_, throwError)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (catchException)
import Foreign (F)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.HTTP as HTTP
import Node.Stream as Stream
import Node.URL as URL
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type Request params query body
  = { path :: params
    , query :: query
    , headers :: Object String
    , body :: body
    }

class DecodeRequest route params query body | route -> params query body where
  decodeRequest :: route -> PathParams -> QueryParams -> String -> F (Request params query body)

instance decodeRequestRoute ::
  ( PrepareSpec
      spec
      { path :: params
      , query :: query
      , body :: body
      , response :: response
      }
  , DecodePathParams params
  , DecodeQueryParams query
  , DecodeMedia body body'
  ) =>
  DecodeRequest (Route path method spec) params query body' where
  decodeRequest _ pathParams queryParams requestBody = do
    params <- decodePathParams (Proxy :: _ params) pathParams
    query <- decodeQueryParams (Proxy :: _ query) queryParams
    body <- decodeMedia (Proxy :: _ body) requestBody
    pure { path: params, query, headers: Object.empty, body }

readBodyAsBuffer :: HTTP.Request -> Aff Buffer
readBodyAsBuffer request = do
  let
    stream = HTTP.requestAsStream request
  bodyResult <- AVar.empty
  chunks <- AVar.new []
  fillResult <- liftEffect do catchException (pure <<< Left) (Right <$> fillBody stream chunks bodyResult)
  body <- AVar.take bodyResult
  either throwError pure (fillResult *> body)
  where
  fillBody stream chunks bodyResult = do
    Stream.onData stream \chunk ->
      let
        modification = do
          v <- AVar.take chunks
          AVar.put (v <> [ chunk ]) chunks
      in
        launchAff_ modification
    Stream.onError stream (launchAff_ <<< flip AVar.put bodyResult <<< Left)
    Stream.onEnd stream do
      launchAff_ do
        AVar.take chunks
          >>= concat'
          >>= (pure <<< Right)
          >>= flip AVar.put bodyResult

  concat' = liftEffect <<< Buffer.concat

readBodyAsString :: HTTP.Request -> Aff String
readBodyAsString = readBodyAsBuffer >=> liftEffect <<< Buffer.toString Encoding.UTF8

requestQuery :: HTTP.Request -> QueryParams
requestQuery =
  maybe Object.empty (coerceQuery <<< URL.parseQueryString)
    <<< (Nullable.toMaybe <<< _.query <<< URL.parse)
    <<< HTTP.requestURL

coerceQuery :: URL.Query -> QueryParams
coerceQuery = unsafeCoerce
