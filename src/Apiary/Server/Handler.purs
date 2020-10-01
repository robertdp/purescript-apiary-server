module Apiary.Server.Handler where

import Prelude
import Apiary (class EncodeMedia, class MediaType, JSON, Route)
import Apiary.Route (class PrepareSpec)
import Apiary.Server.Request (class DecodeRequest, Request)
import Apiary.Server.Request as Request
import Apiary.Server.Response (FullResponse, respondWithMedia)
import Apiary.Server.Response as Response
import Apiary.Server.Router (class AttachToRouter, Router)
import Apiary.Server.Router as Router
import Apiary.Server.Url (PathParams)
import Apiary.Status (class ResponseStatus)
import Apiary.Status as Status
import Control.Monad.Except (runExcept)
import Data.Array.NonEmpty as Array
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Foreign (MultipleErrors, renderForeignError)
import Node.HTTP as HTTP
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..), Proxy2(..))

newtype Handler m route
  = Handler
  { route :: route
  , handler :: HTTP.Request -> HTTP.Response -> PathParams -> m Unit
  }

makeHandler ::
  forall route params query body responder m.
  MonadAff m =>
  DecodeRequest route params query body =>
  BuildResponder route m responder =>
  route ->
  (Request params query body -> responder -> FullResponse m) ->
  Handler m route
makeHandler route handler = Handler { route, handler: routerHandler }
  where
  routerHandler httpRequest httpResponse pathParams = do
    requestBody <- liftAff $ Request.readBodyAsString httpRequest
    let
      queryParams = Request.requestQuery httpRequest

      responder = buildResponder route (Proxy2 :: _ m)

      updateHeaders = _ { headers = HTTP.requestHeaders httpRequest }
    Request.decodeRequest route pathParams queryParams requestBody
      # runExcept
      # case _ of
          Right request -> Response.runResponse (handler (updateHeaders request) responder) httpResponse
          Left errs -> Response.runResponse (sendMultipleErrors errs) httpResponse

sendMultipleErrors :: forall m. MonadEffect m => MultipleErrors -> FullResponse m
sendMultipleErrors errs =
  Response.respondWithMedia Status.badRequest (Proxy :: _ (JSON _))
    { errors: Array.fromFoldable $ map renderForeignError $ errs }

attachToRouter ::
  forall m route.
  AttachToRouter route =>
  (m Unit -> Aff Unit) ->
  Handler m route ->
  Router Unit
attachToRouter runHandler (Handler { route, handler }) =
  Router.attachToRouter route \request response params ->
    launchAff_ $ runHandler $ handler request response params

class BuildResponder route (m :: Type -> Type) responder | route m -> responder where
  buildResponder :: forall proxy2. route -> proxy2 m -> responder

instance buildResponders ::
  ( PrepareSpec
      spec
      { path :: params
      , query :: query
      , body :: body
      , response :: Record responses
      }
  , RowToList responses responseList
  , BuildResponderRecord responseList m responders
  ) =>
  BuildResponder (Route method path spec) m { | responders } where
  buildResponder _ _ = Builder.build (buildResponderRecord (RLProxy :: _ responseList) (Proxy2 :: _ m)) {}

class BuildResponderRecord (responses :: RowList) (m :: Type -> Type) (responders :: # Type) | responses m -> responders where
  buildResponderRecord :: forall proxy proxy2. proxy responses -> proxy2 m -> Builder {} { | responders }

instance buildResponderRecordNil :: BuildResponderRecord Nil m () where
  buildResponderRecord _ _ = identity

instance buildResponderRecordCons ::
  ( IsSymbol status
  , ResponseStatus status
  , MediaType responseRep
  , EncodeMedia responseRep response
  , MonadEffect m
  , Lacks status responders'
  , Cons status (response -> FullResponse m) responders' responders
  , BuildResponderRecord responseList m responders'
  ) =>
  BuildResponderRecord (Cons status responseRep responseList) m responders where
  buildResponderRecord _ m = Builder.insert status responder <<< buildResponderRecord (RLProxy :: _ responseList) m
    where
    status = SProxy :: _ status

    responder = respondWithMedia (Status.toStatus status) (Proxy :: _ responseRep)
