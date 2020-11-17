module Apiary.Server.Router where

import Prelude
import Apiary (Route)
import Apiary.Server.Url (PathParams)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, mkEffectFn2, mkEffectFn3, runEffectFn1, runEffectFn3, runEffectFn4)
import Node.HTTP as HTTP

newtype Router a
  = Router (ReaderT RouterInstance Effect a)

derive newtype instance functorRouter :: Functor Router

derive newtype instance applyRouter :: Apply Router

derive newtype instance applicativeRouter :: Applicative Router

derive newtype instance bindRouter :: Bind Router

derive newtype instance monadRouter :: Monad Router

derive newtype instance monadEffectRouter :: MonadEffect Router

foreign import data RouterInstance :: Type

foreign import _create :: EffectFn1 (EffectFn2 HTTP.Request HTTP.Response Unit) RouterInstance

create :: (HTTP.Request -> HTTP.Response -> Effect Unit) -> Effect RouterInstance
create fallback = runEffectFn1 _create (mkEffectFn2 fallback)

foreign import _lookup :: EffectFn3 RouterInstance HTTP.Request HTTP.Response Unit

lookup :: HTTP.Request -> HTTP.Response -> RouterInstance -> Effect Unit
lookup req res router = runEffectFn3 _lookup router req res

createServer ::
  forall m.
  MonadEffect m =>
  (HTTP.Request -> HTTP.Response -> Effect Unit) ->
  Router Unit ->
  m HTTP.Server
createServer fallback (Router runRouter) =
  liftEffect do
    router <- create fallback
    runReaderT runRouter router
    HTTP.createServer \req res -> do
      lookup req res router

foreign import _on ::
  EffectFn4 RouterInstance String String
    (EffectFn3 HTTP.Request HTTP.Response PathParams Unit)
    Unit

type Method
  = String

type Path
  = String

on ::
  Method ->
  Path ->
  (HTTP.Request -> HTTP.Response -> PathParams -> Effect Unit) ->
  Router Unit
on method path handler =
  Router do
    router <- ask
    lift $ runEffectFn4 _on router method path (mkEffectFn3 handler)

class AttachToRouter route where
  attachToRouter :: route -> (HTTP.Request -> HTTP.Response -> PathParams -> Effect Unit) -> Router Unit

instance attachToRouterRoute ::
  ( IsSymbol method
  , IsSymbol path
  ) =>
  AttachToRouter (Route method path spec) where
  attachToRouter _ = on method path
    where
    method = reflectSymbol (SProxy :: _ method)

    path = reflectSymbol (SProxy :: _ path)
