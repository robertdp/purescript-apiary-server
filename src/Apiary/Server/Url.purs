module Apiary.Server.Url where

import Prelude
import Apiary.Url as Url
import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (traverse)
import Data.Unfoldable (class Unfoldable)
import Foreign (F, Foreign, ForeignError(..), fail)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row (class Cons, class Lacks, class Union)
import Prim.RowList (kind RowList, class RowToList, Cons, Nil)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Simple.JSON (read')
import Type.Data.RowList (RLProxy(..))

type PathParams
  = Object String

type QueryParams
  = Object Foreign

class ReadParams pathParams queryParams params | pathParams queryParams -> params where
  readParams :: forall proxy. proxy pathParams -> proxy queryParams -> PathParams -> QueryParams -> F params

instance readParamsRecord ::
  ( Union pathParams queryParams mergedParams
  , RowToList pathParams pathParamList
  , ReadPathParams pathParams pathParamList
  , RowToList queryParamsRep queryParamList
  , DecodeQueryParams queryParams queryParamList
  ) =>
  ReadParams (Record pathParams) (Record queryParamsRep) (Record mergedParams) where
  readParams pathProxy queryProxy pathParams queryParams = do
    pathBuilder <- readPathParams (RLProxy :: _ pathParamList) pathParams
    queryBuilder <- decodeQueryParams (RLProxy :: _ queryParamList) queryParams
    let
      path = Builder.build pathBuilder {}

      query = Builder.build queryBuilder {}
    pure $ Record.union path query

class ReadPathParams (params :: # Type) (paramList :: RowList) | paramList -> params where
  readPathParams :: forall proxy. proxy paramList -> PathParams -> F (Builder {} (Record params))

instance readPathParamsNil :: ReadPathParams () Nil where
  readPathParams _ _ = pure identity

instance readPathParamsCons ::
  ( Cons name value params' params
  , ReadPathParams params' paramList
  , IsSymbol name
  , Lacks name params'
  , Url.DecodeParam value
  ) =>
  ReadPathParams params (Cons name value paramList) where
  readPathParams _ params = do
    builder <- readPathParams (RLProxy :: _ paramList) params
    let
      name = reflectSymbol (SProxy :: _ name)
    value <- case Object.lookup name params of
      Just a -> Url.decodeParam a
      Nothing ->
        throwError $ pure $ ErrorAtProperty name
          $ ForeignError "invalid path param"
    pure $ builder >>> Builder.insert (SProxy :: _ name) value

class DecodeQueryParams (params :: # Type) (paramList :: RowList) | paramList -> params where
  decodeQueryParams :: forall proxy. proxy paramList -> QueryParams -> F (Builder {} (Record params))

instance decodeQueryParamsNil :: DecodeQueryParams () Nil where
  decodeQueryParams _ _ = pure identity

instance decodeQueryParamsConsArray ::
  ( IsSymbol name
  , Url.DecodeParam value
  , Unfoldable f
  , Cons name (f value) params' params
  , Lacks name params'
  , DecodeQueryParams params' paramList
  ) =>
  DecodeQueryParams params (Cons name (f value) paramList) where
  decodeQueryParams _ params = do
    let
      name = SProxy :: _ name

      prop = Url.encodeParam $ reflectSymbol name
    builder <- decodeQueryParams (RLProxy :: _ paramList) params
    value <- case Object.lookup prop params of
      Nothing -> pure []
      Just a -> do
        values <- read' a <|> Array.singleton <$> read' a
        traverse Url.decodeParam values
    pure $ Builder.insert name (Array.toUnfoldable value) <<< builder
else instance decodeQueryParamsCons ::
  ( IsSymbol name
  , Url.DecodeParam value
  , Cons name value params' params
  , Lacks name params'
  , DecodeQueryParams params' paramList
  ) =>
  DecodeQueryParams params (Cons name value paramList) where
  decodeQueryParams _ params = do
    let
      name = SProxy :: _ name

      prop = Url.encodeParam $ reflectSymbol name
    builder <- decodeQueryParams (RLProxy :: _ paramList) params
    value <- case Object.lookup prop params of
      Nothing -> fail $ ErrorAtProperty prop $ ForeignError "missing query param"
      Just a -> do
        Url.decodeParam =<< read' a
    pure $ Builder.insert name value <<< builder
