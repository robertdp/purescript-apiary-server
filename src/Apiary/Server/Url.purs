module Apiary.Server.Url where

import Prelude
import Apiary (class DecodeParam, None, decodeParam, encodeParam, none)
import Control.Monad.Error.Class (throwError)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign (F, Foreign, ForeignError(..), fail)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record.Builder (Builder)
import Record.Builder as Builder
import Simple.JSON (read')
import Type.Data.RowList (RLProxy(..))

type PathParams
  = Object String

type QueryParams
  = Object Foreign

class DecodePathParams params where
  decodePathParams :: forall proxy. proxy params -> PathParams -> F params

instance decodePathParamsNone :: DecodePathParams None where
  decodePathParams _ _ = pure none

instance decodePathParamsRecord :: (RowToList r rl, DecodePathParamsRecord r rl) => DecodePathParams (Record r) where
  decodePathParams proxy params = do
    builder <- decodePathParamsRecordImpl (RLProxy :: _ rl) params
    pure $ Builder.build builder {}

class DecodeQueryParams params where
  decodeQueryParams :: forall proxy. proxy params -> QueryParams -> F params

instance decodeQueryParamsNone :: DecodeQueryParams None where
  decodeQueryParams _ _ = pure none

instance decodeQueryParamsRecord :: (RowToList r rl, DecodeQueryParamsRecord r rl) => DecodeQueryParams (Record r) where
  decodeQueryParams proxy params = do
    builder <- decodeQueryParamsRecordImpl (RLProxy :: _ rl) params
    pure $ Builder.build builder {}

class DecodePathParamsRecord (params :: # Type) (paramList :: RowList) | paramList -> params where
  decodePathParamsRecordImpl :: forall proxy. proxy paramList -> PathParams -> F (Builder {} (Record params))

instance decodePathParamsNil :: DecodePathParamsRecord () Nil where
  decodePathParamsRecordImpl _ _ = pure identity

instance decodePathParamsCons ::
  ( Cons name value params' params
  , DecodePathParamsRecord params' paramList
  , IsSymbol name
  , Lacks name params'
  , DecodeParam value
  ) =>
  DecodePathParamsRecord params (Cons name value paramList) where
  decodePathParamsRecordImpl _ params = do
    builder <- decodePathParamsRecordImpl (RLProxy :: _ paramList) params
    let
      name = reflectSymbol (SProxy :: _ name)
    value <- case Object.lookup name params of
      Just a -> decodeParam a
      Nothing ->
        throwError $ pure $ ErrorAtProperty name
          $ ForeignError "invalid path param"
    pure $ builder >>> Builder.insert (SProxy :: _ name) value

class DecodeQueryParamsRecord (params :: # Type) (paramList :: RowList) | paramList -> params where
  decodeQueryParamsRecordImpl :: forall proxy. proxy paramList -> QueryParams -> F (Builder {} (Record params))

instance decodeQueryParamsNil :: DecodeQueryParamsRecord () Nil where
  decodeQueryParamsRecordImpl _ _ = pure identity

instance decodeQueryParamsConsFoldable ::
  ( IsSymbol name
  , DecodeParam value
  , Cons name (Maybe value) params' params
  , Lacks name params'
  , DecodeQueryParamsRecord params' paramList
  ) =>
  DecodeQueryParamsRecord params (Cons name (Maybe value) paramList) where
  decodeQueryParamsRecordImpl _ params = do
    let
      name = SProxy :: _ name

      prop = encodeParam $ reflectSymbol name
    builder <- decodeQueryParamsRecordImpl (RLProxy :: _ paramList) params
    value <- case Object.lookup prop params of
      Nothing -> pure Nothing
      Just a -> read' a >>= decodeParam >>> map Just
    pure $ Builder.insert name value <<< builder
else instance decodeQueryParamsCons ::
  ( IsSymbol name
  , DecodeParam value
  , Cons name value params' params
  , Lacks name params'
  , DecodeQueryParamsRecord params' paramList
  ) =>
  DecodeQueryParamsRecord params (Cons name value paramList) where
  decodeQueryParamsRecordImpl _ params = do
    let
      name = SProxy :: _ name

      prop = encodeParam $ reflectSymbol name
    builder <- decodeQueryParamsRecordImpl (RLProxy :: _ paramList) params
    value <- case Object.lookup prop params of
      Nothing -> fail $ ErrorAtProperty prop $ ForeignError "missing query param"
      Just a -> read' a >>= decodeParam
    pure $ Builder.insert name value <<< builder
