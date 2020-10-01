## Module Apiary.Server.Url

#### `PathParams`

``` purescript
type PathParams = Object String
```

#### `QueryParams`

``` purescript
type QueryParams = Object Foreign
```

#### `DecodePathParams`

``` purescript
class DecodePathParams params  where
  decodePathParams :: forall proxy. proxy params -> PathParams -> F params
```

##### Instances
``` purescript
DecodePathParams None
(RowToList r rl, DecodePathParamsRecord r rl) => DecodePathParams (Record r)
```

#### `DecodeQueryParams`

``` purescript
class DecodeQueryParams params  where
  decodeQueryParams :: forall proxy. proxy params -> QueryParams -> F params
```

##### Instances
``` purescript
DecodeQueryParams None
(RowToList r rl, DecodeQueryParamsRecord r rl) => DecodeQueryParams (Record r)
```

#### `DecodePathParamsRecord`

``` purescript
class DecodePathParamsRecord (params :: # Type) (paramList :: RowList) | paramList -> params where
  decodePathParamsRecordImpl :: forall proxy. proxy paramList -> PathParams -> F (Builder (Record ()) (Record params))
```

##### Instances
``` purescript
DecodePathParamsRecord () Nil
(Cons name value params' params, DecodePathParamsRecord params' paramList, IsSymbol name, Lacks name params', DecodeParam value) => DecodePathParamsRecord params (Cons name value paramList)
```

#### `DecodeQueryParamsRecord`

``` purescript
class DecodeQueryParamsRecord (params :: # Type) (paramList :: RowList) | paramList -> params where
  decodeQueryParamsRecordImpl :: forall proxy. proxy paramList -> QueryParams -> F (Builder (Record ()) (Record params))
```

##### Instances
``` purescript
DecodeQueryParamsRecord () Nil
(IsSymbol name, DecodeParam value, Unfoldable f, Cons name (f value) params' params, Lacks name params', DecodeQueryParamsRecord params' paramList) => DecodeQueryParamsRecord params (Cons name (f value) paramList)
(IsSymbol name, DecodeParam value, Cons name value params' params, Lacks name params', DecodeQueryParamsRecord params' paramList) => DecodeQueryParamsRecord params (Cons name value paramList)
```


