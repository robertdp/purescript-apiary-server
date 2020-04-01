## Module Apiary.Server.Url

#### `PathParams`

``` purescript
type PathParams = Object String
```

#### `QueryParams`

``` purescript
type QueryParams = Object Foreign
```

#### `ReadParams`

``` purescript
class ReadParams pathParams queryParams params | pathParams queryParams -> params where
  readParams :: forall proxy. proxy pathParams -> proxy queryParams -> PathParams -> QueryParams -> F params
```

##### Instances
``` purescript
(Union pathParams queryParams mergedParams, RowToList pathParams pathParamList, ReadPathParams pathParams pathParamList, RowToList queryParamsRep queryParamList, DecodeQueryParams queryParams queryParamList) => ReadParams (Record pathParams) (Record queryParamsRep) (Record mergedParams)
```

#### `ReadPathParams`

``` purescript
class ReadPathParams (params :: # Type) (paramList :: RowList) | paramList -> params where
  readPathParams :: forall proxy. proxy paramList -> PathParams -> F (Builder (Record ()) (Record params))
```

##### Instances
``` purescript
ReadPathParams () Nil
(Cons name value params' params, ReadPathParams params' paramList, IsSymbol name, Lacks name params', DecodeParam value) => ReadPathParams params (Cons name value paramList)
```

#### `DecodeQueryParams`

``` purescript
class DecodeQueryParams (params :: # Type) (paramList :: RowList) | paramList -> params where
  decodeQueryParams :: forall proxy. proxy paramList -> QueryParams -> F (Builder (Record ()) (Record params))
```

##### Instances
``` purescript
DecodeQueryParams () Nil
(IsSymbol name, DecodeParam value, Unfoldable f, Cons name (f value) params' params, Lacks name params', DecodeQueryParams params' paramList) => DecodeQueryParams params (Cons name (f value) paramList)
(IsSymbol name, DecodeParam value, Cons name value params' params, Lacks name params', DecodeQueryParams params' paramList) => DecodeQueryParams params (Cons name value paramList)
```


