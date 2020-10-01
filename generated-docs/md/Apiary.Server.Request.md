## Module Apiary.Server.Request

#### `Request`

``` purescript
type Request params query body = { body :: body, headers :: Object String, path :: params, query :: query }
```

#### `DecodeRequest`

``` purescript
class DecodeRequest route params query body | route -> params query body where
  decodeRequest :: route -> PathParams -> QueryParams -> String -> F (Request params query body)
```

##### Instances
``` purescript
(PrepareSpec spec { body :: body, path :: params, query :: query, response :: response }, DecodePathParams params, DecodeQueryParams query, DecodeMedia body body') => DecodeRequest (Route path method spec) params query body'
```

#### `readBodyAsBuffer`

``` purescript
readBodyAsBuffer :: Request -> Aff Buffer
```

#### `readBodyAsString`

``` purescript
readBodyAsString :: Request -> Aff String
```

#### `requestQuery`

``` purescript
requestQuery :: Request -> QueryParams
```

#### `coerceQuery`

``` purescript
coerceQuery :: Query -> QueryParams
```


