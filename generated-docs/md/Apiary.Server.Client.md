## Module Apiary.Server.Client

#### `makeRequest`

``` purescript
makeRequest :: forall route params query body rep response. BuildRequest route params query body rep => DecodeResponse rep response => route -> (Request -> Request) -> params -> query -> body -> Aff (Either Error response)
```

#### `fetch`

``` purescript
fetch :: Request -> ExceptT Error Aff Response
```


### Re-exported from Apiary.Types:

#### `Response`

``` purescript
type Response = { body :: String, headers :: Headers, status :: Int }
```

#### `Request`

``` purescript
type Request = { body :: String, headers :: Headers, method :: Method, url :: URL }
```

#### `Error`

``` purescript
data Error
  = RuntimeError Error
  | DecodeError Request Response MultipleErrors
  | UnexpectedResponse Request Response
```

##### Instances
``` purescript
Show Error
Semigroup Error
```

#### `emptyRequest`

``` purescript
emptyRequest :: Request
```

