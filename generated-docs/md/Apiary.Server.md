## Module Apiary.Server


### Re-exported from Apiary.Server.Request:

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

#### `requestQuery`

``` purescript
requestQuery :: Request -> QueryParams
```

#### `readBodyAsString`

``` purescript
readBodyAsString :: Request -> Aff String
```

#### `readBodyAsBuffer`

``` purescript
readBodyAsBuffer :: Request -> Aff Buffer
```

### Re-exported from Apiary.Server.Response:

#### `StatusLineOpen`

``` purescript
data StatusLineOpen
```

#### `ResponseEnded`

``` purescript
data ResponseEnded
```

#### `Response`

``` purescript
newtype Response m from to a
  = Response (Response -> m a)
```

##### Instances
``` purescript
(Monad m) => IxFunctor (Response m)
(Monad m) => IxApply (Response m)
(Monad m) => IxApplicative (Response m)
(Monad m) => IxBind (Response m)
(Monad m) => IxMonad (Response m)
(Monad m) => Functor (Response m x x)
(Monad m) => Apply (Response m x x)
(Monad m) => Applicative (Response m x x)
(Monad m) => Bind (Response m x x)
(Monad m) => Monad (Response m x x)
(MonadEffect m) => MonadEffect (Response m x x)
(MonadAff m) => MonadAff (Response m x x)
(MonadThrow e m) => MonadThrow e (Response m x x)
(MonadError e m) => MonadError e (Response m x x)
```

#### `HeadersOpen`

``` purescript
data HeadersOpen
```

#### `Header`

``` purescript
type Header = Tuple String String
```

#### `FullResponse`

``` purescript
type FullResponse m = Response m StatusLineOpen ResponseEnded Unit
```

#### `BodyOpen`

``` purescript
data BodyOpen
```

#### `writeStatus`

``` purescript
writeStatus :: forall m. MonadEffect m => Status -> Response m StatusLineOpen HeadersOpen Unit
```

#### `writeHeader`

``` purescript
writeHeader :: forall m. MonadEffect m => String -> String -> Response m HeadersOpen HeadersOpen Unit
```

#### `withResponseStream`

``` purescript
withResponseStream :: forall m a. MonadEffect m => (Writable () -> m a) -> Response m BodyOpen ResponseEnded a
```

#### `send`

``` purescript
send :: forall m. MonadEffect m => String -> Response m BodyOpen ResponseEnded Unit
```

#### `runResponse`

``` purescript
runResponse :: forall m from to a. Response m from to a -> Response -> m a
```

#### `respondWithMedia`

``` purescript
respondWithMedia :: forall m rep a. MediaType rep => EncodeMedia rep a => MonadEffect m => Status -> Proxy rep -> a -> FullResponse m
```

#### `pure`

``` purescript
pure :: forall m a x. IxApplicative m => a -> m x x a
```

#### `map`

``` purescript
map :: forall f a b x y. IxFunctor f => (a -> b) -> f x y a -> f x y b
```

#### `headers`

``` purescript
headers :: forall f m. Foldable f => MonadEffect m => f Header -> Response m HeadersOpen BodyOpen Unit
```

#### `discard`

``` purescript
discard :: forall m a b x y z. IxBind m => IxDiscard a => m x y a -> (a -> m y z b) -> m x z b
```

#### `contentType`

``` purescript
contentType :: forall m. MonadEffect m => MediaType -> Response m HeadersOpen HeadersOpen Unit
```

#### `closeHeaders`

``` purescript
closeHeaders :: forall m. Monad m => Response m HeadersOpen BodyOpen Unit
```

#### `bind`

``` purescript
bind :: forall m a b x y z. IxMonad m => m x y a -> (a -> m y z b) -> m x z b
```

#### `apply`

``` purescript
apply :: forall m a b x y z. IxApply m => m x y (a -> b) -> m y z a -> m x z b
```

