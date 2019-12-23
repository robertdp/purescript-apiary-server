## Module Apiary.Server.Response

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
IxMonadTrans Response
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

#### `runResponse`

``` purescript
runResponse :: forall m from to a. Response m from to a -> Response -> m a
```

#### `StatusLineOpen`

``` purescript
data StatusLineOpen
```

#### `HeadersOpen`

``` purescript
data HeadersOpen
```

#### `BodyOpen`

``` purescript
data BodyOpen
```

#### `ResponseEnded`

``` purescript
data ResponseEnded
```

#### `Header`

``` purescript
type Header = Tuple String String
```

#### `FullResponse`

``` purescript
type FullResponse m = Response m StatusLineOpen ResponseEnded Unit
```

#### `writeStatus`

``` purescript
writeStatus :: forall m. MonadEffect m => Status -> Response m StatusLineOpen HeadersOpen Unit
```

#### `writeHeader`

``` purescript
writeHeader :: forall m. MonadEffect m => String -> String -> Response m HeadersOpen HeadersOpen Unit
```

#### `closeHeaders`

``` purescript
closeHeaders :: forall m. Monad m => Response m HeadersOpen BodyOpen Unit
```

#### `headers`

``` purescript
headers :: forall f m. Foldable f => MonadEffect m => f Header -> Response m HeadersOpen BodyOpen Unit
```

#### `contentType`

``` purescript
contentType :: forall m. MonadEffect m => MediaType -> Response m HeadersOpen HeadersOpen Unit
```

#### `withResponseStream`

``` purescript
withResponseStream :: forall m a. MonadEffect m => (Writable () -> m a) -> Response m BodyOpen ResponseEnded a
```

#### `send`

``` purescript
send :: forall m. MonadEffect m => String -> Response m BodyOpen ResponseEnded Unit
```

#### `respondWithMedia`

``` purescript
respondWithMedia :: forall m rep a. MediaType rep => EncodeMedia rep a => MonadEffect m => Status -> Proxy rep -> a -> FullResponse m
```


### Re-exported from Control.Monad.Indexed.Qualified:

#### `pure`

``` purescript
pure :: forall m a x. IxApplicative m => a -> m x x a
```

#### `map`

``` purescript
map :: forall f a b x y. IxFunctor f => (a -> b) -> f x y a -> f x y b
```

#### `discard`

``` purescript
discard :: forall m a b x y z. IxBind m => IxDiscard a => m x y a -> (a -> m y z b) -> m x z b
```

#### `bind`

``` purescript
bind :: forall m a b x y z. IxMonad m => m x y a -> (a -> m y z b) -> m x z b
```

#### `apply`

``` purescript
apply :: forall m a b x y z. IxApply m => m x y (a -> b) -> m y z a -> m x z b
```

### Re-exported from Control.Monad.Indexed.Trans.Qualified:

#### `lift`

``` purescript
lift :: forall m i a t. IxMonadTrans t => Monad m => m a -> t m i i a
```

