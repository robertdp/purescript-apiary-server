## Module Apiary.Server

#### `Handler`

``` purescript
newtype Handler m route
  = Handler { handler :: Request -> Response -> PathParams -> m Unit, route :: route }
```

#### `makeHandler`

``` purescript
makeHandler :: forall route params body responder m. MonadAff m => DecodeRequest route params body => BuildResponder route m responder => route -> (Request params body -> responder -> FullResponse m) -> Handler m route
```

#### `sendMultipleErrors`

``` purescript
sendMultipleErrors :: forall m. MonadEffect m => MultipleErrors -> FullResponse m
```

#### `attachToRouter`

``` purescript
attachToRouter :: forall m route. AttachToRouter route => (m Unit -> Aff Unit) -> Handler m route -> Router Unit
```


