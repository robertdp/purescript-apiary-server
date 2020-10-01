## Module Apiary.Server.Handler

#### `Handler`

``` purescript
newtype Handler m route
  = Handler { handler :: Request -> Response -> PathParams -> m Unit, route :: route }
```

#### `makeHandler`

``` purescript
makeHandler :: forall route params query body responder m. MonadAff m => DecodeRequest route params query body => BuildResponder route m responder => route -> (Request params query body -> responder -> FullResponse m) -> Handler m route
```

#### `sendMultipleErrors`

``` purescript
sendMultipleErrors :: forall m. MonadEffect m => MultipleErrors -> FullResponse m
```

#### `attachToRouter`

``` purescript
attachToRouter :: forall m route. AttachToRouter route => (m Unit -> Aff Unit) -> Handler m route -> Router Unit
```

#### `BuildResponder`

``` purescript
class BuildResponder route (m :: Type -> Type) responder | route m -> responder where
  buildResponder :: forall proxy2. route -> proxy2 m -> responder
```

##### Instances
``` purescript
(PrepareSpec spec { body :: body, path :: params, query :: query, response :: Record responses }, RowToList responses responseList, BuildResponderRecord responseList m responders) => BuildResponder (Route method path spec) m (Record responders)
```

#### `BuildResponderRecord`

``` purescript
class BuildResponderRecord (responses :: RowList) (m :: Type -> Type) (responders :: # Type) | responses m -> responders where
  buildResponderRecord :: forall proxy proxy2. proxy responses -> proxy2 m -> Builder (Record ()) (Record responders)
```

##### Instances
``` purescript
BuildResponderRecord Nil m ()
(IsSymbol status, ResponseStatus status, MediaType responseRep, EncodeMedia responseRep response, MonadEffect m, Lacks status responders', Cons status (response -> FullResponse m) responders' responders, BuildResponderRecord responseList m responders') => BuildResponderRecord (Cons status responseRep responseList) m responders
```


