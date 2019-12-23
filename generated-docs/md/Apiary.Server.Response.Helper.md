## Module Apiary.Server.Response.Helper

#### `BuildResponder`

``` purescript
class BuildResponder route m responder | route m -> responder where
  buildResponder :: route -> Proxy2 m -> responder
```

##### Instances
``` purescript
(PrepareSpec spec { body :: body, params :: params, query :: query, response :: Record responses }, RowToList responses responseList, BuildResponderRecord responseList m responders) => BuildResponder (Route method path spec) m (Record responders)
```

#### `BuildResponderRecord`

``` purescript
class BuildResponderRecord (responses :: RowList) (m :: Type -> Type) (responders :: # Type) | responses m -> responders where
  buildResponderRecord :: RLProxy responses -> Proxy2 m -> Builder (Record ()) (Record responders)
```

##### Instances
``` purescript
BuildResponderRecord Nil m ()
(IsSymbol status, ResponseStatus status, MediaType responseRep, EncodeMedia responseRep response, MonadEffect m, Lacks status responders', Cons status (response -> FullResponse m) responders' responders, BuildResponderRecord responseList m responders') => BuildResponderRecord (Cons status responseRep responseList) m responders
```

