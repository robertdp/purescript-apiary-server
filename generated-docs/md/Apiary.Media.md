## Module Apiary.Media

#### `DecodeMedia`

``` purescript
class DecodeMedia rep a | rep -> a where
  decodeMedia :: Proxy rep -> String -> F a
```

##### Instances
``` purescript
DecodeMedia None None
DecodeMedia String String
(ReadForeign a) => DecodeMedia (JSON a) a
```

#### `EncodeMedia`

``` purescript
class EncodeMedia rep a | rep -> a where
  encodeMedia :: Proxy rep -> a -> String
```

##### Instances
``` purescript
EncodeMedia None None
EncodeMedia String String
(WriteForeign a) => EncodeMedia (JSON a) a
```

#### `MediaType`

``` purescript
class MediaType rep  where
  mediaType :: Proxy rep -> Maybe MediaType
```

##### Instances
``` purescript
MediaType None
MediaType String
MediaType (JSON a)
```

#### `JSON`

``` purescript
data JSON a
```

##### Instances
``` purescript
MediaType (JSON a)
(WriteForeign a) => EncodeMedia (JSON a) a
(ReadForeign a) => DecodeMedia (JSON a) a
```

#### `None`

``` purescript
data None :: Type
```

##### Instances
``` purescript
Show None
Semigroup None
Monoid None
MediaType None
EncodeMedia None None
DecodeMedia None None
```

#### `none`

``` purescript
none :: None
```


