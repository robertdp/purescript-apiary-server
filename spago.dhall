{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "apiary-server"
, dependencies =
    [ "avar", "indexed-monad", "media-types", "milkis", "node-http", "simple-json" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
