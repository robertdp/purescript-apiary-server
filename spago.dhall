{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "apiary-server"
, dependencies =
    [ "apiary", "avar", "indexed-monad", "node-http" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
