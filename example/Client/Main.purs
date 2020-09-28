module Example.Client.Main where

import Prelude
import Example.Client.EchoTest (echoTest)
import Data.Foldable (for_)
import Data.Variant (match)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console

main :: Effect Unit
main =
  launchAff_ do
    res <- echoTest "Tom" "Jones"
    for_ res
      $ match
          { ok:
            \{ message } -> do
              Console.log "Success!"
              Console.log message
          }
