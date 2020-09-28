module Example.Interface.EchoTest where

import Apiary.Route (GET)
import Apiary.Types (JSON)

type EchoTest
  = GET "/echo/:first/:last"
      { params ::
          { first :: String
          , last :: String
          }
      , response ::
          { ok :: JSON { message :: String }
          }
      }
