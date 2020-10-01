module Example.Interface.EchoTest where

import Apiary (GET, JSON)

type EchoTest
  = GET "/echo/:first/:last"
      { path ::
          { first :: String
          , last :: String
          }
      , response ::
          { ok :: JSON { message :: String }
          }
      }
