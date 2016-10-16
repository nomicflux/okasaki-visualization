module Main where

import Views.Stack as Stack
import Prelude (bind, (/))
import Pux (start, renderToDOM)
import Signal ((~>))
import Signal.Time (every, second)

main = do
  app <- start
    { initialState: Stack.initModel
    , update: Stack.update
    , view: Stack.view
    , inputs: [ every (second / 60.0) ~> Stack.Tick ]
    }
  renderToDOM "#app" app.html
