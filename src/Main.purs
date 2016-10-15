module Main where

import Views.Stack as Stack
import Prelude (bind)
import Pux (start, renderToDOM, fromSimple)

main = do
  app <- start
    { initialState: Stack.initModel
    , update: fromSimple Stack.update
    , view: Stack.view
    , inputs: []
    }
  renderToDOM "#app" app.html
