module Components.Node where

import Prelude

import React as R
import React.DOM as R
import React.DOM.SVG as RS
import React.DOM.Props as RP
import Thermite as T

import Data.Lens
import Data.Maybe
import Data.Tuple
import Data.Either
import Data.List as L
import Data.Array

type State = { classes :: L.List String
             }

initialState :: State
initialState = { classes: L.Nil }

data Action = AddClass String
            | RemoveClass String
            | ChangeClass String String

render :: T.Render State _ Action
render dispatch _ state _ =
  [ RS.rect ( [ RP.className "node"
              , RP.unsafeMkProps "x" 300
              , RP.unsafeMkProps "y" 300
              , RP.unsafeMkProps "height" 100
              , RP.unsafeMkProps "width" 100
              , RP.unsafeMkProps "fill" "black"
              ]
              <> (fromFoldable $ map (\c -> RP.className c) state.classes)
            )
    [ ]
  ]

performAction :: T.PerformAction _ State _ Action
performAction (AddClass newClass) _ _ = void do
  T.cotransform \state -> state { classes = L.snoc state.classes newClass }
performAction (RemoveClass rmClass) _ _ = void do
  T.cotransform \state -> state { classes = L.filter (\c -> c /= rmClass ) state.classes }
performAction (ChangeClass chgClass newClass) _ _ = void do
  T.cotransform \state -> state { classes = map (\c -> if c == chgClass then newClass else c) state.classes }

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render
