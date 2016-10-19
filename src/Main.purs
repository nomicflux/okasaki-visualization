module Main where

import Views.Stack as Stack
import Views.Queue as Queue
import CodeSnippet as CS
import Prelude (bind, (/), ($), (<$>), const, pure, (<<<))
import Pux (start, renderToDOM, EffModel, noEffects, mapState, mapEffects)
import Pux.Html as H
import Pux.Html.Attributes as HA
import Pux.Html.Events as HE
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (runPure)
import Data.Maybe (Maybe(..))
import Signal ((~>))
import Signal.Time (every, second, Time())

data Page = StackPage | QueuePage

type State = { stackModel :: Stack.Model
             , queueModel :: Queue.Model
             , currPage :: Maybe Page
             }

initialState :: State
initialState = { stackModel : Stack.initModel
               , queueModel : Queue.initModel
               , currPage : Nothing
               }

data Action = ChangePage Page
            | StackAction Stack.Action
            | QueueAction Queue.Action
            | Tick Time
updateStack :: State -> Stack.Action -> EffModel State Action _
updateStack state staction =
  let
    updated = Stack.update staction state.stackModel
  in
   mapEffects StackAction $ mapState (\s -> state { stackModel = s}) $ updated

updateQueue :: State -> Queue.Action -> EffModel State Action _
updateQueue state quaction =
  let
    updated = Queue.update quaction state.queueModel
  in
   mapEffects QueueAction $ mapState (\s -> state { queueModel = s}) $ updated

update :: Action -> State -> EffModel State Action _
update (ChangePage page) state =
  { state: state { currPage = Just page }
  , effects: pure $ StackAction <<< Stack.LoadCode <$> CS.getFile "Stack"
  }
update (StackAction staction) state =
  updateStack state staction
update (QueueAction quaction) state =
  updateQueue state quaction
update (Tick time) state =
  case state.currPage of
    Nothing -> noEffects state
    Just StackPage ->
      updateStack state (Stack.Tick time)
    Just QueuePage ->
      updateQueue state (Queue.Tick time)

view :: State -> H.Html Action
view state =
  let
    stackBtn = H.button [ HA.className "pure-button pure-button-primary"
                        , HE.onClick (const $ ChangePage StackPage)] [ H.text "Stack" ]
    queueBtn = H.button [ HA.className "pure-button pure-button-primary"
                        , HE.onClick (const $ ChangePage QueuePage)] [ H.text "Queue" ]
    btnDiv = H.div [ HA.className "pure-u-1-1" ] [ stackBtn, queueBtn ]
    renderDiv =
      case state.currPage of
        Nothing -> H.div [ HA.className "pure-u-1-1" ] [ H.text "Please select a data structure"]
        Just StackPage -> StackAction <$> Stack.view state.stackModel
        Just QueuePage -> QueueAction <$> Queue.view state.queueModel
  in
   H.div [ ] [ btnDiv, renderDiv ]


main = do
  app <- start
    { initialState: initialState
    , update: update
    , view: view
    , inputs: [ every (second / 60.0) ~> Tick ]
    }
  renderToDOM "#app" app.html
