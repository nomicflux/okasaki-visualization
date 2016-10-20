module Main where

import Views.Stack as Stack
import Views.Queue as Queue
import CodeSnippet as CS
import Prelude (bind, (/), ($), (<$>), const, pure, (<<<))
import Pux (start, renderToDOM, EffModel, noEffects, mapState, mapEffects)
import Pux.Html as H
import Pux.Html.Attributes as HA
import Pux.Html.Events as HE
import Control.Monad.Aff (runAff, Aff)
import Control.Monad.Eff (runPure)
import Data.Maybe (Maybe(..))
import Signal ((~>))
import Signal.Time (every, second, Time())

data Page = StackPage | QueuePage

type State = { stackModel :: Stack.Model
             , queueModel :: Queue.Model
             , currPage :: Maybe Page
             , currLanguage :: CS.Language
             }

initialState :: State
initialState = { stackModel : Stack.initModel
               , queueModel : Queue.initModel
               , currPage : Nothing
               , currLanguage : CS.Purescript
               }

data Action = ChangePage Page
            | ChangeLanguage CS.Language
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

-- getSource :: Page -> CS.Language -> Aff _ Action
getSource StackPage lang =
  pure $ StackAction <<< Stack.LoadCode <$> CS.getFile "Stack" lang
getSource QueuePage lang =
  pure $ QueueAction <<< Queue.LoadCode <$> CS.getFile "Queue" lang

update :: Action -> State -> EffModel State Action _
update (ChangeLanguage lang) state =
  case state.currPage of
    Nothing -> noEffects $ state { currLanguage = lang }
    Just page ->
      { state: state { currLanguage = lang }
      , effects: getSource page lang
      }
update (ChangePage page) state =
  { state: state { currPage = Just page }
  , effects: getSource page state.currLanguage
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

dsBtn :: String -> Page -> H.Html Action
dsBtn name token = H.button [ HA.className "pure-button pure-button-primary"
                            , HE.onClick (const $ ChangePage token)
                            ] [ H.text name ]

langBtn :: String -> CS.Language -> H.Html Action
langBtn name token = H.button [ HA.className "pure-button pure-button-danger"
                              , HE. onClick (const $ ChangeLanguage token)
                              ] [ H.text name ]

view :: State -> H.Html Action
view state =
  let
    dataDiv = H.div [ HA.className "pure-u-1-1" ] [ dsBtn "Stack" StackPage
                                                  , dsBtn "Queue" QueuePage ]
    langDiv = H.div [ HA.className "pure-u-1-1" ] [ langBtn "Purescript" CS.Purescript
                                                  , langBtn "Elm" CS.Elm
                                                  , langBtn "Haskell" CS.Haskell
                                                  , langBtn "Idris" CS.Idris
                                                  , langBtn "Clojure" CS.Clojure
                                                  , langBtn "Scheme" CS.Scheme
                                                  , langBtn "Elixir" CS.Elixir
                                                  ]
    renderDiv =
      case state.currPage of
        Nothing -> H.div [ HA.className "pure-u-1-1" ] [ H.text "Please select a data structure"]
        Just StackPage -> StackAction <$> Stack.view state.stackModel
        Just QueuePage -> QueueAction <$> Queue.view state.queueModel
  in
   H.div [ ] [ dataDiv, langDiv, renderDiv ]

main = do
  app <- start
    { initialState: initialState
    , update: update
    , view: view
    , inputs: [ every (second / 60.0) ~> Tick ]
    }
  renderToDOM "#app" app.html
