module Main where

import Views.Stack as Stack
import Views.Queue as Queue
import CodeSnippet as CS
import Prelude (bind, (/), ($), (<$>), const, pure, (<<<), (==), (<>))
import Pux (start, renderToDOM, EffModel, noEffects, mapState, mapEffects)
import Pux.Html as H
import Pux.Html.Attributes as HA
import Pux.Html.Events as HE
import Control.Monad.Aff (Aff)
-- import Control.Monad.Eff (runPure)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Set (Set, insert, delete, empty, member)
import Debug.Trace (spy)
import Network.HTTP.Affjax (AJAX)
import Signal ((~>))
import Signal.Channel (CHANNEL)
import Signal.Time (every, second, Time())

data Page = StackPage | QueuePage

derive instance eqPage :: Eq Page

type State = { stackModel :: Stack.Model
             , queueModel :: Queue.Model
             , currPage :: Maybe Page
             , currLanguage :: CS.Language
             , availableLanguages :: Set CS.Language
             }

initialState :: State
initialState = { stackModel : Stack.initModel
               , queueModel : Queue.initModel
               , currPage : Nothing
               , currLanguage : CS.Purescript
               , availableLanguages : empty
               }

data Action = ChangePage Page
            | ChangeLanguage CS.Language
            | PageCheck CS.Language (Either String CS.SourceCode)
            | StackAction Stack.Action
            | QueueAction Queue.Action
            | Tick Time

updateStack :: State -> Stack.Action
            -> EffModel State Action (channel :: CHANNEL, err :: EXCEPTION, ajax :: AJAX)
updateStack state staction =
  let
    updated = Stack.update staction state.stackModel
  in
   mapEffects StackAction $ mapState (\s -> state { stackModel = s}) $ updated

updateQueue :: State -> Queue.Action
            -> EffModel State Action (channel :: CHANNEL, err :: EXCEPTION, ajax :: AJAX)
updateQueue state quaction =
  let
    updated = Queue.update quaction state.queueModel
  in
   mapEffects QueueAction $ mapState (\s -> state { queueModel = s}) $ updated

getSource :: forall eff. Page -> CS.Language -> Aff (ajax :: AJAX | eff) Action
getSource StackPage lang =
  StackAction <<< Stack.LoadCode <$> CS.getFile "Stack" lang
getSource QueuePage lang =
  QueueAction <<< Queue.LoadCode <$> CS.getFile "Queue" lang

checkSources :: forall eff. Page -> Array (Aff (ajax :: AJAX | eff) Action)
checkSources StackPage =
  map (\lang -> PageCheck lang <$> CS.getFile "Stack" lang) CS.allLangs
checkSources QueuePage =
  map (\lang -> PageCheck lang <$> CS.getFile "Queue" lang) CS.allLangs

update :: Action -> State
       -> EffModel State Action _
update (PageCheck lang (Right code)) state =
  let
    res = spy code
    res2 = spy (CS.parseFunctions code)
  in
   noEffects $ state { availableLanguages = insert lang state.availableLanguages}
update (PageCheck lang (Left err)) state =
  let
    res = spy err
  in
   noEffects $ state { availableLanguages = delete lang state.availableLanguages}
update (ChangeLanguage lang) state =
  case state.currPage of
    Nothing -> noEffects $ state { currLanguage = lang }
    Just page ->
      { state: state { currLanguage = lang }
      , effects: pure $ getSource page lang
      }
update (ChangePage page) state =
  { state: state { currPage = Just page }
  , effects: getSource page state.currLanguage : checkSources page
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

dsBtn :: State -> String -> Page -> H.Html Action
dsBtn state name token =
  let
    baseClasses = "pure-button pure-button-primary"
    allClasses =
      case state.currPage of
        Nothing -> baseClasses
        Just page -> if page == token
                     then baseClasses <> " pure-button-active"
                     else baseClasses
  in
   H.button [ HA.className allClasses
            , HE.onClick (const $ ChangePage token)
            ] [ H.text name ]

langBtn :: State -> String -> CS.Language -> H.Html Action
langBtn state name token =
  let
    baseClasses = "pure-button pure-button-danger"
    allClasses = if state.currLanguage == token
                 then baseClasses <> " pure-button-active"
                 else if member token state.availableLanguages
                      then baseClasses
                      else baseClasses <> " pure-button-disabled"
  in
   H.button [ HA.className allClasses
            , HE. onClick (const $ ChangeLanguage token)
            ] [ H.text name ]

view :: State -> H.Html Action
view state =
  let
    dbf = dsBtn state
    lbf = langBtn state
    dataDiv = H.div [ HA.className "pure-u-1-1" ] [ dbf "Stack" StackPage
                                                  , dbf "Queue" QueuePage ]
    langDiv = H.div [ HA.className "pure-u-1-1" ] [ lbf "Purescript" CS.Purescript
                                                  , lbf "Elm" CS.Elm
                                                  , lbf "Haskell" CS.Haskell
                                                  , lbf "Idris" CS.Idris
                                                  , lbf "Clojure" CS.Clojure
                                                  , lbf "Scheme" CS.Scheme
                                                  , lbf "Elixir" CS.Elixir
                                                  , lbf "Scala" CS.Scala
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
