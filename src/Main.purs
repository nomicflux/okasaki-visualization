module Main where

import Views.Stack as Stack
import Views.Queue as Queue
import Views.Set as Set
import Views.Leftist as Leftist
import CodeSnippet as CS
import Prelude (bind, (/), ($), (<$>), const, pure, (<<<), (==), (<>), Unit)
import Pux (start, renderToDOM, EffModel, noEffects, mapState, mapEffects)
import Pux.Html as H
import Pux.Html.Attributes as HA
import Pux.Html.Events as HE
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
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

data Page = StackPage
          | QueuePage
          | SetPage
          | LeftistPage

derive instance eqPage :: Eq Page

type State = { stackModel :: Stack.Model
             , queueModel :: Queue.Model
             , setModel :: Set.Model
             , leftistModel :: Leftist.Model
             , currPage :: Maybe Page
             , currLanguage :: CS.Language
             , availableLanguages :: Set CS.Language
             }

initialState :: State
initialState = { stackModel : Stack.initModel
               , queueModel : Queue.initModel
               , setModel : Set.initModel
               , leftistModel : Leftist.initModel
               , currPage : Nothing
               , currLanguage : CS.Purescript
               , availableLanguages : empty
               }

data Action = ChangePage Page
            | ChangeLanguage CS.Language
            | PageCheck CS.Language (Either String CS.SourceCode)
            | StackAction Stack.Action
            | QueueAction Queue.Action
            | SetAction Set.Action
            | LeftistAction Leftist.Action
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

updateSet :: State -> Set.Action
          -> EffModel State Action (channel :: CHANNEL, err :: EXCEPTION, ajax :: AJAX)
updateSet state saction =
  let
    updated = Set.update saction state.setModel
  in
   mapEffects SetAction $ mapState (\s -> state { setModel = s}) $ updated

updateLeftist :: State -> Leftist.Action
          -> EffModel State Action (channel :: CHANNEL, err :: EXCEPTION, ajax :: AJAX)
updateLeftist state laction =
  let
    updated = Leftist.update laction state.leftistModel
  in
   mapEffects LeftistAction $ mapState (\s -> state { leftistModel = s}) $ updated

loadAction :: Page -> (Either String CS.SourceCode -> Action)
loadAction StackPage = StackAction <<< Stack.LoadCode
loadAction QueuePage = QueueAction <<< Queue.LoadCode
loadAction SetPage = SetAction <<< Set.LoadCode
loadAction LeftistPage = LeftistAction <<< Leftist.LoadCode

fileName :: Page -> String
fileName StackPage = "Stack"
fileName QueuePage = "Queue"
fileName SetPage = "Set"
fileName LeftistPage = "Leftist"

getSource :: forall eff. Page -> CS.Language -> Aff (ajax :: AJAX | eff) Action
getSource page lang =
  (loadAction page) <$> CS.getFile (fileName page) lang

checkSources :: forall eff. Page -> Array (Aff (ajax :: AJAX | eff) Action)
checkSources page =
  map (\lang -> PageCheck lang <$> CS.getFile (fileName page) lang) CS.allLangs

update :: Action -> State
       -> EffModel State Action _
update (PageCheck lang (Right code)) state =
   noEffects $ state { availableLanguages = insert lang state.availableLanguages}
update (PageCheck lang (Left err)) state =
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
update (SetAction saction) state =
  updateSet state saction
update (LeftistAction laction) state =
  updateLeftist state laction
update (Tick time) state =
  case state.currPage of
    Nothing -> noEffects state
    Just StackPage ->
      updateStack state (Stack.Tick time)
    Just QueuePage ->
      updateQueue state (Queue.Tick time)
    Just SetPage ->
      updateSet state (Set.Tick time)
    Just LeftistPage ->
      updateLeftist state (Leftist.Tick time)

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
    dataDiv = H.div [ HA.className "pure-u-1-1" ] [ dbf "Stack / List" StackPage
                                                  , dbf "Queue" QueuePage
                                                  , dbf "Set / Binary Tree" SetPage
                                                  , dbf "Leftist Heap" LeftistPage
                                                  ]
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
        Just SetPage -> SetAction <$> Set.view state.setModel
        Just LeftistPage -> LeftistAction <$> Leftist.view state.leftistModel
  in
   H.div [ ] [ dataDiv, langDiv, renderDiv ]

main :: Eff (channel :: CHANNEL, ajax :: AJAX, err :: EXCEPTION) Unit
main = do
  app <- start
    { initialState: initialState
    , update: update
    , view: view
    , inputs: [ every (second / 60.0) ~> Tick ]
    }
  renderToDOM "#app" app.html
