module Main where

import CodeSnippet as CS
import Pux.Html as H
import Pux.Html.Attributes as HA
import Pux.Html.Events as HE
import Views.Leftist as Leftist
import Views.Queue as Queue
import Views.Set as Set
import Views.Stack as Stack
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array ((:), catMaybes)
import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Set (Set, insert, delete, empty, member)
import Data.Show (show, class Show)
import Network.HTTP.Affjax (AJAX)
import Prelude (bind, (/), ($), (<$>), pure, (<<<), (==), (<>), Unit)
import Pux (start, renderToDOM, EffModel, noEffects, mapState, mapEffects)
import Signal ((~>))
import Signal.Channel (CHANNEL)
import Signal.Time (every, second, Time)

import Views.Animation as A
import Views.SourceCode as SC

data Page = StackPage
          | QueuePage
          | SetPage
          | LeftistPage

instance showPage :: Show Page where
  show StackPage = "StackPage"
  show QueuePage = "QueuePage"
  show SetPage = "SetPage"
  show LeftistPage = "LeftistPage"

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

data Action = ChangePage (Maybe Page)
            | ChangeLanguage (Maybe CS.Language)
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

loadAction :: Page -> (SC.CodeAction -> Action)
loadAction StackPage = StackAction <<< Stack.Code
loadAction QueuePage = QueueAction <<< Queue.Code
loadAction SetPage = SetAction <<< Set.Code
loadAction LeftistPage = LeftistAction <<< Leftist.Code

fileName :: Page -> String
fileName StackPage = "Stack"
fileName QueuePage = "Queue"
fileName SetPage = "Set"
fileName LeftistPage = "Leftist"

getSource :: forall eff. Page -> CS.Language -> Aff (ajax :: AJAX | eff) Action
getSource page lang =
  (loadAction page <<< SC.LoadCode) <$> CS.getFile (fileName page) lang

checkSources :: forall eff. Page -> Array (Aff (ajax :: AJAX | eff) Action)
checkSources page =
  map (\lang -> PageCheck lang <$> CS.getFile (fileName page) lang) CS.allLangs

update :: Action -> State
       -> EffModel State Action (ajax :: AJAX)
update (PageCheck lang (Right code)) state =
  noEffects $ state { availableLanguages = insert lang state.availableLanguages}
update (PageCheck lang (Left err)) state =
  noEffects $ state { availableLanguages = delete lang state.availableLanguages}
update (ChangeLanguage Nothing) state =
  noEffects state
update (ChangeLanguage (Just lang)) state =
  case state.currPage of
    Nothing -> noEffects $ state { currLanguage = lang }
    Just page ->
      { state: state { currLanguage = lang }
      , effects: pure $ getSource page lang
      }
update (ChangePage Nothing) state =
  noEffects $ state { currPage = Nothing }
update (ChangePage (Just page)) state =
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
      updateStack state (Stack.Animate $ A.Tick time)
    Just QueuePage ->
      updateQueue state (Queue.Animate $ A.Tick time)
    Just SetPage ->
      updateSet state (Set.Animate $ A.Tick time)
    Just LeftistPage ->
      updateLeftist state (Leftist.Animate $ A.Tick time)

dsOption :: State -> String -> Maybe Page -> H.Html Action
dsOption state name token =
  let
    value =
      case token of
        Nothing -> ""
        Just page -> show page
  in
   H.option [ HA.value value
            ] [ H.text name ]

langOption :: State -> String -> Maybe CS.Language -> Maybe (H.Html Action)
langOption _ _ Nothing = Just $ H.option [ HA.value "" ] [ H.text "" ]
langOption state name (Just lang) =
  if member lang state.availableLanguages
  then
    let
      value = CS.suffix lang
    in
     Just $ H.option [ HA.value value
                     ] [ H.text name ]
  else Nothing

stringToPage :: String -> Maybe Page
stringToPage str =
  case str of
    "StackPage" -> Just StackPage
    "QueuePage" -> Just QueuePage
    "SetPage" -> Just SetPage
    "LeftistPage" -> Just LeftistPage
    _ -> Nothing

view :: State -> H.Html Action
view state =
  let
    dbf = dsOption state
    lbf = langOption state
    dataDiv = H.div [ HE.onChange (\page -> ChangePage $ stringToPage page.target.value)]
                    [ H.label [ HA.htmlFor "structure" ] [ H.text "Structure: "]
                    , H.select [ HA.name "structure" ]
                      [ dbf "" Nothing
                      , dbf "Stack / List" (Just StackPage)
                      , dbf "Queue" (Just QueuePage)
                      , dbf "Set / Binary Tree" (Just SetPage)
                      , dbf "Leftist Heap" (Just LeftistPage)
                      ]
                    ]
    langDiv = H.div [ HE.onChange (\lang -> ChangeLanguage $ CS.stringToLang lang.target.value)]
                    [ H.label [ HA.htmlFor "language" ] [ H.text "Language: " ]
                    , H.select [ HA.name "language" ]
                      (catMaybes
                       [ lbf "" Nothing
                       , lbf "Purescript" $ Just CS.Purescript
                       , lbf "Elm" $ Just CS.Elm
                       , lbf "Haskell" $ Just CS.Haskell
                       , lbf "Idris" $ Just CS.Idris
                       , lbf "Clojure" $ Just CS.Clojure
                       , lbf "Scheme" $ Just CS.Scheme
                       , lbf "Elixir" $ Just CS.Elixir
                       , lbf "Scala" $ Just CS.Scala
                       ])
                    ]
    blank = H.div [ ] [ ]
    pageBtns =
      case state.currPage of
        Nothing -> blank
        Just StackPage -> StackAction <$> Stack.viewCtrl state.stackModel
        Just QueuePage -> QueueAction <$> Queue.viewCtrl state.queueModel
        Just SetPage -> SetAction <$> Set.viewCtrl state.setModel
        Just LeftistPage -> LeftistAction <$> Leftist.viewCtrl state.leftistModel
    modelRendered =
      case state.currPage of
        Nothing -> blank
        Just StackPage -> StackAction <$> Stack.viewModel state.stackModel
        Just QueuePage -> QueueAction <$> Queue.viewModel state.queueModel
        Just SetPage -> SetAction <$> Set.viewModel state.setModel
        Just LeftistPage -> LeftistAction <$> Leftist.viewModel state.leftistModel
    codeSnippets =
      case state.currPage of
        Nothing -> blank
        Just StackPage -> (StackAction <<< Stack.Code) <$> SC.viewCode state.stackModel.code
        Just QueuePage -> (QueueAction <<< Queue.Code) <$> SC.viewCode state.queueModel.code
        Just SetPage -> (SetAction <<< Set.Code) <$> SC.viewCode state.setModel.code
        Just LeftistPage -> (LeftistAction <<< Leftist.Code) <$> SC.viewCode state.leftistModel.code
    sideBar = H.div [ HA.className "pure-u-1-4" ] [ H.div [ HA.className "sidebar" ]
                                                    [ dataDiv
                                                    , langDiv
                                                    , pageBtns
                                                    ]
                                                  ]
    render = H.div [ HA.className "pure-u-3-4 render" ]
             [ H.div [ HA.className "render" ] [ modelRendered ]
             , H.div [ HA.className "code-snippet" ] [ codeSnippets ]
             ]
  in
   H.div [ HA.className "pure-g pure-container" ] [ sideBar
                                                  , render
                                                  ]

main :: Eff (channel :: CHANNEL, ajax :: AJAX, err :: EXCEPTION) Unit
main = do
  app <- start
    { initialState: initialState
    , update: update
    , view: view
    , inputs: [ every (second / 60.0) ~> Tick ]
    }
  renderToDOM "#app" app.html
