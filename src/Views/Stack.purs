module Views.Stack where

import Pux.Html as H
import Pux.Html.Attributes as HA
import Pux.Html.Events as HE
import Structures.Purs.Stack as S
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, EXCEPTION)
import Data.Array ((:), concatMap, fromFoldable)
import Data.Foldable (class Foldable, foldr)
import Data.Int (fromString, toNumber)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Tuple (Tuple(..), snd)
import Prelude (($), (+), (/), (-), (*), (<), (<>), (<<<), const, min, (<$>), bind, pure)
import Pux (EffModel, noEffects)
import Signal ((~>))
import Signal.Channel (CHANNEL)
import Signal.Time (now)

import Views.Animation (Animation, AnimationAction(..), defaultAnimation, resetAnimation, getPhase, updateAnimation)
import Views.Node (NodeMap, NodeValue, NodeID, Node(..), maxWidth, maxHeight, viewNodePos, wipeClasses, changeAllClasses, changeClass, buffer, maxRadius)
import Views.SourceCode (CodeAction, SourceCodeInfo, sourceBtn, changeFn, updateCode, blankSourceCode)


type Model = { stack :: S.Stack Node
             , currId :: NodeID
             , currInput :: Maybe NodeValue
             , currNodes :: NodeMap
             , prevNodes :: NodeMap
             , animation :: Animation
             , code :: SourceCodeInfo
             }

initModel :: Model
initModel = { stack : S.empty
            , currId : 0
            , currInput : Nothing
            , currNodes : M.empty
            , prevNodes : M.empty
            , animation : defaultAnimation
            , code : blankSourceCode
            }

mkNode :: Model -> Maybe (Tuple Node Model)
mkNode model =
  case model.currInput of
    Nothing -> Nothing
    Just val ->
      let
        mlastNode = S.head model.stack
        connections =
          case mlastNode of
            Nothing -> []
            Just (Node n) -> [n.id]
        newNode = Node { value : val
                       , classes : "cons"
                       , id : model.currId
                       , connections : connections
                       }
        newModel = model { currId = model.currId + 1 }
      in
       Just $ Tuple newNode newModel

mkNodePos :: Int -> Node -> Tuple Int NodeMap -> Tuple Int NodeMap
mkNodePos total (Node node) (Tuple pos acc) =
  let
    ftotal = toNumber total
    fpos = toNumber pos
    r = min maxRadius (maxWidth / 3.0 / ftotal)
    offset x = x + buffer + r
    calcPos x = maxWidth - (offset $ x * maxWidth)
    circPos = { x : calcPos $ fpos / ftotal
              , y : maxHeight / 2.0
              , r : r
              , value : node.value
              , connections : node.connections
              , classes : node.classes
              }
  in
   Tuple (pos + 1) (M.insert node.id circPos acc)

getNodeMap :: forall f. Foldable f => Int -> f Node -> NodeMap
getNodeMap total stack = snd $ foldr (mkNodePos total) (Tuple 0 M.empty) stack

data Action = Empty
            | Head
            | Tail
            | Pop
            | Reverse
            | Insert
            | CurrentInput String
            | ShowStructure
            | Animate AnimationAction
            | Code CodeAction

updateStack :: forall eff. Model -> S.Stack Node -> String -> EffModel Model Action _
updateStack model stack fn =
  let
    ct = S.count stack
    newMap = getNodeMap ct stack
    newSource = changeFn model.code fn
  in
   { state: model { prevNodes = model.currNodes
                  , currNodes = newMap
                  , stack = stack
                  , animation = resetAnimation model.animation
                  , code = newSource
                  }
   , effects: [ do
      time <- liftEff now
      pure $ Animate (StartTimer time)
              ]
   }

update :: forall eff. Action -> Model -> EffModel Model Action (channel :: CHANNEL, err :: EXCEPTION | eff)
update (Animate action) model =
  noEffects $ model { animation = updateAnimation action model.animation }
update (Code action) model =
  let
    newCode = updateCode action model.code
  in
   noEffects $ model { code = newCode }
update Empty model =
  updateStack model S.empty "empty"
update Head model =
  let
    mhead = S.head model.stack
    mtail = S.tail model.stack
  in
   case Tuple mhead mtail of
     Tuple Nothing _ -> noEffects $ model { code = changeFn model.code "head" }
     Tuple _ Nothing -> noEffects $ model { code = changeFn model.code "head" }
     Tuple (Just h) (Just t) ->
       let
         newHead = changeClass "head" h
         newTail = wipeClasses t
         newStack = S.cons newHead newTail
       in
        updateStack model newStack "head"
update Tail model =
  let
    mhead = S.head model.stack
    mtail = S.tail model.stack
  in
   case Tuple mhead mtail of
     Tuple Nothing _ -> noEffects $ model { code = changeFn model.code "tail" }
     Tuple _ Nothing -> noEffects $ model { code = changeFn model.code "tail" }
     Tuple (Just h) (Just t) ->
       let
         newHead = changeClass "" h
         newTail = changeAllClasses "tail" t
         newStack = S.cons newHead newTail
       in
        updateStack model newStack "tail"
update Pop model =
  let
    mtail = S.tail model.stack
  in
   case mtail of
     Nothing -> noEffects $ model { code = changeFn model.code "tail" }
     Just t ->
       let
         newTail = changeAllClasses "tail" t
         newStack = newTail
       in
        updateStack model newStack "tail"
update Reverse model =
  updateStack model (S.reverse model.stack) "reverse"
update Insert model =
  let
    mnode = mkNode model
    cleanStack = wipeClasses model.stack
  in
   case mnode of
     Nothing -> noEffects $ model { code = changeFn model.code "cons" }
     Just (Tuple node newModel) ->
       updateStack newModel (S.cons node cleanStack) "cons"
update (CurrentInput s) model =
  noEffects $ model { currInput = fromString s }
update ShowStructure model =
  noEffects $ model { code = changeFn model.code "Stack" }

viewCtrl :: Model -> H.Html Action
viewCtrl model =
   let
    dataBtn = H.div [ ] [ H.button [ HA.className "pure-button pure-button-warning"
                                   , HE.onClick $ const ShowStructure
                                   ] [ H.text "Stack Structure" ]
                        ]
    emptyBtn = H.div [ ] [ H.button [ HA.className "pure-button"
                                    , HE.onClick $ const Empty
                                    ] [ H.text "Empty" ]
                         ]
    headBtn = H.button [ HA.className "pure-button"
                                   , HE.onClick $ const Head
                                   ] [ H.text "Head" ]
    tailBtn = H.button [ HA.className "pure-button"
                                   , HE.onClick $ const Tail
                                   ] [ H.text "Tail" ]
    viewsDiv = H.div [ ] [ headBtn, tailBtn ]
    revBtn = H.div [ ] [ H.button [ HA.className "pure-button"
                                  , HE.onClick $ const Reverse
                                  ] [ H.text "Reverse" ]
                       ]
    popBtn = H.div [ ] [ H.button [ HA.className "pure-button"
                                  , HE.onClick $ const Pop
                                  ] [ H.text "Pop" ]
                       ]
    consSpan = H.div [ ] [ H.span [ ] [ H.button [ HA.className "pure-button"
                                                 , HE.onClick $ const Insert
                                                 ] [ H.text "Cons"]
                                      , H.input [ HA.type_ "number"
                                                , HA.maxLength "3"
                                                , HA.size 3
                                                , HE.onChange $ \t -> CurrentInput t.target.value
                                                ] [ ]]
                         ]
   in
    H.div [ ] [ Code <$> sourceBtn
              , dataBtn
              , emptyBtn
              , viewsDiv
              , revBtn
              , popBtn
              , consSpan
              ]

viewModel :: Model -> H.Html Action
viewModel model =
  let
    keys = M.keys $ M.union model.prevNodes model.currNodes
    showNodes = viewNodePos (getPhase model.animation) model.prevNodes model.currNodes
    nodes = concatMap showNodes (fromFoldable keys)
  in
    H.div [ ]
          [ H.svg [HA.height (show maxHeight)
                  , HA.width (show maxWidth)  ] nodes
          ]
