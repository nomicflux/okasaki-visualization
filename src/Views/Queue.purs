module Views.Queue where

import Data.Map as M
import Pux.DOM.HTML as H
import Pux.DOM.HTML.Attributes as HA
import Pux.DOM.Events as HE
import Structures.Purs.Queue as Q
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array ((:))
import Data.Foldable (foldr)
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (($), (+), (/), (-), (*), (<), (<>), (<<<), const, min, (<$>), bind, pure)
import Pux (EffModel, noEffects)
import Signal.Channel (CHANNEL)
import Signal.Time (now)

import Views.Animation (Animation, AnimationAction(..), defaultAnimation, resetAnimation, updateAnimation)
import Views.Node (NodeMap, Node(..), maxWidth, maxHeight, wipeClasses, changeAllClasses, changeClass, buffer, maxRadius, NodeData, blankNodes, getNextId, updateNodes, incId, getInput, setInput)
import Views.SourceCode (CodeAction, SourceCodeInfo, sourceBtn, changeFn, updateCode, blankSourceCode)

type Model = { queue :: Q.Queue Node
             , nodes :: NodeData
             , animation :: Animation
             , code :: SourceCodeInfo
             }

initModel :: Model
initModel = { queue : Q.empty
            , nodes : blankNodes
            , animation : defaultAnimation
            , code : blankSourceCode
            }

data Direction = Front | Back

mkNode :: Direction -> Model -> Maybe (Tuple Node Model)
mkNode dir model =
  case getInput model.nodes of
    Nothing -> Nothing
    Just val ->
      let
        mlastNode =
          case dir of
            Front -> Q.topHead model.queue
            Back -> Q.backHead model.queue
        connections =
          case mlastNode of
            Nothing -> []
            Just (Node n) -> [n.id]
        newNode = Node { value : val
                       , classes : "cons"
                       , id : getNextId model.nodes
                       , connections : connections
                       }
        newModel = model { nodes = incId model.nodes }
      in
       Just $ Tuple newNode newModel

mkNodePos :: Int -> Number -> Node -> Tuple Int NodeMap -> Tuple Int NodeMap
mkNodePos total ypos (Node node) (Tuple pos acc) =
  let
    ftotal = toNumber total
    fpos = toNumber pos
    r = min maxRadius (maxWidth / 3.0 / ftotal)
    offset x = x + buffer + r
    calcPos x = maxWidth - (offset $ x * maxWidth)
    circPos = { x : calcPos $ fpos / ftotal
              , y : ypos
              , r : r
              , value : node.value
              , connections : node.connections
              , classes : node.classes
              }
  in
   Tuple (pos + 1) (M.insert node.id circPos acc)

getNodeMap :: Int -> Int -> Q.Queue Node -> NodeMap
getNodeMap totalFront totalBack (Q.Queue queue) =
  let
    front = snd $ foldr (mkNodePos totalFront (maxHeight / 4.0)) (Tuple 0 M.empty) queue.front
    back = snd $ foldr (mkNodePos totalBack (3.0 * maxHeight / 4.0)) (Tuple 0 M.empty) queue.back
  in
   M.union front back

data Action = Empty
            | Rotate
            | Top
            | Bottom
            | Pop
            | Eject
            | Push
            | Inject
            | CurrentInput String
            | ShowStructure
            | Code CodeAction
            | Animate AnimationAction

updateQueue :: Model -> Q.Queue Node -> String -> EffModel Model Action _
updateQueue model queue fn =
  let
    bict = Q.biCount queue
    newMap = getNodeMap (fst bict) (snd bict) queue
    newSource = changeFn model.code fn
  in
   { state: model { nodes = updateNodes model.nodes newMap
                  , queue = queue
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
  noEffects $ model { code = updateCode action model.code }
update Empty model =
  updateQueue model Q.empty "empty"
update Rotate model =
  updateQueue model (Q.rotate model.queue) "rotate"
update Top model =
  let
    mhead = Q.top model.queue
    mtail = Q.pop model.queue
  in
   case Tuple mhead mtail of
     Tuple Nothing _ -> noEffects $ model { code = changeFn model.code "top" }
     Tuple _ Nothing -> noEffects $ model { code = changeFn model.code "top" }
     Tuple (Just h) (Just t) ->
       let
         newHead = changeClass "head" h
         newTail = wipeClasses t
         newQ = Q.push newHead newTail
       in
        updateQueue model newQ "top"
update Pop model =
  let
    mtail = Q.pop model.queue
  in
   case mtail of
     Nothing -> noEffects $ model { code = changeFn model.code "pop" }
     Just t ->
       let
         newTail = changeAllClasses "tail" t
         newQ = newTail
       in
        updateQueue model newQ "pop"
update Bottom model =
  let
    mhead = Q.back model.queue
    mtail = Q.eject model.queue
  in
   case Tuple mhead mtail of
     Tuple Nothing _ -> noEffects $ model { code = changeFn model.code "back" }
     Tuple _ Nothing -> noEffects $ model { code = changeFn model.code "back" }
     Tuple (Just h) (Just t) ->
       let
         newHead = changeClass "head" h
         newTail = wipeClasses t
         newQ = Q.inject newHead newTail
       in
        updateQueue model newQ "back"
update Eject model =
  let
    mtail = Q.eject model.queue
  in
   case mtail of
     Nothing -> noEffects $ model { code = changeFn model.code "eject" }
     Just t ->
       let
         newTail = changeAllClasses "tail" t
         newQ = newTail
       in
        updateQueue model newQ "eject"
update Inject model =
  let
    mnode = mkNode Back model
    cleanQueue = wipeClasses model.queue
  in
   case mnode of
     Nothing -> noEffects $ model { code = changeFn model.code "inject" }
     Just (Tuple node newModel) ->
       updateQueue newModel (Q.inject node cleanQueue) "inject"
update Push model =
  let
    mnode = mkNode Front model
    cleanQueue = wipeClasses model.queue
  in
   case mnode of
     Nothing -> noEffects $ model { code = changeFn model.code "push" }
     Just (Tuple node newModel) ->
       updateQueue newModel (Q.push node cleanQueue) "push"
update (CurrentInput s) model =
  noEffects $ model { nodes = setInput model.nodes (fromString s) }
update ShowStructure model =
  noEffects $ model { code = changeFn model.code "Queue" }

viewCtrl :: Model -> H.Html Action
viewCtrl model =
  let
    dataBtn = H.div [ ] [ H.button [ HA.className "pure-button pure-button-warning"
                                   , HE.onClick $ const ShowStructure
                                   ] [ H.text "Queue Structure" ]
                        ]
    emptyBtn = H.div [ ] [ H.button [ HA.className "pure-button"
                                    , HE.onClick $ const Empty
                                    ] [ H.text "Empty" ]
                         ]
    rotateBtn = H.div [ ] [ H.button [ HA.className "pure-button"
                                     , HE.onClick $ const Rotate
                                     ] [ H.text "Rotate" ]
                          ]
    topBtn = H.button [ HA.className "pure-button"
                      , HE.onClick $ const Top
                      ] [ H.text "Top" ]
    popBtn = H.button [ HA.className "pure-button"
                      , HE.onClick $ const Pop
                      ] [ H.text "Pop / Unshift" ]
    frontDiv = H.div [ ] [ topBtn, popBtn ]
    backBtn = H.button [ HA.className "pure-button"
                       , HE.onClick $ const Bottom
                       ] [ H.text "Back" ]
    ejectBtn = H.button [ HA.className "pure-button"
                        , HE.onClick $ const Eject
                        ] [ H.text "Eject" ]
    backDiv = H.div [ ] [ backBtn, ejectBtn ]
    consSpan = H.div [ ] [ H.span [ ] [ H.button [ HA.className "pure-button"
                                                 , HE.onClick $ const Inject
                                                 ] [ H.text "Inject / Shift"]
                                      , H.button [ HA.className "pure-button"
                                                 , HE.onClick $ const Push
                                                 ] [ H.text "Push" ]
                                      , H.input [ HA.type_ "number"
                                                , HA.maxLength "3"
                                                , HA.size 3
                                                , HA.placeholder "1"
                                                , HE.onChange $ \t -> CurrentInput t.target.value
                                                ] [ ]]
                         ]
  in
    H.div [ ] [ Code <$> sourceBtn
              , dataBtn
              , consSpan
              , emptyBtn
              , rotateBtn
              , frontDiv
              , backDiv
              ]
