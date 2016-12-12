module Views.Leftist where

import Data.Map as M
import Pux.Html as H
import Pux.Html.Attributes as HA
import Pux.Html.Events as HE
import Structures.Purs.Leftist as L
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array ((:))
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Math (pow)
import Prelude (($), (+), (/), (-), (*), (<), (<>), (<<<), const, min, (<$>), bind, pure, negate)
import Pux (EffModel, noEffects)
import Signal.Channel (CHANNEL)
import Signal.Time (now)

import Views.Animation (Animation, AnimationAction(..), defaultAnimation, resetAnimation, updateAnimation)
import Views.Node (NodeMap, NodeValue, NodeID, Node(..), maxWidth, maxHeight, wipeClasses, changeClass, buffer, maxRadius, getID, NodePos, Classes, NodeData, blankNodes, getNextId, updateNodes, incId, getInput, setInput)
import Views.SourceCode (CodeAction, SourceCodeInfo, sourceBtn, changeFn, updateCode, blankSourceCode)

type Model = { heap :: L.Leftist Node
             , nodes :: NodeData
             , animation :: Animation
             , code :: SourceCodeInfo
             }

initModel :: Model
initModel = { heap : L.empty
            , nodes : blankNodes
            , animation : defaultAnimation
            , code : blankSourceCode
            }

mkNode :: Model -> Maybe (Tuple Node Model)
mkNode model =
  case getInput model.nodes of
    Nothing -> Nothing
    Just val ->
      let
        newNode = Node { value : val
                       , classes : "cons"
                       , id : getNextId model.nodes
                       , connections : []
                       }
        newModel = model { nodes = incId model.nodes }
      in
       Just $ Tuple newNode newModel

type Position = { x :: Int
                , y :: Int
                }

mkNodePos :: Int -> Position -> Node -> Array NodeID -> NodePos
mkNodePos depth pos (Node node) conns =
  let
    fheight = toNumber depth
    rY = maxWidth / 1.0 / (pow 2.0 (toNumber pos.y))
    rH = maxWidth / 1.0 / (pow 2.0 fheight)
    r = min maxRadius (min rY rH)
    offset x = x + buffer + r
    calcPosY y = offset $ (y * maxHeight / fheight)
    calcPosX x y =
      let
        bins = pow 2.0 y
        binSize = maxWidth / bins
        binOffset = binSize / 2.0
      in
       x * binSize + binOffset
    circPos = { x : calcPosX (toNumber pos.x) (toNumber pos.y)
              , y : calcPosY (toNumber pos.y)
              , r : r
              , value : node.value
              , connections : conns
              , classes : node.classes
              }
  in
   circPos

getNodeMap :: Int -> L.Leftist Node -> NodeMap
getNodeMap depth heap = go heap {x: 0, y: 0} []
  where
    go :: L.Leftist Node -> Position -> Array NodeID -> NodeMap
    go L.Leaf _ _ = M.empty
    go (L.Node s) pos parents =
      let
        currId = getID s.value
        left = go s.left (pos {x = 2*pos.x, y = pos.y + 1}) [currId]
        right = go s.right (pos { x = 2*pos.x + 1, y = pos.y + 1}) [currId]
        combined = M.union left right
        curr = mkNodePos depth pos s.value parents
      in
       M.insert (getID s.value) curr combined

data Action = Empty
            | Insert
            | FindMin
            | DeleteMin
            | CurrentInput String
            | ShowStructure
            | Code CodeAction
            | Animate AnimationAction

updateHeap :: Model -> L.Leftist Node -> String -> EffModel Model Action _
updateHeap model heap fn =
  let
    depth = L.depth heap
    newMap = getNodeMap depth heap
    newSource = changeFn model.code fn
  in
   { state: model { nodes = updateNodes model.nodes newMap
                  , heap = heap
                  , animation = resetAnimation model.animation
                  , code = newSource
                  }
   , effects: [ do
      time <- liftEff now
      pure $ Animate (StartTimer time)
              ]
   }

changeConn :: Node -> Array NodeID -> Node
changeConn (Node node) conns = Node (node {connections = conns})

blankNode :: NodeValue -> Node
blankNode val = Node { value : val
                     , classes : ""
                     , id : -1
                     , connections : []
                     }

changeClasses :: Node -> Classes -> Node
changeClasses (Node node) classes = Node $ node { classes = classes }

update :: forall eff. Action -> Model -> EffModel Model Action (channel :: CHANNEL, err :: EXCEPTION | eff)
update (Animate action) model =
  noEffects $ model { animation = updateAnimation action model.animation }
update (Code action) model =
  noEffects $ model { code = updateCode action model.code }
update Empty model =
  updateHeap model L.empty "empty"
update Insert model =
  let
    cleanHeap = wipeClasses model.heap
    mnode = mkNode model
  in
   case mnode of
     Nothing -> noEffects $ model { code = changeFn model.code "insert" }
     Just (Tuple node newModel) ->
       let
         newHeap = L.insert cleanHeap node
       in
        updateHeap newModel newHeap "insert"
update FindMin model =
  let
    mheapMin = L.findMin model.heap
  in
   case mheapMin of
     Nothing -> noEffects $ model { code = changeFn model.code "findMin" }
     Just heapMin ->
       let
         newNode = changeClass "head" heapMin
         newHeap = L.update (wipeClasses model.heap) newNode
       in
        updateHeap model newHeap "findMin"
update DeleteMin model =
  let
    cleanHeap = wipeClasses model.heap
  in
   case L.deleteMin cleanHeap of
     Nothing -> noEffects $ model { code = changeFn model.code "deleteMin" }
     Just heap -> updateHeap model heap "deleteMin"
update (CurrentInput s) model =
  noEffects $ model { nodes = setInput model.nodes (fromString s) }
update ShowStructure model =
  noEffects $ model { code = changeFn model.code "LeftistHeap" }

viewCtrl :: Model -> H.Html Action
viewCtrl model =
  let
    dataBtn = H.div [ ] [ H.button [ HA.className "pure-button pure-button-warning"
                                   , HE.onClick $ const ShowStructure
                                   ] [ H.text "Leftist Heap Structure" ]
                        ]
    emptyBtn = H.div [ ] [ H.button [ HA.className "pure-button"
                                    , HE.onClick $ const Empty
                                    ] [ H.text "Empty" ]
                         ]
    findMinBtn = H.button [ HA.className "pure-button"
                                      , HE.onClick $ const FindMin
                                      ] [ H.text "Find Min"]
    delMinBtn = H.button [ HA.className "pure-button"
                                      , HE.onClick $ const DeleteMin
                                      ] [ H.text "Delete Min"]
    minSpan = H.div [ ] [ findMinBtn
                        , delMinBtn
                        ]
    insertSpan = H.div [ ] [ H.span [ ] [ H.button [ HA.className "pure-button"
                                                   , HE.onClick $ const Insert
                                                   ] [ H.text "Insert"]
                                        , H.input [ HA.type_ "number"
                                                  , HA.maxLength "3"
                                                  , HA.size 3
                                                  , HE.onChange $ \t -> CurrentInput t.target.value
                                                  ] [ ]]
                         ]
  in
    H.div [ ] [ Code <$> sourceBtn
              , dataBtn
              , insertSpan
              , emptyBtn
              , minSpan
              ]
