module Views.Set where

import Data.Map as M
import Pux.Html as H
import Pux.Html.Attributes as HA
import Pux.Html.Events as HE
import Structures.Purs.Set as S
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array ((:))
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Math (pow)
import Prelude (($), (+), (/), (-), (*), (<), (<>), (<<<), const, min, (<$>), bind, pure, negate)
import Signal.Channel (CHANNEL)
import Pux (EffModel, noEffects)
import Signal.Time (now)

import Views.Animation (Animation, AnimationAction(..), defaultAnimation, resetAnimation, updateAnimation)
import Views.Node (NodeMap, NodeValue, NodeID, Node(..), maxWidth, maxHeight, wipeClasses, buffer, maxRadius, getID, Classes, NodePos, NodeData, blankNodes, getNextId, updateNodes, incId, getInput, setInput)
import Views.SourceCode (CodeAction, SourceCodeInfo, sourceBtn, changeFn, updateCode, blankSourceCode)

type Model = { set :: S.Set Node
             , nodes :: NodeData
             , animation :: Animation
             , code :: SourceCodeInfo
             }

initModel :: Model
initModel = { set : S.empty
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

type Dimensions = { width :: Int
                  , depth :: Int
                  }
type Position = { x :: Int
                , y :: Int
                }

mkNodePos :: Dimensions -> Position -> Node -> NodePos
mkNodePos dim pos (Node node) =
  let
    fheight = toNumber (dim.depth)
    rY = maxWidth / 2.0 / (pow 2.0 (toNumber pos.y))
    rH = maxWidth / 4.0 / fheight
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
              , connections : node.connections
              , classes : node.classes
              }
  in
   circPos

getNodeMap :: Dimensions -> S.Set Node -> NodeMap
getNodeMap dim set = go set {x: 0, y: 0}
  where
    go :: S.Set Node -> Position -> NodeMap
    go S.Leaf _ = M.empty
    go (S.Node s) pos =
      let
        left = go s.left (pos {x = 2*pos.x, y = pos.y + 1})
        right = go s.right (pos { x = 2*pos.x + 1, y = pos.y + 1})
        combined = M.union left right
        curr = mkNodePos dim pos s.value
      in
       M.insert (getID s.value) curr combined

data Action = Empty
            | Member
            | Insert
            | CurrentInput String
            | ShowStructure
            | Code CodeAction
            | Animate AnimationAction

updateSet :: Model -> S.Set Node -> String -> EffModel Model Action _
updateSet model set fn =
  let
    depth = S.depth set
    width = S.maxWidth set
    newMap = getNodeMap {width : width, depth : depth} set
    newSource = changeFn model.code fn
  in
   { state: model { nodes = updateNodes model.nodes newMap
                  , set = set
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
  updateSet model S.empty "empty"
update Member model =
  case getInput model.nodes of
    Nothing ->
      updateSet model (wipeClasses model.set) "member"
    Just val ->
      let
        node = S.get model.set (blankNode val)
      in
       case node of
         Nothing ->
           updateSet model (wipeClasses model.set) "member"
         Just oldNode ->
           updateSet model (S.update (wipeClasses model.set) $ changeClasses oldNode "head") "member"
update Insert model =
  let
    cleanSet = wipeClasses model.set
    mnode = mkNode model
  in
   case mnode of
     Nothing -> noEffects $ model { code = changeFn model.code "insert" }
     Just (Tuple node newModel) ->
       if S.member model.set node
       then noEffects model
       else
         let
           midSet = S.insertWithParent cleanSet node
           finalSet = case snd midSet of
             Nothing -> fst midSet
             Just parent ->
               S.update (fst midSet) (changeConn node [getID parent])
         in
          updateSet newModel finalSet "insert"
update (CurrentInput s) model =
  noEffects $ model { nodes = setInput model.nodes (fromString s) }
update ShowStructure model =
  noEffects $ model { code = changeFn model.code "Set" }

viewCtrl :: Model -> H.Html Action
viewCtrl model =
  let
    dataBtn = H.div [ ] [ H.button [ HA.className "pure-button pure-button-warning"
                                   , HE.onClick $ const ShowStructure
                                   ] [ H.text "Set Structure" ]
                        ]
    emptyBtn = H.div [ ] [ H.button [ HA.className "pure-button"
                                    , HE.onClick $ const Empty
                                    ] [ H.text "Empty" ]
                         ]
    insertSpan = H.div [ ] [ H.span [ ] [ H.button [ HA.className "pure-button"
                                                   , HE.onClick $ const Insert
                                                   ] [ H.text "Insert"]
                                        , H.button [ HA.className "pure-button"
                                                   , HE.onClick $ const Member
                                                   ] [ H.text "Member"]
                                        , H.input [ HA.type_ "number"
                                                  , HA.maxLength "3"
                                                  , HA.placeholder "1"
                                                  , HA.size 3
                                                  , HE.onChange $ \t -> CurrentInput t.target.value
                                                  ] [ ]]
                         ]
  in
    H.div [ ] [ Code <$> sourceBtn
              , dataBtn
              , insertSpan
              , emptyBtn
              ]
