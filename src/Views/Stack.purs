module Views.Stack where

import Data.Map as M
import Pux.Html as H
import Pux.Html.Attributes as HA
import Pux.Html.Events as HE
import Structures.Stack as S
import Data.Array ((:), concatMap, fromFoldable)
import Data.Eq (class Eq, eq)
import Data.Filterable (filterMap)
import Data.Foldable (class Foldable, foldr)
import Data.Functor (map)
import Data.Int (fromString, toNumber, round)
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord, compare)
import Data.Show (show)
import Data.Tuple (Tuple(..), snd)
import Prelude (($), (+), (/), (-), (*), (<>), (<<<), const, min)
-- import Pux (EffModel, noEffects)

type Classes = String
type NodeID = Int
type NodeValue = Int
data Node = Node { value :: NodeValue
                 , classes :: Classes
                 , id :: NodeID
                 , connections :: Array NodeID
                 }

type NodePos = { x :: Number
               , y :: Number
               , r :: Number
               , connections :: Array NodeID
               , value :: NodeValue
               , classes :: Classes
               }
type NodeMap = M.Map NodeID NodePos

instance eqNode :: Eq Node where
  eq (Node a) (Node b) = eq a.value b.value

instance ordNode :: Ord Node where
  compare (Node a) (Node b) = compare a.value b.value

type Model = { stack :: S.Stack Node
             , currId :: NodeID
             , currInput :: Maybe NodeValue
             , currNodes :: NodeMap
             , prevNodes :: NodeMap
             }

initModel :: Model
initModel = { stack : S.empty
            , currId : 0
            , currInput : Nothing
            , currNodes : M.empty
            , prevNodes : M.empty
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

maxWidth :: Number
maxWidth = 800.0

maxHeight :: Number
maxHeight = 200.0

maxRadius :: Number
maxRadius = 40.0

buffer :: Number
buffer = 10.0

mkNodePos :: Int -> Node -> Tuple Int NodeMap -> Tuple Int NodeMap
mkNodePos total (Node node) (Tuple pos acc) =
  let
    ftotal = toNumber total
    fpos = toNumber pos
    r = min maxRadius (maxWidth / 3.0 / ftotal)
    offset x = x + buffer + r
    calcPos x = offset $ x * maxWidth
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

changeClass :: Classes -> Node -> Node
changeClass classes (Node node) = Node (node { classes = classes })

changeAllClasses :: Classes -> S.Stack Node -> S.Stack Node
changeAllClasses classes = map (changeClass classes)

wipeClasses :: S.Stack Node -> S.Stack Node
wipeClasses = changeAllClasses ""

data Action = Empty
            | Head
            | Tail
            | Insert
            | CurrentInput String

updateStack :: Model -> S.Stack Node -> Model
updateStack model stack =
  let
    ct = S.count stack
    newMap = getNodeMap ct stack
  in
   model { prevNodes = model.currNodes
         , currNodes = newMap
         , stack = stack
         }

update :: Action -> Model -> Model
update Empty model =
  updateStack model S.empty
update Head model =
  let
    mhead = S.head model.stack
    mtail = S.tail model.stack
  in
   case Tuple mhead mtail of
     Tuple Nothing _ -> model
     Tuple _ Nothing -> model
     Tuple (Just h) (Just t) ->
       let
         newHead = changeClass "head" h
         newTail = wipeClasses t
         newStack = S.cons newHead newTail
       in
        updateStack model newStack
update Tail model =
  let
    mhead = S.head model.stack
    mtail = S.tail model.stack
  in
   case Tuple mhead mtail of
     Tuple Nothing _ -> model
     Tuple _ Nothing -> model
     Tuple (Just h) (Just t) ->
       let
         newHead = changeClass "" h
         newTail = changeAllClasses "tail" t
         newStack = S.cons newHead newTail
       in
        updateStack model newStack
update Insert model =
  let
    mnode = mkNode model
    cleanStack = wipeClasses model.stack
  in
   case mnode of
     Nothing -> model
     Just (Tuple node newModel) ->
       updateStack newModel (S.cons node cleanStack)
update (CurrentInput s) model =
  model { currInput = fromString s }

viewNodePos :: NodeMap -> NodePos -> Array (H.Html Action)
viewNodePos nodemap nodepos =
  let
    xstr = show $ round nodepos.x
    ystr = show $ round nodepos.y
    rstr = show $ round nodepos.r
    circ = H.circle [ HA.cx xstr
                    , HA.cy ystr
                    , HA.r rstr
                    , HA.className (nodepos.classes <> " node")
                    ] [ ]
    val = H.textPath [ HA.x xstr
                     , HA.y ystr
                     , HA.textAnchor "middle" ] [ H.text (show nodepos.value)]
    edges = filterMap (\nid ->
                        let
                          mnode = M.lookup nid nodemap
                        in
                         case mnode of
                           Nothing -> Nothing
                           Just n ->
                             Just $ H.line [ HA.x1 (show <<< round $ nodepos.x - nodepos.r)
                                           , HA.y1 (show <<< round $ nodepos.y)
                                           , HA.x2 (show <<< round $ n.x + n.r)
                                           , HA.y2 (show <<< round $ n.y)
                                           , HA.className "edge"] [ ]) nodepos.connections
  in
   circ : val : edges

view :: Model -> H.Html Action
view model =
  let
    nodes = concatMap (viewNodePos model.currNodes) (fromFoldable $ M.values model.currNodes)
    stackDiv = H.div [ HA.className "render" ] [ H.svg [HA.height (show maxHeight)
                                                       , HA.width (show maxWidth)  ] nodes ]
    emptyBtn = H.div [ ] [ H.button [ HA.className "pure-button"
                                    , HE.onClick $ const Empty
                                    ] [ H.text "Empty" ]
                         ]
    headBtn = H.div [ ] [ H.button [ HA.className "pure-button"
                                   , HE.onClick $ const Head
                                   ] [ H.text "Head" ]
                        ]
    tailBtn = H.div [ ] [ H.button [ HA.className "pure-button"
                                   , HE.onClick $ const Tail
                                   ] [ H.text "Tail" ]
                        ]
    consSpan = H.div [ ] [ H.span [ ] [ H.button [ HA.className "pure-button"
                                                 , HE.onClick $ const Insert
                                                 ] [ H.text "Cons"]
                                      , H.input [ HA.type_ "number"
                                                , HE.onChange $ \t -> CurrentInput t.target.value
                                                ] [ ]]
                         ]
  in
   H.div [ ] [ stackDiv, emptyBtn, headBtn, tailBtn, consSpan ]
