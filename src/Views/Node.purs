module Views.Node where

import Pux.Html as H
import Pux.Html.Attributes as HA
import Data.Array ((:))
import Data.Eq (class Eq, eq)
import Data.Filterable (filterMap)
import Data.Function.Uncurried (runFn3)
import Data.Functor (class Functor, map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (class Ord, compare)
import Data.Show (show)
import Math (atan, sin, cos)
import Prelude (($), (+), (/), (-), (*), (<), (<>), (<<<), (<$>), negate, min)

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

getID :: Node -> NodeID
getID (Node n) = n.id

maxWidth :: Number
maxWidth = 800.0

maxHeight :: Number
maxHeight = 600.0

maxRadius :: Number
maxRadius = 40.0

buffer :: Number
buffer = 10.0

changeClass :: Classes -> Node -> Node
changeClass classes (Node node) = Node (node { classes = classes })

changeAllClasses :: forall f. Functor f => Classes -> f Node -> f Node
changeAllClasses classes = map (changeClass classes)

wipeClasses :: forall f. Functor f => f Node -> f Node
wipeClasses = changeAllClasses ""

initialNode :: NodeID -> NodePos
initialNode nid = { x : maxWidth + 10.0
                  , y : -10.0
                  , r : 0.0
                  , connections : [nid]
                  , value : 0
                  , classes : ""
                  }

finalNode :: NodeID -> NodePos
finalNode _ = { x : -10.0
              , y : maxHeight + 10.0
              , r : 0.0
              , connections : []
              , value : 0
              , classes : ""
              }

svgText :: forall a. Array (H.Attribute a) -> Array (H.Html a) -> H.Html a
svgText = runFn3 H.element "text"

interpolate :: Number -> Number -> Number -> Number
interpolate phase old new =
  let
    diff = new - old
  in
   old + phase*diff

interpolateNodes :: Number -> NodePos -> NodePos -> NodePos
interpolateNodes phase old new =
  let
    intphase = interpolate phase
  in
   new { x = intphase old.x new.x
       , y = intphase old.y new.y
       , r = intphase old.r new.r
       }

viewNodePos :: forall action. Number -> NodeMap -> NodeMap -> NodeID -> Array (H.Html action)
viewNodePos phase prevmap currmap nid =
  let
    interphase = interpolateNodes phase
    prev = fromMaybe (initialNode nid) (M.lookup nid prevmap)
    curr = fromMaybe (finalNode nid) (M.lookup nid currmap)
    node = interphase prev curr
    fontSize = min (node.r * 2.0 - 1.0) 16.0
    circ = H.circle [ HA.cx (show $ node.x)
                    , HA.cy (show $ node.y)
                    , HA.r (show $ node.r)
                    , HA.className (node.classes <> " node")
                    ] [ ]
    val = svgText [ HA.x (show $ node.x)
                  , HA.y (show $ node.y + (fontSize / 2.0) - 1.0)
                  , HA.fontSize (show fontSize)
                  , HA.textAnchor "middle" ] [ H.text (show node.value)]
    edges = filterMap (\n ->
                        let
                          mnode = M.lookup n currmap
                        in
                         case mnode of
                           Nothing -> Nothing
                           Just currEdge ->
                             let
                               prevEdge = fromMaybe (initialNode n) (M.lookup n prevmap)
                               edgeNode = interphase prevEdge currEdge
                               signX = if node.x < edgeNode.x then 1.0 else -1.0
                               -- signY = if node.y < edgeNode.y then -1.0 * signX else -1.0 * signX
                               invtan = atan ((node.y - edgeNode.y) / (node.x - edgeNode.x))
                             in
                              Just $ H.line [ HA.x1 (show $ node.x + signX*cos invtan * node.r)
                                            , HA.y1 (show $ node.y + signX*sin invtan * node.r)
                                            , HA.x2 (show $ edgeNode.x - signX*cos invtan * edgeNode.r)
                                            , HA.y2 (show $ edgeNode.y - signX*sin invtan * edgeNode.r)
                                            , HA.className "edge"] [ ]) node.connections
  in
   circ : val : edges
