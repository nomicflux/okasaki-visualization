module Views.Queue where

import Data.Map as M
import Pux.Html as H
import Pux.Html.Attributes as HA
import Pux.Html.Events as HE
import Structures.Queue as Q
import Control.Monad.Eff.Class (liftEff)
-- import Control.Monad.Eff.Timer (TIMER)
import Data.Array ((:), concatMap, fromFoldable)
import Data.Eq (class Eq, eq)
import Data.Filterable (filterMap)
import Data.Foldable (class Foldable, foldr)
import Data.Function.Uncurried (runFn3)
import Data.Functor (map, class Functor)
import Data.Int (fromString, toNumber, round)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (class Ord, compare)
import Data.Show (show)
import Data.Tuple (Tuple(..), fst, snd)
import Math (atan, sin, cos, pi)
import Prelude (($), (+), (/), (-), (*), (<), (<>), (<<<), const
               , min, max, (<$>), bind, pure, negate)
import Pux (EffModel, noEffects)
import Signal ((~>))
import Signal.Time (now, Time, millisecond)

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

type Model = { queue :: Q.Queue Node
             , currId :: NodeID
             , currInput :: Maybe NodeValue
             , currNodes :: NodeMap
             , prevNodes :: NodeMap
             , startAnimation :: Maybe Time
             , animationPhase :: Number
             , delay :: Number
             }

initModel :: Model
initModel = { queue : Q.empty
            , currId : 0
            , currInput : Nothing
            , currNodes : M.empty
            , prevNodes : M.empty
            , startAnimation : Nothing
            , animationPhase : 1.0
            , delay : 750.0 * millisecond
            }

data Direction = Front | Back

mkNode :: Direction -> Model -> Maybe (Tuple Node Model)
mkNode dir model =
  case model.currInput of
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

changeAllClasses :: forall f. Functor f => Classes -> f Node -> f Node
changeAllClasses classes = map (changeClass classes)

wipeClasses :: forall f. Functor f => f Node -> f Node
wipeClasses = changeAllClasses ""

data Action = Empty
            | Top
            | Bottom
            | Pop
            | Eject
            | Push
            | Inject
            | CurrentInput String
            | StartTimer Time
            | Tick Time

updateQueue :: Model -> Q.Queue Node -> EffModel Model Action _
updateQueue model queue =
  let
    bict = Q.biCount queue
    -- ct = max (fst bict) (snd bict)
    ct = (fst bict) + (snd bict)
    newMap = getNodeMap ct queue
  in
   { state: model { prevNodes = model.currNodes
                  , currNodes = newMap
                  , queue = queue
                  , animationPhase = 0.0
                  }
   , effects: [ do
      time <- liftEff now
      pure $ StartTimer time
              ]
   }

update :: Action -> Model -> EffModel Model Action _
update (Tick time) model =
  case model.startAnimation of
    Nothing -> noEffects model
    Just start ->
      let
        timeDiff = time - start
      in
       if model.delay < timeDiff
       then noEffects $ model { animationPhase = 1.0 }
       else noEffects $ model { animationPhase = sin (timeDiff / model.delay * pi / 2.0) }
update (StartTimer time) model = noEffects $ model { startAnimation = Just time
                                                   , animationPhase = 0.0
                                                   }
update Empty model =
  updateQueue model Q.empty
update Top model =
  let
    mhead = Q.top model.queue
    mtail = Q.pop model.queue
  in
   case Tuple mhead mtail of
     Tuple Nothing _ -> noEffects $ model
     Tuple _ Nothing -> noEffects $ model
     Tuple (Just h) (Just t) ->
       let
         newHead = changeClass "head" h
         newTail = wipeClasses t
         newQ = Q.push newHead newTail
       in
        updateQueue model newQ
update Pop model =
  let
    mtail = Q.pop model.queue
  in
   case mtail of
     Nothing -> noEffects $ model
     Just t ->
       let
         newTail = changeAllClasses "tail" t
         newQ = newTail
       in
        updateQueue model newQ
update Bottom model =
  let
    mhead = Q.back model.queue
    mtail = Q.eject model.queue
  in
   case Tuple mhead mtail of
     Tuple Nothing _ -> noEffects $ model
     Tuple _ Nothing -> noEffects $ model
     Tuple (Just h) (Just t) ->
       let
         newHead = changeClass "head" h
         newTail = wipeClasses t
         newQ = Q.inject newHead newTail
       in
        updateQueue model newQ
update Eject model =
  let
    mtail = Q.eject model.queue
  in
   case mtail of
     Nothing -> noEffects $ model
     Just t ->
       let
         newTail = changeAllClasses "tail" t
         newQ = newTail
       in
        updateQueue model newQ
update Inject model =
  let
    mnode = mkNode Back model
    cleanQueue = wipeClasses model.queue
  in
   case mnode of
     Nothing -> noEffects $ model
     Just (Tuple node newModel) ->
       updateQueue newModel (Q.inject node cleanQueue)
update Push model =
  let
    mnode = mkNode Front model
    cleanQueue = wipeClasses model.queue
  in
   case mnode of
     Nothing -> noEffects $ model
     Just (Tuple node newModel) ->
       updateQueue newModel (Q.push node cleanQueue)
update (CurrentInput s) model =
  noEffects $ model { currInput = fromString s }

svgText :: forall a. Array (H.Attribute a) -> Array (H.Html a) -> H.Html a
svgText = runFn3 H.element "text"

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

viewNodePos :: Number -> NodeMap -> NodeMap -> NodeID -> Array (H.Html Action)
viewNodePos phase prevmap currmap nid =
  let
    interphase = interpolateNodes phase
    prev = fromMaybe (initialNode nid) (M.lookup nid prevmap)
    curr = fromMaybe (finalNode nid) (M.lookup nid currmap)
    node = interphase prev curr
    circ = H.circle [ HA.cx (show $ node.x)
                    , HA.cy (show $ node.y)
                    , HA.r (show $ node.r)
                    , HA.className (node.classes <> " node")
                    ] [ ]
    val = svgText [ HA.x (show $ node.x)
                  , HA.y (show $ node.y + 6.0)
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
                               signY = if node.y < edgeNode.y then 1.0 else -1.0
                               invtan = atan ((node.y - edgeNode.y) / (node.x - edgeNode.x))
                             in
                              Just $ H.line [ HA.x1 (show $ node.x + signX*cos invtan * node.r)
                                            , HA.y1 (show $ node.y - signY*sin invtan * node.r)
                                            , HA.x2 (show $ edgeNode.x - signX*cos invtan * edgeNode.r)
                                            , HA.y2 (show $ edgeNode.y + signY*sin invtan * edgeNode.r)
                                            , HA.className "edge"] [ ]) node.connections
  in
   circ : val : edges

view :: Model -> H.Html Action
view model =
  let
    keys = M.keys $ M.union model.prevNodes model.currNodes
    showNodes = viewNodePos model.animationPhase model.prevNodes model.currNodes
    nodes = concatMap showNodes (fromFoldable keys)
    stackDiv = H.div [ HA.className "render" ] [ H.svg [HA.height (show maxHeight)
                                                       , HA.width (show maxWidth)  ] nodes ]
    emptyBtn = H.div [ ] [ H.button [ HA.className "pure-button"
                                    , HE.onClick $ const Empty
                                    ] [ H.text "Empty" ]
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
                                                , HE.onChange $ \t -> CurrentInput t.target.value
                                                ] [ ]]
                         ]
  in
   H.div [ ] [ stackDiv, emptyBtn, frontDiv, backDiv, consSpan ]
