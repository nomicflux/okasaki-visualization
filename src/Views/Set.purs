module Views.Set where

import CodeSnippet as CS
import Data.Map as M
import Pux.Html as H
import Pux.Html.Attributes as HA
import Pux.Html.Events as HE
import Structures.Purs.Set as S
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Data.Array (foldM, (:), concatMap, fromFoldable)
import Data.Either (Either(..))
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
import Debug.Trace (spy)
import Math (atan, sin, cos, pi, pow)
import Prelude (($), (+), (/), (-), (*), (<), (<>), (<<<), const, min, max, (<$>), bind, pure, negate)
import Pux (EffModel, noEffects)
import Pux.Html.Attributes (offset)
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

type Model = { set :: S.Set Node
             , currId :: NodeID
             , currInput :: Maybe NodeValue
             , currNodes :: NodeMap
             , prevNodes :: NodeMap
             , startAnimation :: Maybe Time
             , animationPhase :: Number
             , delay :: Number
             , sourceCode :: M.Map String String
             , currFnName :: Maybe String
             , currFn :: Maybe String
             }

initModel :: Model
initModel = { set : S.empty
            , currId : 0
            , currInput : Nothing
            , currNodes : M.empty
            , prevNodes : M.empty
            , startAnimation : Nothing
            , animationPhase : 1.0
            , delay : 750.0 * millisecond
            , sourceCode : M.empty
            , currFnName : Nothing
            , currFn : Nothing
            }

mkNode :: Model -> Maybe (Tuple Node Model)
mkNode model =
  case model.currInput of
    Nothing -> Nothing
    Just val ->
      let
        newNode = Node { value : val
                       , classes : "cons"
                       , id : model.currId
                       , connections : []
                       }
        newModel = model { currId = model.currId + 1 }
      in
       Just $ Tuple newNode newModel

maxWidth :: Number
maxWidth = 800.0

maxHeight :: Number
maxHeight = 600.0

maxRadius :: Number
maxRadius = 40.0

buffer :: Number
buffer = 10.0

type Dimensions = { width :: Int
                  , depth :: Int
                  }
type Position = { x :: Int
                , y :: Int
                }

mkNodePos :: Dimensions -> Position -> Node -> NodePos
mkNodePos dim pos (Node node) =
  let
    fwidth = toNumber (dim.width + 1)
    fheight = toNumber (dim.depth + 1)
    ftotal = max fwidth fheight
    rY = maxWidth / 2.0 / (pow 2.0 (toNumber pos.y))
    rH = maxWidth / 2.0 / (max fwidth fheight)
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

getID :: Node -> NodeID
getID (Node n) = n.id

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

changeClass :: Classes -> Node -> Node
changeClass classes (Node node) = Node (node { classes = classes })

changeAllClasses :: forall f. Functor f => Classes -> f Node -> f Node
changeAllClasses classes = map (changeClass classes)

wipeClasses :: forall f. Functor f => f Node -> f Node
wipeClasses = changeAllClasses ""

data Action = Empty
            | Member
            | Insert
            | CurrentInput String
            | ShowStructure
            | StartTimer Time
            | LoadCode (Either String CS.SourceCode)
            | Failure Error
            | Tick Time

changeFn :: Model -> String -> Model
changeFn model fn = model { currFn = M.lookup fn model.sourceCode
                          , currFnName = Just fn}

updateSet :: Model -> S.Set Node -> String -> EffModel Model Action _
updateSet model set fn =
  let
    depth = S.depth set
    width = S.maxWidth set
    -- leftWidth = S.leftMaxWidth set
    -- rightWidth = S.rightMaxWidth set
    newMap = getNodeMap {width : width, depth : depth} set
  in
   { state: model { prevNodes = model.currNodes
                  , currNodes = newMap
                  , set = set
                  , animationPhase = 0.0
                  , currFnName = Just fn
                  , currFn = M.lookup fn model.sourceCode
                  }
   , effects: [ do
      time <- liftEff now
      pure $ StartTimer time
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

update :: Action -> Model -> EffModel Model Action _
update (Failure err) model = noEffects model
update (LoadCode (Left err)) model = noEffects model
update (LoadCode (Right code)) model =
  let
    sourceCodeModel = model { sourceCode = CS.parseFunctions code }
    newModel =
      case model.currFnName of
        Nothing -> sourceCodeModel
        Just fn -> changeFn sourceCodeModel fn
  in
   noEffects newModel
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
  updateSet model S.empty "empty"
update Member model =
  case model.currInput of
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
     Nothing -> noEffects $ changeFn model "insert"
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
  noEffects $ model { currInput = fromString s }
update ShowStructure model =
  noEffects $ changeFn model "Set"

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
                               signY = if node.y < edgeNode.y then signX else -1.0 * signX
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
                                                  , HE.onChange $ \t -> CurrentInput t.target.value
                                                  ] [ ]]
                         ]
    controlDiv = H.div [ HA.className "pure-u-1-2" ] [ dataBtn
                                                     , emptyBtn
                                                     , insertSpan
                                                     ]
    codeDiv = H.div [ HA.className "pure-u-1-2" ]
                    [ H.code [ ]
                      [ H.pre [ ]
                        [ case model.currFn of
                             Nothing ->
                               H.i []
                                   [ H.text "No implementation given or no function selected"]
                             Just fn -> H.text fn ]
                      ]
                    ]
  in
   H.div [ HA.className "pure-g" ] [ stackDiv
                                   , controlDiv
                                   , codeDiv
                                   ]
