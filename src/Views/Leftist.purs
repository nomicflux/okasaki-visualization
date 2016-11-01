module Views.Leftist where

import CodeSnippet as CS
import Data.Map as M
import Pux.Html as H
import Pux.Html.Attributes as HA
import Pux.Html.Events as HE
import Structures.Purs.Leftist as L
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Data.Array (foldM, (:), concatMap, fromFoldable)
import Data.Either (Either(..))
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Tuple (Tuple(..), fst, snd)
import Debug.Trace (spy)
import Math (sin, pi, pow)
import Prelude (($), (+), (/), (-), (*), (<), (<>), (<<<), const, min, max, (<$>), bind, pure, negate)
import Pux (EffModel, noEffects)
import Signal ((~>))
import Signal.Time (now, Time, millisecond)

import Views.Node

type Model = { heap :: L.Leftist Node
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
initModel = { heap : L.empty
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

type Position = { x :: Int
                , y :: Int
                }

mkNodePos :: Int -> Position -> Node -> NodePos
mkNodePos depth pos (Node node) =
  let
    -- fwidth = toNumber (dim.width + 1)
    fheight = toNumber (depth + 1)
    -- ftotal = max fwidth fheight
    rY = maxWidth / 2.0 / (pow 2.0 (toNumber pos.y))
    rH = maxWidth / 2.0 / fheight
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

getNodeMap :: Int -> L.Leftist Node -> NodeMap
getNodeMap depth heap = go heap {x: 0, y: 0}
  where
    go :: L.Leftist Node -> Position -> NodeMap
    go L.Leaf _ = M.empty
    go (L.Node s) pos =
      let
        left = go s.left (pos {x = 2*pos.x, y = pos.y + 1})
        right = go s.right (pos { x = 2*pos.x + 1, y = pos.y + 1})
        combined = M.union left right
        curr = mkNodePos depth pos s.value
      in
       M.insert (getID s.value) curr combined

data Action = Empty
            | Insert
            | FindMin
            | DeleteMin
            | CurrentInput String
            | ShowStructure
            | StartTimer Time
            | LoadCode (Either String CS.SourceCode)
            | Failure Error
            | Tick Time

changeFn :: Model -> String -> Model
changeFn model fn = model { currFn = M.lookup fn model.sourceCode
                          , currFnName = Just fn}

updateHeap :: Model -> L.Leftist Node -> String -> EffModel Model Action _
updateHeap model heap fn =
  let
    depth = L.rank heap
    newMap = getNodeMap depth heap
  in
   { state: model { prevNodes = model.currNodes
                  , currNodes = newMap
                  , heap = heap
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
  updateHeap model L.empty "empty"
-- update Member model =
--   case model.currInput of
--     Nothing ->
--       updateHeap model (wipeClasses model.set) "member"
--     Just val ->
--       let
--         node = S.get model.set (blankNode val)
--       in
--        case node of
--          Nothing ->
--            updateSet model (wipeClasses model.set) "member"
--          Just oldNode ->
--            updateSet model (S.update (wipeClasses model.set) $ changeClasses oldNode "head") "member"
update Insert model =
  let
    cleanHeap = wipeClasses model.heap
    mnode = mkNode model
  in
   case mnode of
     Nothing -> noEffects $ changeFn model "insert"
     Just (Tuple node newModel) ->
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
