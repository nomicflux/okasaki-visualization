module Views.Queue where

import Data.Map as M
import Pux.Html as H
import Pux.Html.Attributes as HA
import Pux.Html.Events as HE
import Structures.Purs.Queue as Q
import CodeSnippet as CS
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Debug.Trace (spy)
import Data.Array ((:), concatMap, fromFoldable)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Tuple (Tuple(..), fst, snd)
import Math (sin, pi)
import Prelude (($), (+), (/), (-), (*), (<), (<>), (<<<), const, min, max, (<$>), bind, pure)
import Pux (EffModel, noEffects)
import Signal ((~>))
import Signal.Time (now, Time, millisecond)

import Views.Node

type Model = { queue :: Q.Queue Node
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
initModel = { queue : Q.empty
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

mkNodePos :: Int -> Number -> Node -> Tuple Int NodeMap -> Tuple Int NodeMap
mkNodePos total ypos (Node node) (Tuple pos acc) =
  let
    ftotal = toNumber total
    fpos = toNumber pos
    r = min maxRadius (maxWidth / 3.0 / ftotal)
    offset x = x + buffer + r
    calcPos x = offset $ x * maxWidth
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
            | StartTimer Time
            | LoadCode (Either String CS.SourceCode)
            | Failure Error
            | Tick Time

changeFn :: Model -> String -> Model
changeFn model fn = model { currFn = M.lookup fn model.sourceCode
                          , currFnName = Just fn}

updateQueue :: Model -> Q.Queue Node -> String -> EffModel Model Action _
updateQueue model queue fn =
  let
    bict = Q.biCount queue
    -- ct = max (fst bict) (snd bict)
    -- ct = (fst bict) + (snd bict)
    newMap = getNodeMap (fst bict) (snd bict) queue
  in
   { state: model { prevNodes = model.currNodes
                  , currNodes = newMap
                  , queue = queue
                  , animationPhase = 0.0
                  , currFnName = Just fn
                  , currFn = M.lookup fn model.sourceCode
                  }
   , effects: [ do
      time <- liftEff now
      pure $ StartTimer time
              ]
   }

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
  updateQueue model Q.empty "empty"
update Rotate model =
  updateQueue model (Q.rotate model.queue) "rotate"
update Top model =
  let
    mhead = Q.top model.queue
    mtail = Q.pop model.queue
  in
   case Tuple mhead mtail of
     Tuple Nothing _ -> noEffects $ changeFn model "top"
     Tuple _ Nothing -> noEffects $ changeFn model "top"
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
     Nothing -> noEffects $ changeFn model "pop"
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
     Tuple Nothing _ -> noEffects $ changeFn model "back"
     Tuple _ Nothing -> noEffects $ changeFn model "back"
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
     Nothing -> noEffects $ changeFn model "eject"
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
     Nothing -> noEffects $ changeFn model "inject"
     Just (Tuple node newModel) ->
       updateQueue newModel (Q.inject node cleanQueue) "inject"
update Push model =
  let
    mnode = mkNode Front model
    cleanQueue = wipeClasses model.queue
  in
   case mnode of
     Nothing -> noEffects $ changeFn model "push"
     Just (Tuple node newModel) ->
       updateQueue newModel (Q.push node cleanQueue) "push"
update (CurrentInput s) model =
  noEffects $ model { currInput = fromString s }
update ShowStructure model =
  noEffects $ changeFn model "Queue"

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
                                                , HE.onChange $ \t -> CurrentInput t.target.value
                                                ] [ ]]
                         ]
    controlDiv = H.div [ HA.className "pure-u-1-2" ] [ dataBtn
                                                     , emptyBtn
                                                     , rotateBtn
                                                     , frontDiv
                                                     , backDiv
                                                     , consSpan
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
