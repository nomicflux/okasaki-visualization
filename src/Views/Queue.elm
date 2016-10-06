module Views.Queue exposing (..)

-- import Html.App
import Html as H
import Html.Attributes as HA
import Svg exposing (..)
import Svg.Attributes as SA
import Html.Events as HE
import String exposing (toInt)
import Maybe exposing (Maybe, withDefault)
import Result exposing (toMaybe)
import Animation as A
import AnimationFrame as AF
import Time
import Task
import IntDict
import Debug

import Structures.Queue as Queue
import Structures.Stack as Stack

type alias ClassNames = String
type alias NodeID = Int
type alias ModelData = { nodeid : NodeID
                       , value : Int
                       , classes : ClassNames
                       }
type alias Model = { currQueue : Queue.Queue ModelData
                   , currNodes : IntDict.IntDict Position
                   , prevNodes : IntDict.IntDict Position
                   , currId : Int
                   , notice : Maybe String
                   , currInsert : String
                   , animation : Maybe A.Animation
                   , movement : Maybe Float
                   }

initialModel : Model
initialModel = { currQueue = Queue.empty
               , currNodes = IntDict.empty
               , prevNodes = IntDict.empty
               , currId = 0
               , notice = Nothing
               , currInsert = ""
               , animation = Nothing
               , movement = Nothing
               }

type Msg = Empty
         | IsEmpty
         | Front
         | Back
         | Eject
         | Push
         | Enqueue
         | Dequeue
         | Failure String
         | StartAnimation Time.Time
         | Tick Time.Time
         | UpdateInsert String

toClass : Queue.Queue ModelData -> ClassNames -> Queue.Queue ModelData
toClass queue cl = Queue.map (\ md -> {md | classes = cl}) queue

clearClasses : Queue.Queue ModelData -> Queue.Queue ModelData
clearClasses queue = toClass queue ""

updateQueue : Model -> Queue.Queue ModelData -> Maybe String -> (Model, Cmd Msg)
updateQueue model queue notice =
    let
        quarterHeight = (toFloat maxHeight) / 4.0
        currSize = max (Queue.countLeft queue) (Queue.countRight queue)
        leftNodes = queue |>
                    Queue.left |>
                    Stack.foldl (nodePositions quarterHeight currSize)
                        (0, Nothing, IntDict.empty) |>
                    \ (_, _, s) -> s
        rightNodes = queue |>
                     Queue.right |>
                     Stack.foldl (nodePositions (3.0 * quarterHeight) currSize)
                         (0, Nothing, IntDict.empty) |>
                     \ (_, _, s) -> s
    in
        ({ model | prevNodes = model.currNodes
         , currNodes = IntDict.union leftNodes rightNodes
         , currQueue = queue
         , notice = notice
         , movement = Just 0
         , animation = Nothing }
        , Task.perform Failure StartAnimation Time.now)

mkNode : NodeID -> Int -> ClassNames -> ModelData
mkNode nid val cl = { nodeid = nid
                    , value = val
                    , classes = cl
                    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Failure s ->
            (model, Cmd.none)
        Tick t ->
            let
                mvmt = case model.animation of
                           Nothing -> Nothing
                           Just a -> Just (A.animate t a)
            in
                ({ model | movement = mvmt}, Cmd.none)
        StartAnimation t ->
            let
                anim = A.animation t |> A.duration (500 * Time.millisecond)
            in
                ({model|animation=Just anim}, Cmd.none)
        Empty ->
            updateQueue model Queue.empty (Just "Emptied")
        IsEmpty ->
            let
                notice = if Queue.isEmpty model.currQueue then "Empty" else "Not Empty"
            in
                ({ model | notice = Just notice }, Cmd.none)
        Enqueue ->
            case model.currInsert |> toInt |> toMaybe of
                Nothing -> (model, Cmd.none)
                Just val ->
                    let
                        newVal = mkNode model.currId val "cons"
                        newQueue = clearClasses model.currQueue
                    in
                        updateQueue {model | currId = model.currId + 1}
                            (Queue.enqueue newVal newQueue)
                            (Just ("Added " ++ toString val))
        Push ->
            case model.currInsert |> toInt |> toMaybe of
                Nothing -> (model, Cmd.none)
                Just val ->
                    let
                        newVal = mkNode model.currId val "cons"
                        newQueue = clearClasses model.currQueue
                    in
                        updateQueue {model | currId = model.currId + 1}
                            (Queue.push newVal newQueue)
                            (Just ("Added " ++ toString val))
        Front ->
            let
                newQueue = clearClasses model.currQueue
                (notice, classedQueue) =
                    case (Queue.front model.currQueue, Queue.dequeue model.currQueue) of
                        (Nothing, _) -> (Nothing, Queue.empty)
                        (_, Nothing) -> (Nothing, Queue.empty)
                        (Just h, Just t) -> ( Just ("Head is: " ++ toString h.value)
                                            , Queue.push { h | classes = "head" } (clearClasses t))
            in
                updateQueue model classedQueue notice
        Back ->
            let
                newQueue = clearClasses model.currQueue
                (notice, classedQueue) =
                    case (Queue.back model.currQueue, Queue.eject model.currQueue) of
                        (Nothing, _) -> (Nothing, Queue.empty)
                        (_, Nothing) -> (Nothing, Queue.empty)
                        (Just h, Just t) -> ( Just ("Head is: " ++ toString h.value)
                                            , Queue.enqueue { h | classes = "head" } (clearClasses t))
            in
                updateQueue model classedQueue notice
        Eject ->
            let
                newQueue = clearClasses model.currQueue
                (notice, classedQueue) =
                    case (Queue.back model.currQueue, Queue.eject model.currQueue) of
                        (Nothing, _) -> (Nothing, Queue.empty)
                        (_, Nothing) -> (Nothing, Queue.empty)
                        (Just h, Just t) -> ( Just ("Tail is: not implemented")
                                                   , (toClass t "tail"))
            in
                updateQueue model classedQueue notice
        Dequeue ->
            let
                newQueue = clearClasses model.currQueue
                (notice, classedQueue) =
                    case (Queue.front model.currQueue, Queue.dequeue model.currQueue) of
                        (Nothing, _) -> (Nothing, Queue.empty)
                        (_, Nothing) -> (Nothing, Queue.empty)
                        (Just _, Just t) -> ( Just ("Tail is: not implemented")
                                            , toClass t "tail")
            in
                updateQueue model classedQueue notice
        UpdateInsert s ->
            ( { model | currInsert = s }, Cmd.none )

maxWidth : Int
maxWidth = 800

maxHeight : Int
maxHeight = 200

circRadius : Int
circRadius = 40

buffer : Int
buffer = 10

type alias Node = { pos : Int
                  , classes : ClassNames
                  }

calcPercent : Int -> Int -> Float
calcPercent x y = (toFloat x) / (toFloat y)

type alias Position = { x : Float
                      , y : Float
                      , r : Float
                      , val : Int
                      , prev : Maybe NodeID
                      , classes : ClassNames
                      }

initialPos : Int -> Position
initialPos v = { x = toFloat maxWidth + 10.0
               , y = 0.0
               , r = 0.0
               , val = v
               , prev = Nothing
               , classes = ""
               }

finalPos : Int -> Position
finalPos v = { x = -10.0
             , y = toFloat maxHeight
             , r = 0.0
             , val = v
             , prev = Nothing
             , classes = ""
             }

nodePositions : Float
              -> Int
              -> (Int, Maybe NodeID, IntDict.IntDict Position)
              -> ModelData
              -> (Int, Maybe NodeID, IntDict.IntDict Position)
nodePositions yplace total (pos, prev, acc) currNode =
    let
        fwidth = toFloat maxWidth
        r = min (toFloat circRadius) (fwidth / 3.0 / (toFloat total))
        offset x = x + (buffer |> toFloat) + r
        calcPos x = x * (toFloat maxWidth) |> offset
        circPos = { x = calcPercent pos total |> calcPos
                  , y = yplace
                  , r = r
                  , val = currNode.value
                  , prev = prev
                  , classes = currNode.classes
                  }
    in
        (pos + 1
        , Just currNode.nodeid
        , IntDict.insert currNode.nodeid circPos acc
        )

interpolateF : Float
             -> Float
             -> Float
             -> Float
interpolateF t a b =
    let
        diff = b - a
    in
        a + t*diff

interpolate : Float
            -> Position
            -> Position
            -> Position
interpolate mvmt former curr =
    let
        x = interpolateF mvmt former.x curr.x
        y = interpolateF mvmt former.y curr.y
        r = interpolateF mvmt former.r curr.r
    in
        { curr | x = x
        , y = y
        , r = r
        }

compNodes : Maybe Float
          -> IntDict.IntDict Position
          -> IntDict.IntDict Position
          -> List (Svg Msg)
compNodes mvmt prev curr =
    let
        getCurr i v = withDefault (finalPos v) (IntDict.get i curr)
        getPrev i v = withDefault (initialPos v) (IntDict.get i prev)
        m = withDefault 0.0 mvmt
        interpolateFrom i a = interpolate m a (getCurr i a.val)
        interpolateTo i a = interpolate m (getPrev i a.val) a
        exit = IntDict.filter (\ i _ -> IntDict.get i curr == Nothing) prev
        nodesIn = IntDict.map interpolateTo curr
        nodesOut = IntDict.map interpolateFrom exit
        nodes = IntDict.union nodesIn nodesOut
        svgs i a acc =
            let
                otherEdge = case a.prev of
                                Nothing -> Nothing
                                Just p -> Just (withDefault (initialPos a.val) (IntDict.get p nodes))
                edged = case otherEdge of
                            Nothing -> acc
                            Just e ->
                                let
                                    tangent = (a.y - e.y) / (a.x - e.x)
                                    invtan = atan tangent
                                    signX = if a.x > e.x then 1 else -1
                                    signY = if a.y > e.y then 1 else -1
                                in
                                    line [ SA.x1 (e.x + signX*e.r*(cos invtan) |> toString)
                                         , SA.y1 (e.y - signY*e.r*(sin invtan) |> toString)
                                         , SA.x2 (a.x - signX*a.r*(cos invtan) |> toString)
                                         , SA.y2 (a.y + signY*a.r*(sin invtan) |> toString)
                                         , SA.class "edge"
                                         ] [] :: acc
                circ = circle [ SA.cx (a.x |> toString)
                              , SA.cy (a.y |> toString)
                              , SA.r (a.r |> toString)
                              , SA.class (a.classes ++ " node")
                              ] [ ]
                numText = text' [ SA.x (a.x |> toString)
                                , SA.y (a.y + 6 |> toString)
                                , SA.textAnchor "middle"
                                ] [ text (a.val |> toString) ]
            in
                circ :: numText :: edged
        folded = IntDict.foldl svgs [] nodes
    in
        folded

view : Model -> H.Html Msg
view model =
    let
        svgDiv = svg [ SA.class "stack"
                     , SA.height ((toString maxHeight) ++ "px")
                     , SA.width ((toString maxWidth) ++ "px")
                     ] ( compNodes model.movement model.prevNodes model.currNodes )
        emptyBtn = H.div [] [ H.button [ HA.class "btn", HE.onClick Empty ] [ H.text "Empty" ] ]
        insertBtn = H.div [ HA.class "input-btn-group" ]
                           [ H.button [ HA.class "btn"
                                      , HE.onClick Enqueue
                                      ] [ H.text "Enqueue"
                                        ]
                           , H.button [ HA.class "btn-large"
                                      , HE.onClick Push
                                      ] [ H.text "Push" ]
                           , H.input [ HA.type' "number"
                                     , HE.onInput UpdateInsert
                                     ] []
                           ]
        frontBtn = H.div [] [ H.button [ HA.class "btn", HE.onClick Front ] [ H.text "View Front" ] ]
        backBtn = H.div [] [ H.button [ HA.class "btn", HE.onClick Back ] [ H.text "View Back" ] ]
        dequeueBtn = H.div [] [ H.button [ HA.class "btn", HE.onClick Dequeue ] [ H.text "Dequeue" ] ]
        ejectBtn = H.div [] [ H.button [ HA.class "btn", HE.onClick Eject ] [ H.text "Eject" ] ]
    in
        H.div [ SA.class "app"] [ svgDiv
                                , H.div [ HA.class "buttons" ] [ emptyBtn
                                                               , frontBtn
                                                               , backBtn
                                                               , dequeueBtn
                                                               , ejectBtn
                                                               , insertBtn
                                                               ]
                                ]

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch [ AF.times Tick
                            ]

-- main : Program Never
-- main =
--     Html.App.program
--         { init = (initialModel, Cmd.none)
--         , view = view
--         , update = update
--         , subscriptions = subscriptions
--         }
