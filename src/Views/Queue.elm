module Views.Stack exposing (..)

import Html.App
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

import Structures.Stack as Stack

type alias ClassNames = String
type alias NodeID = Int
type alias ModelData = { nodeid : NodeID
                       , value : Int
                       , classes : ClassNames
                       }
type alias Model = { currStack : Stack.Stack ModelData
                   , currNodes : IntDict.IntDict Position
                   , prevNodes : IntDict.IntDict Position
                   -- , prevStack : Stack.Stack ModelData
                   , currId : Int
                   , notice : Maybe String
                   , currInsert : String
                   , animation : Maybe A.Animation
                   , movement : Maybe Float
                   }

initialModel : Model
initialModel = { currStack = Stack.empty
               , currNodes = IntDict.empty
               , prevNodes = IntDict.empty
               -- , prevStack = Stack.empty
               , currId = 0
               , notice = Nothing
               , currInsert = ""
               , animation = Nothing
               , movement = Nothing
               }

type Msg = Empty
         | IsEmpty
         | Cons
         | Head
         | Tail
         | TakeTail
         | Failure String
         | StartAnimation Time.Time
         | Tick Time.Time
         | UpdateInsert String

toClass : Stack.Stack ModelData -> ClassNames -> Stack.Stack ModelData
toClass stack cl = Stack.map (\ md -> {md | classes = cl}) stack

clearClasses : Stack.Stack ModelData -> Stack.Stack ModelData
clearClasses stack = toClass stack ""

highlightClass : ClassNames
highlightClass = "highlight"

highlightStack : Stack.Stack ModelData -> Stack.Stack ModelData
highlightStack stack = toClass stack highlightClass

updateStack : Model -> Stack.Stack ModelData -> Maybe String -> (Model, Cmd Msg)
updateStack model stack notice =
    let
        currTotal = Stack.count stack
        currNodes = stack |> Stack.foldl (nodePositions currTotal) (0, Nothing, IntDict.empty) |> \ (_, _, s) -> s
    in
        ({ model | prevNodes = model.currNodes
         , currNodes = currNodes
         , currStack = stack
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
            updateStack model Stack.empty (Just "Emptied")
        IsEmpty ->
            let
                notice = if Stack.isEmpty model.currStack then "Empty" else "Not Empty"
            in
                ({ model | notice = Just notice }, Cmd.none)
        Cons ->
            case model.currInsert |> toInt |> toMaybe of
                Nothing -> (model, Cmd.none)
                Just val ->
                    let
                        newVal = mkNode model.currId val "cons"
                        newStack = clearClasses model.currStack
                    in
                        updateStack {model | currId = model.currId + 1}
                            (Stack.cons newVal newStack)
                            (Just ("Added " ++ toString val))
        Head ->
            let
                newStack = clearClasses model.currStack
                (notice, classedStack) =
                    case (Stack.head model.currStack, Stack.tail model.currStack) of
                        (Nothing, _) -> (Nothing, Stack.empty)
                        (_, Nothing) -> (Nothing, Stack.empty)
                        (Just h, Just t) -> ( Just ("Head is: " ++ toString h.value)
                                            , Stack.cons { h | classes = "head" } (clearClasses t))
            in
                updateStack model classedStack notice
        Tail ->
            let
                newStack = clearClasses model.currStack
                (notice, classedStack) =
                    case (Stack.head model.currStack, Stack.tail model.currStack) of
                        (Nothing, _) -> (Nothing, Stack.empty)
                        (_, Nothing) -> (Nothing, Stack.empty)
                        (Just h, Just t) -> ( Just ("Tail is: not implemented")
                                                   , Stack.cons { h | classes = "" } (toClass t "tail"))
            in
                updateStack model classedStack notice
        TakeTail ->
            let
                newStack = clearClasses model.currStack
                (notice, classedStack) =
                    case (Stack.head model.currStack, Stack.tail model.currStack) of
                        (Nothing, _) -> (Nothing, Stack.empty)
                        (_, Nothing) -> (Nothing, Stack.empty)
                        (Just _, Just t) -> ( Just ("Tail is: not implemented")
                                            , toClass t "tail")
            in
                updateStack model classedStack notice
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
initialPos v = { x = toFloat maxWidth
               , y = 0.0
               , r = 0.0
               , val = v
               , prev = Nothing
               , classes = ""
               }

finalPos : Int -> Position
finalPos v = { x = -10.0
             , y = toFloat maxHeight
             , r = -1.0
             , val = v
             , prev = Nothing
             , classes = ""
             }

nodePositions : Int
              -> (Int, Maybe NodeID, IntDict.IntDict Position)
              -> ModelData
              -> (Int, Maybe NodeID, IntDict.IntDict Position)
nodePositions total (pos, prev, acc) currNode =
    let
        fwidth = toFloat maxWidth
        fheight = toFloat maxHeight
        r = min (toFloat circRadius) (fwidth / 3.0 / (toFloat total))
        offset x = x + (buffer |> toFloat) + r
        calcPos x = x * (toFloat maxWidth) |> offset
        circPos = { x = calcPercent pos total |> calcPos
                  , y = (fheight / 2.0)
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
                            Just e -> line [ SA.x1 (e.x + e.r |> toString)
                                           , SA.y1 (e.y |> toString)
                                           , SA.x2 (a.x - a.r |> toString)
                                           , SA.y2 (a.y |> toString)
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
                                      , HE.onClick Cons
                                      ] [ H.text "Cons"
                                        ]
                           , H.input [ HA.type' "number"
                                     , HE.onInput UpdateInsert
                                     ] []
                           ]
        headBtn = H.div [] [ H.button [ HA.class "btn", HE.onClick Head ] [ H.text "View Head" ] ]
        tailBtn = H.div [] [ H.button [ HA.class "btn", HE.onClick Tail ] [ H.text "View Tail" ] ]
        tailTakeBtn = H.div [] [ H.button [ HA.class "btn", HE.onClick TakeTail ] [ H.text "Take Tail" ] ]
    in
        H.div [ SA.class "app"] [ svgDiv
                                , H.div [ HA.class "buttons" ] [ emptyBtn
                                                               , headBtn
                                                               , tailBtn
                                                               , tailTakeBtn
                                                               , insertBtn
                                                               ]
                                ]

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch [ AF.times Tick
                            ]

main : Program Never
main =
    Html.App.program
        { init = (initialModel, Cmd.none)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
