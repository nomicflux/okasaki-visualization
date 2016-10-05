module Okasaki exposing (..)

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
import Time
import Task

import Structures.Stack as Stack

type alias ClassNames = String
type alias ModelData = (Int, ClassNames)
type alias Model = { stack : Stack.Stack ModelData
                   , notice : Maybe String
                   , currInsert : String
                   , animation : Maybe A.Animation
                   , movement : Maybe Float
                   }

initialModel : Model
initialModel = { stack = Stack.empty
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
         | Failure String
         | StartAnimation Time.Time
         | Tick Time.Time
         | UpdateInsert String

toClass : Stack.Stack ModelData -> ClassNames -> Stack.Stack ModelData
toClass stack cl = Stack.map (\ (i, _) -> (i, cl)) stack

clearClasses : Stack.Stack ModelData -> Stack.Stack ModelData
clearClasses stack = toClass stack ""

highlightClass : ClassNames
highlightClass = "highlight"

highlightStack : Stack.Stack ModelData -> Stack.Stack ModelData
highlightStack stack = toClass stack highlightClass

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
                anim = A.animation t |> A.from 1 |> A.to 0 |> A.duration (500 * Time.millisecond)
            in
                ({model|animation=Just anim}, Cmd.none)
        Empty ->
            ({ model | stack = Stack.empty }, Cmd.none)
        IsEmpty ->
            let
                notice = if Stack.isEmpty model.stack then "Empty" else "Not Empty"
            in
                ({ model | notice = Just notice }, Cmd.none)
        Cons ->
            case model.currInsert |> toInt |> toMaybe of
                Nothing -> (model, Cmd.none)
                Just val ->
                    let
                        newVal = (val, "cons")
                        newStack = clearClasses model.stack
                    in
                        ({ model | stack = Stack.cons newVal newStack
                         , movement = Just 1
                         , animation = Nothing }
                        , Task.perform Failure StartAnimation Time.now)
        Head ->
            let
                newStack = clearClasses model.stack
                (notice, classedStack) =
                    case (Stack.head model.stack, Stack.tail model.stack) of
                        (Nothing, _) -> (Nothing, Stack.empty)
                        (_, Nothing) -> (Nothing, Stack.empty)
                        (Just (hi, hc), Just t) -> ( Just ("Head is: " ++ toString hi)
                                                   , Stack.cons (hi, "head") (clearClasses t))
            in
                ({ model | notice = notice, stack = classedStack }, Cmd.none)
        Tail ->
            let
                newStack = clearClasses model.stack
                (notice, classedStack) =
                    case (Stack.head model.stack, Stack.tail model.stack) of
                        (Nothing, _) -> (Nothing, Stack.empty)
                        (_, Nothing) -> (Nothing, Stack.empty)
                        (Just (hi, hc), Just t) -> ( Just ("Tail is: not implemented")
                                                   , Stack.cons (hi, "") (toClass t "tail"))
            in
                ({ model | notice = notice, stack = classedStack }, Cmd.none)
        UpdateInsert s ->
            ( { model | currInsert = s }, Cmd.none )

maxWidth : Int
maxWidth = 800

circRadius : Int
circRadius = 25

buffer : Int
buffer = 10

type alias Node = { pos : Int
                  , classes : ClassNames
                  }

calcPercent : Int -> Int -> Float
calcPercent x y = (toFloat x) / (toFloat y)

viewNode : Int
         -> Maybe Float
         -> (Int, Maybe Node, List (Svg Msg))
         -> ModelData
         -> (Int, Maybe Node, List (Svg Msg))
viewNode total animation (pos, formerNode, acc) (num, cl) =
    let
        realPos = if pos == total - 1
                  then (max ((toFloat pos) - (withDefault 0.0 animation)*1.0) 0.0)
                  else (toFloat pos)
        place =  realPos / (toFloat total)
        offset x = x + circRadius + buffer
        calcPos x = x * (toFloat maxWidth) |> round |> offset
        x1 = calcPos place
        y1 = offset 0
        circNode = circle [ SA.cx (toString x1)
                          , SA.cy (toString y1)
                          , SA.r (toString circRadius)
                          , SA.class (cl ++ " node")
                          ] [ ]
        numText = text' [ SA.x (toString x1)
                        , SA.y (toString (y1 + 6))
                        , SA.textAnchor "middle"
                        ] [ text (toString num)]
        edged = case formerNode of
                    Nothing -> acc
                    Just n ->
                        let
                            formerPlace = calcPercent n.pos total
                            x1' = x1 - circRadius
                            x2' = (calcPos formerPlace) + circRadius
                            gedge = line [ SA.x1 (x1' |> toString)
                                         , SA.y1 (toString y1)
                                         , SA.x2 (x2' |> toString)
                                         , SA.y2 (toString y1)
                                         , SA.class "edge"
                                         ] [ ]
                        in
                            if x2' < x1'
                                then (gedge :: acc)
                                else acc
    in
        ( pos + 1
        , Just { pos = pos, classes = cl }
        , circNode :: numText :: edged)

emptySvg : List (Svg Msg)
emptySvg = []

view : Model -> H.Html Msg
view model =
    let
        total = Stack.count model.stack
        svgDiv = svg [ SA.class "stack"
                     , SA.height "200px"
                     , SA.width "800px"
                     ] ( model.stack |>
                             Stack.foldl (viewNode total model.movement) (0, Nothing, emptySvg) |>
                             (\ (_, _, s) -> s)
                       )
        emptyBtn = H.button [ HA.class "btn", HE.onClick Empty ] [ H.text "Empty" ]
        insertBtn = H.span [ HA.class "input-btn-group" ]
                           [ H.input [ HA.type' "number"
                                     , HE.onInput UpdateInsert
                                     ] []
                           , H.button [ HA.class "btn"
                                      , HE.onClick Cons
                                      ] [ H.text "Cons"
                                        ]
                           ]
        headBtn = H.button [ HA.class "btn", HE.onClick Head ] [ H.text "Head" ]
        tailBtn = H.button [ HA.class "btn", HE.onClick Tail ] [ H.text "Tail" ]
    in
        H.div [ SA.class "app"] [ svgDiv
                                , H.div [ SA.class "btn-group" ] [ emptyBtn
                                                                 , headBtn
                                                                 , tailBtn
                                                                 , insertBtn ]
                                ]

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch [ Time.every (100 * Time.millisecond) Tick
                            ]

main : Program Never
main =
    Html.App.program
        { init = (initialModel, Cmd.none)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
