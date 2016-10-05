module Okasaki exposing (..)

import Html.App
import Html as H
import Html.Attributes as HA
import Html.Events as HE

import Views.Queue as VQ
import Views.Stack as VS

type DataStructure = Stack
                   | Queue
                   | None

type alias Model = { currDS : DataStructure
                   , stackModel : VS.Model
                   , queueModel : VQ.Model
                   }

initialModel : Model
initialModel = { currDS = None
               , stackModel = VS.initialModel
               , queueModel = VQ.initialModel
               }

type Msg = ChangeDS DataStructure
         | StackMsg VS.Msg
         | QueueMsg VQ.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangeDS ds ->
            ( {model | currDS = ds},  Cmd.none )
        StackMsg msg ->
            let
                (smodel, scmd) = VS.update msg model.stackModel
            in
                ( { model | stackModel = smodel }, Cmd.map StackMsg scmd )
        QueueMsg msg ->
            let
                (qmodel, qcmd) = VQ.update msg model.queueModel
            in
                ( { model | queueModel = qmodel }, Cmd.map QueueMsg qcmd )


view : Model -> H.Html Msg
view model =
    let
        base = H.div [ HA.class "ds-btns" ]
                     [ H.button [ HA.class "btn-large btn-primary"
                                , HE.onClick (ChangeDS Stack)] [ H.text "Stack" ]
                     , H.button [ HA.class "btn-arge btn-primary"
                                , HE.onClick (ChangeDS Queue)] [ H.text "Queue" ]
                     ]
        rest =
            case model.currDS of
                Stack -> Html.App.map (StackMsg) (VS.view (model.stackModel))
                Queue -> Html.App.map (QueueMsg) (VQ.view (model.queueModel))
                None -> H.div [ ] [ H.text "No data structure selected yet" ]
    in
        H.div [ HA.class "elm-app" ]
            [ base
            , rest
            ]

subscriptions : Model -> Sub Msg
subscriptions model =
    let
        stackSubs = Sub.map (StackMsg) (VS.subscriptions model.stackModel)
        queueSubs = Sub.map (QueueMsg) (VQ.subscriptions model.queueModel)
    in
        Sub.batch [ stackSubs, queueSubs ]


main : Program Never
main =
    Html.App.program
        { init = (initialModel, Cmd.none)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
