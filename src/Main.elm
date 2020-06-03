port module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Events exposing (onClick)
import Task
import Time
import Utils



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


port playSound : String -> Cmd msg



-- MODEL


type alias TimerInfo =
    { millis : Int
    , caption : String
    }


type PausableTimer
    = Running TimerInfo
    | Paused TimerInfo


type alias Schedule =
    List TimerInfo


type alias Model =
    { time : Maybe Time.Posix
    , timer : Maybe PausableTimer
    , schedule : Schedule
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing Nothing []
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | StartTimer
    | Pause
    | Run


schedule : Schedule
schedule =
    [ { millis = 3000, caption = "Three timer" }
    , { millis = 2000, caption = "Five timer" }
    , { millis = 1000, caption = "Five timer" }
    , { millis = 1000, caption = "Five timer" }
    , { millis = 1000, caption = "Five timer" }
    , { millis = 1000, caption = "Five timer" }
    , { millis = 5000, caption = "Five timer" }
    ]


pause : Maybe PausableTimer -> Maybe PausableTimer
pause maybeTimer =
    case maybeTimer of
        Nothing ->
            maybeTimer

        Just (Paused _) ->
            maybeTimer

        Just (Running info) ->
            Just (Paused info)


run : Maybe PausableTimer -> Maybe PausableTimer
run maybeTimer =
    case maybeTimer of
        Nothing ->
            maybeTimer

        Just (Running _) ->
            maybeTimer

        Just (Paused info) ->
            Just (Running info)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            processModel newTime model

        StartTimer ->
            ( { model | schedule = schedule }
            , Cmd.none
            )

        Pause ->
            ( { model | timer = pause model.timer }
            , Cmd.none
            )

        Run ->
            ( { model | timer = run model.timer }
            , Cmd.none
            )


popFromSchedule : Model -> Model
popFromSchedule model =
    case model.timer of
        Just _ ->
            model

        Nothing ->
            case model.schedule of
                [] ->
                    model

                t1 :: rest ->
                    { model
                        | timer = Just (Running t1)
                        , schedule = rest
                    }


processModel : Time.Posix -> Model -> ( Model, Cmd Msg )
processModel newTime model =
    let
        model2 =
            popFromSchedule model

        withNewTime =
            { model2 | time = Just newTime }
    in
    case model2.time of
        Nothing ->
            ( withNewTime
            , Cmd.none
            )

        Just oldTime ->
            let
                timePassed =
                    Utils.timeDiff newTime oldTime
            in
            processModelWithTimePassed timePassed withNewTime


processModelWithTimePassed : Int -> Model -> ( Model, Cmd Msg )
processModelWithTimePassed timePassed model =
    case model.timer of
        Nothing ->
            ( model, Cmd.none )

        Just (Paused _) ->
            ( model, Cmd.none )

        Just (Running ({ millis } as timerInfo)) ->
            let
                newMillis =
                    millis - timePassed
            in
            if newMillis <= 0 then
                let
                    withNextTimer =
                        popFromSchedule { model | timer = Nothing }
                in
                ( withNextTimer, playSound "hello" )

            else
                ( { model | timer = Just (Running { timerInfo | millis = newMillis }) }
                , Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrame Tick



-- VIEW


showTimer : TimerInfo -> Html Msg
showTimer { millis, caption } =
    Html.div []
        [ Html.h2 [] [ Html.text caption ]
        , Html.div [] [ Html.text (String.fromInt (millis // 100)) ]
        ]


view : Model -> Html Msg
view model =
    case model.timer of
        Just (Running timer) ->
            Html.div []
                [ showTimer timer
                , Html.button [ onClick Pause ] [ Html.text "Pause" ]
                ]

        Just (Paused timer) ->
            Html.div []
                [ showTimer timer
                , Html.button [ onClick Run ] [ Html.text "Run" ]
                ]

        _ ->
            Html.div []
                [ Html.button [ onClick StartTimer ] [ Html.text "Start" ]
                ]
