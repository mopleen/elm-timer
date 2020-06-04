port module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
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
    , pauseBefore : Bool
    , preTimer : Bool
    }


type alias PausableTimer =
    { timerInfo : TimerInfo
    , paused : Bool
    }


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
    | Skip


type ScheduleCommand
    = CountDown { seconds : Int, caption : String }
    | Wait


mkT : String -> Int -> ScheduleCommand
mkT caption sec =
    CountDown { seconds = sec, caption = caption }


timeSpeed : Int
timeSpeed =
    1


timePeriod : Int
timePeriod =
    30


twoPeriods : Int
twoPeriods =
    2 * timePeriod


program : List ScheduleCommand
program =
    [ -- Warmup
      Wait
    , mkT "Elbow drive" twoPeriods
    , mkT "Hitchhike thumb rotation" twoPeriods
    , mkT "Elbow rotate one direction" timePeriod
    , mkT "Elbow rotate another direction" timePeriod
    , mkT "Hands rotate one direction" timePeriod
    , mkT "Hands rotate another direction" timePeriod
    , mkT "Cat" twoPeriods

    -- Workout
    -- 2 flows
    , Wait
    , mkT "Break #1" (2 * twoPeriods)

    -- 1 flow
    , Wait
    , mkT "Break #2" (2 * twoPeriods)

    -- Fourth random workout
    , Wait
    , mkT "Random workout" (2 * twoPeriods)

    -- Cooldown
    , Wait
    , mkT "Left hand grip" timePeriod
    , mkT "Right hand grip" timePeriod
    , mkT "Elbow couch" timePeriod
    , mkT "Stretch couch" timePeriod
    , mkT "Forward lean one hand" timePeriod
    , mkT "Forward lean another hand" timePeriod
    , mkT "Shoulder stretch one" timePeriod
    , mkT "Shoulder stretch another" timePeriod
    , mkT "Sphinx" twoPeriods
    ]


schedule : Schedule
schedule =
    let
        s =
            { waiting = False, result = [] }

        f item s1 =
            case item of
                Wait ->
                    { s1 | waiting = True }

                CountDown { seconds, caption } ->
                    let
                        timer =
                            { millis = seconds * 1000
                            , caption = caption
                            , pauseBefore = False
                            , preTimer = False
                            }

                        preTimer =
                            { millis = 3500
                            , caption = "➡️ " ++ timer.caption
                            , pauseBefore = s1.waiting
                            , preTimer = True
                            }
                    in
                    { s1
                        | result = timer :: preTimer :: s1.result
                        , waiting = False
                    }

        result =
            List.foldl f s program
    in
    List.reverse result.result


pause : Maybe PausableTimer -> Maybe PausableTimer
pause =
    Maybe.map (\t -> { t | paused = True })


run : Maybe PausableTimer -> Maybe PausableTimer
run =
    Maybe.map (\t -> { t | paused = False })


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

        Skip ->
            ( popFromSchedule { model | timer = Nothing }
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
                    let
                        newTimer =
                            { timerInfo = t1, paused = t1.pauseBefore }
                    in
                    { model
                        | timer = Just newTimer
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
                    timeSpeed
                        * Utils.timeDiff newTime oldTime
            in
            processModelWithTimePassed timePassed withNewTime


decreaseTimer : Int -> PausableTimer -> ( Maybe PausableTimer, Cmd Msg )
decreaseTimer timePassed pausableTimer =
    if pausableTimer.paused then
        ( Just pausableTimer, Cmd.none )

    else
        let
            timerInfo =
                pausableTimer.timerInfo

            newMillis =
                timerInfo.millis - timePassed

            oldSec =
                timerInfo.millis // 1000

            newSec =
                newMillis // 1000

            beepSound =
                if newSec <= 2 && oldSec /= newSec then
                    playSound "beep"

                else
                    Cmd.none

            endSound =
                if timerInfo.preTimer then
                    "long_beep"

                else
                    "long_beep"

            newTimerInfo =
                { timerInfo | millis = newMillis }
        in
        if newMillis <= 0 then
            ( Nothing, playSound endSound )

        else
            ( Just { pausableTimer | timerInfo = newTimerInfo }, beepSound )


processModelWithTimePassed : Int -> Model -> ( Model, Cmd Msg )
processModelWithTimePassed timePassed model =
    case model.timer of
        Nothing ->
            ( model, Cmd.none )

        Just pausableTimer ->
            let
                ( maybePausableTimer, cmd ) =
                    decreaseTimer timePassed pausableTimer
            in
            case maybePausableTimer of
                Nothing ->
                    ( popFromSchedule { model | timer = Nothing }
                    , cmd
                    )

                Just newPausableTimer ->
                    ( { model | timer = Just newPausableTimer }
                    , cmd
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
        , Html.div [ Html.Attributes.class "timer" ] [ Html.text (String.fromInt (millis // 1000)) ]
        ]


view : Model -> Html Msg
view model =
    case model.timer of
        Just { timerInfo, paused } ->
            let
                runPause =
                    if paused then
                        Html.button [ onClick Run ] [ Html.text "Run" ]

                    else
                        Html.button [ onClick Pause ] [ Html.text "Pause" ]
            in
            Html.div []
                [ showTimer timerInfo
                , runPause
                , Html.button [ onClick Skip ] [ Html.text "Skip" ]
                ]

        Nothing ->
            Html.div []
                [ Html.button [ onClick StartTimer ] [ Html.text "Start" ]
                ]
