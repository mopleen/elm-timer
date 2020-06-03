port module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes
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
    , pauseBefore : Bool
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


mkT : String -> Int -> TimerInfo
mkT caption sec =
    { millis = sec * 1000, caption = caption, pauseBefore = False }


schedule : Schedule
schedule =
    [ -- Warmup
      mkT "Get ready..." 3
    , mkT "Elbow drive" 60
    , mkT "Hitchhike thumb rotation" 60
    , mkT "Elbow rotate one direction" 30
    , mkT "Elbow rotate another direction" 30
    , mkT "Hands rotate one direction" 30
    , mkT "Hands rotate another direction" 30
    , mkT "Cat" 60

    -- Workout
    -- 2 flows
    , { millis = 2 * 60 * 1000, caption = "Break #1", pauseBefore = True }

    -- 1 flow
    , { millis = 2 * 60 * 1000, caption = "Break #2", pauseBefore = True }

    -- Fourth random workout
    , { millis = 2 * 60 * 1000, caption = "Random workout", pauseBefore = True }

    -- Cooldown
    , { millis = 30 * 1000, caption = "Left hand grip", pauseBefore = True }
    , mkT "Right hand grip" 30
    , mkT "Elbow couch" 30
    , mkT "Stretch couch" 30
    , mkT "Forward lean one hand" 30
    , mkT "Forward lean another hand" 30
    , mkT "Shoulder stretch one" 30
    , mkT "Shoulder stretch another" 30
    , mkT "Sphinx" 60
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
                    let
                        newTimer =
                            if t1.pauseBefore then
                                Paused t1

                            else
                                Running t1
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
        , Html.div [ Html.Attributes.class "timer" ] [ Html.text (String.fromInt (millis // 1000)) ]
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
