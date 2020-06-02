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

type TimerState
  = RunningUntil Time.Posix
  | Expired

type alias TimerInfo =
  { seconds : Int
  , caption : String
  }

type alias TimerProgram = List TimerInfo

type alias CurrentTimerInfo =
  { state : TimerState
  , info : TimerInfo
  }


type alias Model =
  { time : Time.Posix
  , timer : Maybe CurrentTimerInfo
  , program : TimerProgram
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model (Time.millisToPosix 0) Nothing []
  , Cmd.none
  )



-- UPDATE


type Msg
  = Tick Time.Posix
  | StartTimer

program : TimerProgram
program =
  [ { seconds = 3, caption = "Three timer" }
  , { seconds = 5, caption = "Five timer" }
  ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime -> processModel { model | time = newTime }
      
    StartTimer ->
      ( { model | program = program }
      , Cmd.none
      )

updateTimer : Time.Posix -> TimerState -> TimerState
updateTimer now state =
  case state of
    RunningUntil when ->
      let
        secondsLeft = Utils.timeDiff when now
      in if secondsLeft <= 0 then
        Expired
      else
        RunningUntil when

    Expired -> Expired




startTimer : Time.Posix -> TimerProgram -> (TimerProgram, Maybe CurrentTimerInfo)
startTimer now timers =
  case timers of
    [] -> ([], Nothing)
    t1::rest ->
      let
        timerState = RunningUntil (Utils.plusSeconds t1.seconds now)
      in (rest, Just { state = timerState, info = t1 })

popTimer : Model -> Model
popTimer model =
  let
    (newProgram, newTimer) = startTimer model.time model.program
  in
    { model | timer = newTimer, program = newProgram }

processModel : Model -> (Model, Cmd Msg)
processModel model =
  case model.timer of
    Nothing -> (popTimer model, Cmd.none)

    Just { state } ->
      let
        state2 = updateTimer model.time state
      in case state2 of
        Expired -> (popTimer model, playSound "hello")

        _ -> (model, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Browser.Events.onAnimationFrame Tick



-- VIEW


view : Model -> Html Msg
view model =
  case model.timer of
    Just { state, info } ->
      case state of
        RunningUntil when ->
          let
            secondsLeft = (Utils.timeDiff when model.time)//1000 + 1
          in
            Html.div []
              [ Html.h2 [] [ Html.text info.caption ]
              , Html.div [] [ Html.text (String.fromInt secondsLeft) ]
              ]
        _ -> 
          Html.div []
            [ Html.button [ onClick StartTimer ] [ Html.text "Start" ]
            ]
    _ ->
      Html.div []
        [ Html.button [ onClick StartTimer ] [ Html.text "Start" ]
        ]
