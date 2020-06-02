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
  = WaitingSeconds Int
  | RunningUntil Time.Posix
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
    Tick newTime ->
      let
        (m2, cmd2) = processModel { model | time = newTime }
        (m3, cmd3) = processModel m2
        (m4, cmd4) = processModel m3
        (m5, cmd5) = processModel m4
      in
        (m5, Cmd.batch [cmd2, cmd3, cmd4, cmd5])
      
    StartTimer ->
      ( { model | program = program }
      , Cmd.none
      )

updateTimer : Time.Posix -> TimerState -> TimerState
updateTimer now state
  = case state of
    WaitingSeconds sec -> RunningUntil (Utils.plusSeconds sec now)

    RunningUntil when ->
      let
        secondsLeft = Utils.timeDiff when now
      in if secondsLeft <= 0 then
        Expired
      else
        RunningUntil when

    Expired -> Expired



-- processTimer : Time.Posix -> Maybe TimerState -> Int
-- processTimer now maybeTimer
--   = case maybeTimer of 
--     Nothing -> 

processModel : Model -> (Model, Cmd Msg)
processModel model =
  case model.timer of
    Nothing -> 
      case model.program of
        [] -> (model, Cmd.none)
        sec1::rest ->
          let newTimer = { state = WaitingSeconds sec1.seconds, info = sec1 }
          in
            ( { model | timer = Just newTimer, program = rest }
            , Cmd.none
            )

    Just { state, info } ->
      case state of
        Expired -> 
          ({ model | timer = Nothing }
          , playSound "hello"
          )

        _ -> 
          let
            newState = updateTimer model.time state
            newCurTimerInfo = { state = newState, info = info }
          in
            ( { model | timer = Just newCurTimerInfo }
            , Cmd.none
            )


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
