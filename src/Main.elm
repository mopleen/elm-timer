port module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Events exposing (onClick)
import Task
import Time



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

type alias TimerProgram = List Int


type alias Model =
  { time : Time.Posix
  , timer : Maybe TimerState
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

plusSeconds : Int -> Time.Posix -> Time.Posix
plusSeconds sec t1 = 
  let millis = sec*1000 + Time.posixToMillis t1
  in Time.millisToPosix millis

timeDiff : Time.Posix -> Time.Posix -> Int
timeDiff t2 t1 =
  let 
    t2m = Time.posixToMillis t2
    t1m = Time.posixToMillis t1
  in (t2m-t1m) // 1000

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      processModel { model | time = newTime }

    StartTimer ->
      ( { model | program = [3,5] }
      , Cmd.none
      )

updateTimer : Time.Posix -> TimerState -> TimerState
updateTimer now state
  = case state of
    WaitingSeconds sec -> RunningUntil (plusSeconds sec now)

    RunningUntil when ->
      let
        secondsLeft = timeDiff when now
      in if secondsLeft < 0 then
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
          ( { model | timer = Just (WaitingSeconds sec1), program = rest }
          , Cmd.none
          )

    Just Expired -> 
      ({ model | timer = Nothing }
      , playSound "hello"
      )

    Just timerState -> 
      ( { model | timer = Just (updateTimer model.time timerState) }
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
    Just (RunningUntil when) ->
      let
        secondsLeft = timeDiff when model.time
      in
        Html.div []
          [
            Html.div [] [ Html.text (String.fromInt secondsLeft) ]
          ]

    _ ->
      Html.div []
        [
          Html.button [ onClick StartTimer ] [ Html.text "Start" ]
        ]
