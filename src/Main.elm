module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Events exposing (onClick)
import Task
import Time



-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type TimerState
  = WaitingSeconds Int
  | RunningUntil Time.Posix
  | Expired

type alias CountdownTimerData =
  { 

  }

type alias TimerProgram = List Int


type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  , elapseAt : Maybe Time.Posix
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0) Nothing
  , Cmd.none
  )



-- UPDATE


type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone
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
      ( processModel { model | time = newTime }
      , Cmd.none
      )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )

    StartTimer ->
      ( { model | elapseAt = Just (plusSeconds 5 model.time) }
      , Cmd.none
      )

processModel : Model -> Model
processModel model =
  case model.elapseAt of
    Nothing -> model
    Just when ->
      let
        secondsLeft = timeDiff when model.time
      in if secondsLeft < 0 then
        { model | elapseAt = Nothing }
      else model

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Time.every 50 Tick



-- VIEW


view : Model -> Html Msg
view model =
  case model.elapseAt of
    Nothing ->
      Html.div []
        [
          Html.button [ onClick StartTimer ] [ Html.text "Start" ]
        ]

    Just when ->
      let
        secondsLeft = timeDiff when model.time
      in
        Html.div []
          [
            Html.div [] [ Html.text (String.fromInt secondsLeft) ]
          ]
