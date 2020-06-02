module Utils exposing (plusSeconds, timeDiff)

import Time

plusSeconds : Int -> Time.Posix -> Time.Posix
plusSeconds sec t1 = 
  let millis = sec*1000 + Time.posixToMillis t1
  in Time.millisToPosix millis

timeDiff : Time.Posix -> Time.Posix -> Int
timeDiff t2 t1 =
  let 
    t2m = Time.posixToMillis t2
    t1m = Time.posixToMillis t1
  in t2m-t1m
