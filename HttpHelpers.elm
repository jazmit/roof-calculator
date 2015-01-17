module HttpHelpers where

import Signal
import List
import String
import Http (Response(..))


-- If only Elm had monad tranformers...
combineSignals : List (Signal a) -> Signal (List a)
combineSignals = List.foldl (Signal.map2 (::)) (Signal.constant [])

combineResponses : List (Response a) -> Response (List a)
combineResponses = List.foldl (map2Response (::)) (Success [])

map2Response : (a -> b -> c) -> Response a -> Response b -> Response c
map2Response fn req1 req2 = case (req1, req2) of
    (Success u, Success v)    -> Success (fn u v)
    (Failure errNo errMsg, _) -> Failure errNo errMsg
    (_, Failure errNo errMsg) -> Failure errNo errMsg
    _                         -> Waiting

tagResponse : String -> Signal (Response a) -> Signal (Response (String, a))
tagResponse name = Signal.map (mapResponse (\v -> (name, v)))

mapResponse : (a -> b) -> Response a -> Response b
mapResponse fn res = case res of
    Success x         -> Success (fn x)
    Waiting           -> Waiting
    Failure errNo msg -> Failure errNo msg

