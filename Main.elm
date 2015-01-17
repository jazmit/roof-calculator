module RoofCalculator where

import Graphics.Element (..)
import Graphics.Input (..)
import Signal (channel, Channel, Signal, map4, send, (<~), (~), subscribe)
import Signal
import List
import Text (asText)
import Text
import Math.Vector3 (Vec3, vec3, dot, cross, toRecord, length)
import String
import Dict
import Dict (Dict)
import Json.Encode
import Http (Response(..))
import HttpHelpers (..)
import WebGL

-- Graphics
import Graphics.Collage (..)
import Color (..)
import Window

-- HTML form
import Html
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Basics

-- The 3D view module
import ThreeDView (..)

-- Represents application state
type alias Params = { w : Float
                    , l : Float
                    , h : Float
                    }

main : Signal Html
main = draw <~ inputs ~ Window.dimensions ~ getTextures textures

inputs = (,,) <~ (makeInput "W" (toString initialParams.w))
               ~ (makeInput "L" (toString initialParams.l))
               ~ (makeInput "H" (toString initialParams.h))


textures = ["roof", "wall", "ruler", "rise", "end-run", "side-run"]

getTextures : List String -> Signal (Response (String -> WebGL.Texture))
getTextures names =
    let requestSignals = List.map makeReqSignal names -- List (Signal (Response (String, WebGL.Texture)))
        makeReqSignal name = "/" ++ name ++ "-texture.png" |> WebGL.loadTexture |> tagResponse name
        getUnsafe list key = case Dict.get key (Dict.fromList list) of
            Just value -> value
    in  requestSignals |> combineSignals
                       |> Signal.map (combineResponses >> mapResponse getUnsafe)

paramSignal = 
    let parseValue v = case String.toFloat v of
            Err e -> defaultValue
            Ok  newV -> newV
        getFirst ((wv,b), (lv,d), (hv,f)) =
            {w = parseValue wv, l = parseValue lv, h = parseValue hv }
     in getFirst <~ inputs

draw : ((String, Html), (String, Html), (String, Html)) -> (Int, Int) 
       -> Response (String -> WebGL.Texture) -> Html
draw ((w, wEl), (l, lEl), (h, hEl)) (winWidth, winHeight) textureRequests = 
    let params = Params (parse w) (parse l) (parse h)
        parse str = case (String.toFloat str) of
            Ok res  -> res
            Err err -> 0
        n = northRoof params
        s = southRoof params
        e = eastRoof  params
        a = ridgeAngle n s
        ridgeDiagWidth = 400
        ridgeDiagrams = container winWidth (ridgeDiagWidth // 2) middle <|
            ridgeDiagram ridgeDiagWidth "Ridge angle" (ridgeAngle n s) `beside`
            ridgeDiagram ridgeDiagWidth "Hip angle" (ridgeAngle n e)
        forms = Html.form [class "form-horizontal col-xs-6"] [
                bsFormGroup "Side run (meters):" "W" wEl,
                bsFormGroup "End run (meters):" "L" lEl,
                bsFormGroup "Rise (meters):" "H" hEl
            ]
        threeDView = case textureRequests of
            Success textures ->
                div [class "col-xs-6 bordered"] [
                    fromElement <| make3DView textures (winWidth // 2, winWidth // 2) params
                ]
            Waiting ->
                div [] [text "Waiting for texture"]
            Failure errNo msg -> div [] [text "Failed to fetch textures"]
        bsFormGroup : String -> String -> Html -> Html
        bsFormGroup lbl myId formControl =
            div [class "form-group row"] [
                label [class "control-label col-sm-4", for myId] [ text lbl ],
                div [class "col-sm-8"] [ formControl ]
            ]
    in div []
        [ div [class "row"] [forms , threeDView]
        , div [class "row"] [fromElement ridgeDiagrams]
        ]

makeInput : String -> String -> Signal (String, Html)
makeInput myId defaultValue =
    let updateChannel = channel defaultValue
        valueToHtml v = 
            input [type' "text"
                  , class "form-control"
                  , id myId
                  , placeholder ""
                  , tabindex 0
                  , value v
                  , on "input" targetValue (send updateChannel)] []
    in  (\v -> (v, valueToHtml v)) <~ subscribe updateChannel


-- Calculation functions
-- There are north, east and south - facing roof sections
-- The x-axis points east, y north and z up

northRoof : Params -> Vec3
northRoof p = vec3 0 p.h p.w

southRoof : Params -> Vec3
southRoof p = vec3 0 (-p.h) p.w

eastRoof : Params -> Vec3
eastRoof p = vec3 p.h 0 p.l

ridgeAngle : Vec3 -> Vec3 -> Float
ridgeAngle r1 r2 = pi - acos (dot r1 r2 / length r1 / length r2)

-- -- Drawing functions
showAngle : Float -> Element
showAngle angle = Text.plainText
    <| toString (round <| angle / degrees 1) ++ " degrees"

pointAt : Float -> Float -> (Float, Float)
pointAt radius theta = fromPolar (radius, theta - pi / 2)

arc : Float -> Float -> Path
arc radius angle =
    let maxN = 20
        thetaAtN n = angle * (n / maxN - 0.5)
    in List.map (thetaAtN >> pointAt radius) [0 .. maxN]

ridgeDiagram : Int -> String -> Float -> Element
ridgeDiagram width name angle = 
    let nameLabel  = toForm <| Text.rightAligned
                            <| Text.bold
                            <| Text.fromString name
        angleLabel = toForm <| showAngle angle 
        diagRoof   = path [pointAt 50 (-0.5 * angle)
                          , (0, 0), pointAt 50 (0.5 * angle) ]
        diagArc    = arc 30 angle
    in  collage (width // 2) (width // 2) <| List.map (scale (toFloat width / 300) << move (0, 40))
            [ move (0, toFloat width / 12) nameLabel
            , traced { defaultLine | width <- 10 } diagRoof
            , traced { defaultLine | width <- 5  } diagArc
           , move (3, negate (toFloat width / 6)) angleLabel]

-- Integration with browser
port initialLocation : String
-- For debugging
port initialLocation = "h=2.2&l=3.55&w=3.95"

defaultValue = 3.5

parseParams : String -> Params
parseParams search =
    let params = String.split "&" search
        parseParam param =
            let [key, stringValue] = case String.split "=" param of
                    [a, b]    -> [a, b]
                    x -> [key, toString defaultValue] 
                value = case (String.toFloat stringValue) of
                    Ok v  -> v
                    Err s -> defaultValue
            in  (key, value)
        dict = Dict.fromList <| List.map parseParam params
        getKey x = case Dict.get x dict of 
            Just v  -> v
            Nothing -> defaultValue
    in { w = getKey "w", l = getKey "l", h = getKey "h" }

paramToString : Params -> String
paramToString {w, l, h} = "w=" ++ toString w ++ "&l=" ++ toString l ++ "&h=" ++ toString h

initialParams : Params
initialParams = parseParams initialLocation

port location : Signal String
port location = paramToString <~ paramSignal
