module RoofCalculator where

import Graphics.Element (..)
import Graphics.Input.Field (..)
import Graphics.Input (..)
import Signal (..)
import List
import Text (asText)
import Text
import Math.Vector3 (Vec3, vec3, dot, cross, toRecord, length)
import String
import Json.Encode

-- Graphics
import Graphics.Collage (..)
import Color (..)

-- HTML form
import Html
import Html (..)
import Html.Attributes (..)
import Html.Events (..)

main : Signal Html
main = map3 draw (makeInput "W" "10") (makeInput "L" "10") (makeInput "H" "10")

draw : (String, Html) -> (String, Html) -> (String, Html) -> Html
draw (w, wEl) (l, lEl) (h, hEl) = 
    let params = Params (parse w) (parse l) (parse h)
        parse str = case (String.toFloat str) of
            Ok res  -> res
            Err err -> 0
        n = northRoof params
        s = southRoof params
        e = eastRoof  params
        a = ridgeAngle n s
        ridgeDiagrams =
            ridgeDiagram "Ridge angle" (ridgeAngle n s) `beside`
            ridgeDiagram "Hip angle" (ridgeAngle n e)

        bsFormGroup : String -> String -> Html -> Html
        bsFormGroup lbl myId formControl =
            div [class "form-group"] [
                label [class "control-label col-sm-2", for myId] [ text lbl ],
                div [class "col-sm-10"] [ formControl ]
            ]
    in div [class "container"] [
            Html.form [class "form-horizontal"] [
                bsFormGroup "Side run:" "W" wEl,
                bsFormGroup "End run:" "L" lEl,
                bsFormGroup "Rise:" "H" hEl
            ],
            fromElement ridgeDiagrams
        ]

makeInput : String -> String -> Signal (String, Html)
makeInput myId defaultValue =
    let updateChannel = channel defaultValue
        valueToHtml v = 
            input [type' "text"
                  , class "form-control"
                  , id myId
                  , placeholder ""
                  , value v
                  , on "input" targetValue (send updateChannel)] []
    in  (\v -> (v, valueToHtml v)) <~ subscribe updateChannel


-- Calculation functions
-- There are north, east and south - facing roof sections
-- The x-axis points east, y north and z up
type alias Params = {
    w : Float,
    l : Float,
    h : Float
}

northRoof : Params -> Vec3
northRoof p = vec3 0 p.h p.w

southRoof : Params -> Vec3
southRoof p = vec3 0 (-p.h) p.w

eastRoof : Params -> Vec3
eastRoof p = vec3 p.h 0 p.l

ridgeAngle : Vec3 -> Vec3 -> Float
ridgeAngle r1 r2 = pi - acos (dot r1 r2 / length r1 / length r2)

-- Drawing functions
showAngle : Float -> Element
showAngle angle = Text.plainText
    <| toString (round <| angle / degrees 1) ++ "°"

pointAt : Float -> Float -> (Float, Float)
pointAt radius theta = fromPolar (radius, theta - pi / 2)

arc : Float -> Float -> Path
arc radius angle =
    let maxN = 20
        thetaAtN n = angle * (n / maxN - 0.5)
    in List.map (thetaAtN >> pointAt radius) [0 .. maxN]

ridgeDiagram : String -> Float -> Element
ridgeDiagram name angle = 
    let nameLabel  = toForm <| Text.rightAligned
                            <| Text.bold
                            <| Text.fromString name
        angleLabel = toForm <| showAngle angle 
        diagRoof   = path [pointAt 50 (-0.5 * angle)
                          , (0, 0), pointAt 50 (0.5 * angle) ]
        diagArc    = arc 30 angle
    in  collage 150 150 <| List.map (move (0, 40))
            [ move (0, 20) nameLabel
            , traced { defaultLine | width <- 10 } diagRoof
            , traced { defaultLine | width <- 5  } diagArc
            , move (3, -50) angleLabel]
