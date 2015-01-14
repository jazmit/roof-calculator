
import Graphics.Element (..)
import Graphics.Input.Field (..)
import Graphics.Input (..)
import Signal (..)
import List
import Text (asText)
import Text
import Math.Vector3 (Vec3, vec3, dot, cross, toRecord, length)
import String

import Graphics.Collage (..)
import Color (..)


main : Signal Element
main = map3 draw (makeInput "W") (makeInput "L") (makeInput "H")

draw : (String, Element) -> (String, Element) -> (String, Element) -> Element
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
    in  flow down [wEl, lEl, hEl, ridgeDiagrams]

makeInput : String -> Signal (String, Element)
makeInput name =
    let chan       = channel <| {noContent | string <- "20"}
        signal     = subscribe chan
        inputField = field defaultStyle (send chan) name
        wLabel el  = flow right [Text.leftAligned <| Text.fromString name, el]
     in (\c -> (c.string, wLabel (inputField c))) <~ signal


type alias Params = {
    w : Float,
    l : Float,
    h : Float
}

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
