
import Graphics.Element (..)
import Graphics.Input.Field (..)
import Graphics.Input (..)
import Signal (..)
import List
import Text (asText)
import Text
import Math.Vector3 (Vec3, vec3, dot, cross, toRecord, length)
import String


main : Signal Element
main = map3 doit (makeInput "W") (makeInput "L") (makeInput "H")

doit : (String, Element) -> (String, Element) -> (String, Element) -> Element
doit (w, wEl) (l, lEl) (h, hEl) = 
    let params = Params (parse w) (parse l) (parse h)
        parse str = case (String.toFloat str) of
            Ok res  -> res
            Err err -> 0
        n = northRoof params
        s = southRoof params
        e = eastRoof  params
        a = ridgeAngle n s
    in  scene [wEl, lEl, hEl,
               angleDisplay "Ridge angle" (ridgeAngle n s),
               angleDisplay "Hip angle" (ridgeAngle n e)]

scene : List Element -> Element
scene elements = flow down elements

angleDisplay : String -> Float -> Element
angleDisplay name angle =
    let s = toString angle
     in asText angle

makeInput : String -> Signal (String, Element)
makeInput name =
    let chan       = channel <| {noContent | string <- "10"}
        signal     = subscribe chan
        inputField = field defaultStyle (send chan) name
        wLabel el  = flow right [Text.leftAligned <| Text.fromString name, el]
     in (\c -> (c.string, wLabel (inputField c))) <~ signal


type alias Params = {
    w : Float,
    l : Float,
    h : Float
}

-- There are north, east and south - facing roof sections
-- The x-axis points east, y north and z up

northRoof : Params -> Vec3
northRoof p = vec3 0 p.h p.w

southRoof : Params -> Vec3
southRoof p = vec3 0 (-p.h) p.w

eastRoof : Params -> Vec3
eastRoof p = vec3 p.h 0 p.l

ridgeAngle : Vec3 -> Vec3 -> Float
ridgeAngle r1 r2 = 180 - acos (dot r1 r2 / length r1 / length r2) * 180 / pi

--ridgeAngle : Int -> Int -> Element
--ridgeAngle content = asText <| content.string
