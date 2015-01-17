module ThreeDView where

import Graphics.Element (..)
import Math.Vector3 (..)
import Math.Vector3
import Math.Vector2 (Vec2, vec2)
import Math.Matrix4 (mul, makePerspective, makeLookAt, Mat4)
import Signal
import Time (..)
import WebGL (..)
import Debug (watch)
import Basics
import Dict

type alias Params = { h: Float, l: Float, w: Float }
type alias Vertex = { pos: Vec3,  coord: Vec2 }

make3DView : (String -> Texture) -> (Int, Int) -> Params -> Element
make3DView getTexture (w, h) params =
    let persp = perspective <| Basics.max (Basics.max (params.h * 1.4) (params.l)) (params.w)
        makeRoof mesh = entity textureVShader textureFShader mesh
                          { texture = getTexture "roof", perspective = persp}
        makeWall mesh = entity textureVShader textureFShader mesh
                          { texture = getTexture "wall", perspective = persp}
        makeRuler mesh = entity textureVShader textureFShader mesh
                          { texture = getTexture "ruler", perspective = persp}
        makeRulerLabel mesh tex = entity textureVShader textureFShader mesh
                          { texture = tex, perspective = persp }
                    
    in webgl (w, h) [ makeRoof (eastRoof params)
                  , makeRoof (northRoof params)
                  , makeRoof (southRoof params)
                  , makeWall (southWall params)
                  , makeWall (eastWall params)
                  , makeWall (northWall params)
                  , makeRulerLabel (riseRulerLabel params) (getTexture "rise")
                  , makeRulerLabel (endRunRulerLabel params) (getTexture "end-run")
                  , makeRulerLabel (sideRunRulerLabel params) (getTexture "side-run")
                  , makeRuler (riseRuler params)
                  , makeRuler (endRunRuler params)
                  , makeRuler (sideRunRuler params)
                  ]

roofLength = 5
wallHeight = 2.5

summit p   = vec3 0 0 p.h
neCorner p = vec3 p.l p.w 0
seCorner p = vec3 p.l -p.w 0

wSummit  p = vec3 -roofLength 0 p.h
nwCorner p = vec3 -roofLength p.w 0
swCorner p = vec3 -roofLength -p.w 0

eastMidPoint p = vec3 p.l 0 0
midAir p = vec3 p.l 0 p.h

neBase p = vec3 p.l p.w -wallHeight 
seBase p = vec3 p.l -p.w -wallHeight
nwBase p = vec3 -roofLength p.w -wallHeight
swBase p = vec3 -roofLength -p.w -wallHeight

eastRoof : Params -> List (Triangle Vertex)
eastRoof p = triangleRoof (summit p) (seCorner p) (neCorner p)

southRoof : Params -> List (Triangle Vertex)
southRoof p = trapeziumRoof (wSummit p) (summit p) (swCorner p) (seCorner p)

northRoof : Params -> List (Triangle Vertex)
northRoof p =  trapeziumRoof (summit p) (wSummit p) (neCorner p) (nwCorner p)

southWall p = trapeziumRoof (swCorner p) (seCorner p) (swBase p) (seBase p)
eastWall p = trapeziumRoof (seCorner p) (neCorner p) (seBase p) (neBase p)
northWall p = trapeziumRoof (neCorner p) (nwCorner p) (neBase p) (nwBase p)

triangleRoof : Vec3 -> Vec3 -> Vec3 ->  List (Triangle Vertex)
triangleRoof top left right =
    let coord v =
            let v3 = sub v left
                xc = dot alongUnit v3
                yc = length <| sub v3 (scale xc alongUnit)
             in vec2 xc yc
        along = sub right left
        alongUnit = scale (1 / length along) along
    in  [( { pos = top,   coord = coord top }
         , { pos = left,  coord = coord left }
         , { pos = right, coord = coord right }
        )]

trapeziumRoof : Vec3 -> Vec3 -> Vec3 -> Vec3 -> List (Triangle Vertex)
trapeziumRoof topLeft topRight botLeft botRight =
    let coord v =
            let v3 = sub v botLeft
                xc = dot alongUnit v3
                yc = length <| sub v3 (scale xc alongUnit)
             in vec2 xc yc
        along = sub botRight botLeft
        alongUnit = scale (1 / length along) along
    in  [ ( { pos = topLeft,  coord = coord topLeft }
          , { pos = topRight, coord = coord topRight }
          , { pos = botRight, coord = coord botRight }
          )
        , ( { pos = topLeft,  coord = coord topLeft }
          , { pos = botRight, coord = coord botRight }
          , { pos = botLeft,  coord = coord botLeft }
          )
        ]

ruler : Vec3 -> Vec3 -> Vec3 -> List (Triangle Vertex)
ruler start end up = 
    let a = { pos = add start up, coord = vec2 0 1 }
        b = { pos = add end up, coord = vec2 n 1 }
        c = { pos = start, coord = vec2 0 0 }
        d = { pos = end, coord = vec2 n 0 }
        n = length (sub end start) / length up / 4
     in [(a, b, c), (b, c, d)]

rulerLabel : Vec3 -> Vec3 -> Vec3 -> List (Triangle Vertex)
rulerLabel start end up =
    let pos = add (scale 0.2 up) <| add out <| scale 0.5 (add start end)
        across = scale (length up * 2) (direction start end)
        out = scale 0.001 <| cross up across -- Move out to make above ruler
        a = { pos = add c.pos up, coord = vec2 1 1 }
        b = { pos = add d.pos up, coord = vec2 0 1 }
        c = { pos = sub pos across, coord = vec2 1 0 }
        d = { pos = add pos across, coord = vec2 0 0 }
     in [ (a, b, c), (b, c, d) ]

rulerHeight = 1
labelHeight = rulerHeight / 1.9

riseRuler p    = ruler (eastMidPoint p) (midAir p) (vec3 rulerHeight 0 0)
endRunRuler p  = ruler (summit p) (midAir p) (vec3 0 0 rulerHeight)
sideRunRuler p = ruler (seCorner p) (eastMidPoint p) (vec3 rulerHeight 0 0)

riseRulerLabel p    = rulerLabel (midAir p) (eastMidPoint p) (vec3 labelHeight 0 0)
sideRunRulerLabel p = rulerLabel (eastMidPoint p) (seCorner p) (vec3 labelHeight 0 0)
endRunRulerLabel p  = rulerLabel (summit p) (midAir p) (vec3 0 0 labelHeight)

perspective : Float -> Mat4
perspective distance =
    let camera = scale distance <| vec3 2 -2 1
        lookAt = vec3 0 0 -1
        up = vec3 0 0 1
    in  mul (makePerspective 45 1 0.01 10000)
            (makeLookAt camera lookAt up)


-- SHADERS --
textureVShader : Shader { attr | pos:Vec3, coord:Vec2 }
                      { unif | perspective:Mat4 }
                      { vcoord:Vec2 }
textureVShader = [glsl|

attribute vec3 pos;
attribute vec2 coord;
uniform mat4 perspective;
varying vec2 vcoord;

void main () {
    gl_Position = perspective * vec4(pos, 1.0);
    vcoord = coord;
}

|]

textureFShader : Shader {} {u | texture:Texture } { vcoord:Vec2 }
textureFShader = [glsl|

precision mediump float;
uniform sampler2D texture;
varying vec2 vcoord;

void main () {
    gl_FragColor = texture2D(texture, vcoord);
}

|]
