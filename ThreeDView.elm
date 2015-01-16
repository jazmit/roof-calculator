module ThreeDView where

import Graphics.Element (..)
import Math.Vector3 (..)
import Math.Vector2 (..)
import Math.Matrix4 (..)
import Signal
import Time (..)
import WebGL (..)

type alias Params = { h: Float, l: Float, w: Float }
type alias Vertex = { pos: Vec3,  coord: Vec2 }

make3DView : Texture -> Texture -> (Int, Int) -> Params -> Element
make3DView roofTexture wallTexture dims params =
    let makeRoof mesh = entity vertexShader fragmentShader mesh
                          { roof = roofTexture, perspective = perspective }
        makeWall mesh = entity vertexShader fragmentShader mesh
                          { roof = wallTexture, perspective = perspective }
    in webgl dims [ makeRoof (eastRoof params)
                  , makeRoof (northRoof params)
                  , makeRoof (southRoof params)
                  , makeWall (southWall params)
                  , makeWall (eastWall params)
                  , makeWall (northWall params)]


roofLength = 10
wallHeight = 2

summit p   = vec3 0 0 p.h
neCorner p = vec3 p.l p.w 0
seCorner p = vec3 p.l -p.w 0

wSummit  p = vec3 -roofLength 0 p.h
nwCorner p = vec3 -roofLength p.w 0
swCorner p = vec3 -roofLength -p.w 0

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
       [( { pos = top,   coord = vec2 1 0 }
        , { pos = left,  coord = vec2 0 2 }
        , { pos = right, coord = vec2 2 2 }
       )]

trapeziumRoof : Vec3 -> Vec3 -> Vec3 -> Vec3 -> List (Triangle Vertex)
trapeziumRoof topLeft topRight botLeft botRight =
    [ ( { pos = topLeft,  coord = vec2 0 0 }
      , { pos = topRight, coord = vec2 2 0 }
      , { pos = botRight, coord = vec2 2 2 }
      )
    , ( { pos = topLeft,  coord = vec2 0 0 }
      , { pos = botRight, coord = vec2 2 2 }
      , { pos = botLeft,  coord = vec2 0 2 }
      )
    ]

perspective : Mat4
perspective =
    let camera = vec3 2 -3 2
        lookAt = vec3 0 0 0
        up = vec3 0 0 1
    in  mul (makePerspective 45 2 0.01 100)
            (makeLookAt camera lookAt up)

vertexShader : Shader { attr | pos:Vec3, coord:Vec2 }
                      { unif | perspective:Mat4 }
                      { vcoord:Vec2 }
vertexShader = [glsl|

attribute vec3 pos;
attribute vec2 coord;
uniform mat4 perspective;
varying vec2 vcoord;

void main () {
    gl_Position = perspective * vec4(pos, 1.0);
    vcoord = coord;
}

|]


fragmentShader : Shader {} {u | roof:Texture } { vcoord:Vec2 }
fragmentShader = [glsl|

precision mediump float;
uniform sampler2D roof;
varying vec2 vcoord;

void main () {
    gl_FragColor = texture2D(roof, vcoord);
}

|]
