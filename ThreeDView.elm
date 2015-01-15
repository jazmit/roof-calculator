module ThreeDView where

import Graphics.Element (..)
import Math.Vector3 (..)
import Math.Matrix4 (..)
import Signal
import Time (..)
import WebGL (..)

type alias Params = { h: Float, l: Float, w: Float }
type alias Vertex = { pos: Vec3,  color: Vec3 }

make3DView : (Int, Int) -> Params -> Element
make3DView dims params =
    let makeEntity mesh = entity vertexShader fragmentShader mesh { perspective = perspective (1) }
    in webgl dims [ makeEntity (northRoof params)
                  , makeEntity (southRoof params)
                  , makeEntity (eastRoof params)]

mesh : List (Triangle Vertex)
mesh =
    [ ( Vertex (vec3 0  0 0) (vec3 0 0 1)
      , Vertex (vec3 1  0 0) (vec3 0 1 1)
      , Vertex (vec3 0  1 0) (vec3 0 1 1)
    --, Vertex (vec3 1  1 0) (vec3 1 0 1)
      )
    ]

summit : Params -> Vec3
summit p   = vec3 0 0 p.h
neCorner p = vec3 p.l p.w 0
seCorner p = vec3 p.l -p.w 0

roofLength = 10

wSummit  p = vec3 -roofLength 0 p.h
nwCorner p = vec3 -roofLength p.w 0
swCorner p = vec3 -roofLength -p.w 0

purple : Vec3 -> Vertex
purple v = { pos = v, color = vec3 1 0 1 }
cyan v   = { pos = v, color = vec3 0 1 1 }
yellow v = { pos = v, color = vec3 1 1 0 }

eastRoof : Params -> List (Triangle Vertex)
eastRoof p = [( purple (summit p), purple (neCorner p), purple (seCorner p) )]

southRoof : Params -> List (Triangle Vertex)
southRoof p =
    let a = cyan (summit p)
        b = cyan (wSummit p)
        c = cyan (swCorner p)
        d = cyan (seCorner p)
    in [(a, b, c), (c, a, d)]

northRoof : Params -> List (Triangle Vertex)
northRoof p = 
    let a = yellow (summit p)
        b = yellow (wSummit p)
        c = yellow (nwCorner p)
        d = yellow (neCorner p)
    in [(a, b, c), (c, a, d)]


perspective : Float -> Mat4
perspective t =
    mul (makePerspective 45 2 0.01 100)
        (makeLookAt (vec3 (4 * cos t) 0 (4 * sin t)) (vec3 0 0 0) (vec3 0 1 0))

texture = loadTexture "roof-texture.jpg"

vertexShader : Shader { attr | pos:Vec3, color:Vec3 }
                      { unif | perspective:Mat4 }
                      { vcolor:Vec3 }
vertexShader = [glsl|

attribute vec3 pos;
attribute vec3 color;
uniform mat4 perspective;
varying vec3 vcolor;

void main () {
    gl_Position = perspective * vec4(pos, 1.0);
    vcolor = color;
}

|]


fragmentShader : Shader {} u { vcolor:Vec3 }
fragmentShader = [glsl|

precision mediump float;
varying vec3 vcolor;

void main () {
    gl_FragColor = vec4(vcolor, 1.0);
}

|]
