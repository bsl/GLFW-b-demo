module Cube
  ( draw
  ) where

import qualified Graphics.Rendering.OpenGL as GL

draw :: GL.GLfloat -> IO ()
draw s = do
    GL.scale s s s
    GL.renderPrimitive GL.Quads $ do
        -- front
        GL.color red
        GL.normal (GL.Normal3 z  z  p1)
        GL.vertex (GL.Vertex3 n1 n1 p1)
        GL.vertex (GL.Vertex3 p1 n1 p1)
        GL.vertex (GL.Vertex3 p1 p1 p1)
        GL.vertex (GL.Vertex3 n1 p1 p1)
        -- back
        GL.color red
        GL.normal (GL.Normal3  z  z n1)
        GL.vertex (GL.Vertex3 n1 n1 n1)
        GL.vertex (GL.Vertex3 n1 p1 n1)
        GL.vertex (GL.Vertex3 p1 p1 n1)
        GL.vertex (GL.Vertex3 p1 n1 n1)
        -- top
        GL.color green
        GL.normal (GL.Normal3 z  p1  z)
        GL.vertex (GL.Vertex3 n1 p1 n1)
        GL.vertex (GL.Vertex3 n1 p1 p1)
        GL.vertex (GL.Vertex3 p1 p1 p1)
        GL.vertex (GL.Vertex3 p1 p1 n1)
        -- bottom
        GL.color green
        GL.normal (GL.Normal3  z n1  z)
        GL.vertex (GL.Vertex3 n1 n1 n1)
        GL.vertex (GL.Vertex3 p1 n1 n1)
        GL.vertex (GL.Vertex3 p1 n1 p1)
        GL.vertex (GL.Vertex3 n1 n1 p1)
        -- right
        GL.color blue
        GL.normal (GL.Normal3 p1  z  z)
        GL.vertex (GL.Vertex3 p1 n1 n1)
        GL.vertex (GL.Vertex3 p1 p1 n1)
        GL.vertex (GL.Vertex3 p1 p1 p1)
        GL.vertex (GL.Vertex3 p1 n1 p1)
        -- left
        GL.color blue
        GL.normal (GL.Normal3 n1  z  z)
        GL.vertex (GL.Vertex3 n1 n1 n1)
        GL.vertex (GL.Vertex3 n1 n1 p1)
        GL.vertex (GL.Vertex3 n1 p1 p1)
        GL.vertex (GL.Vertex3 n1 p1 n1)

red, green, blue :: GL.Color4 GL.GLfloat
red   = GL.Color4 1 0 0 1
green = GL.Color4 0 1 0 1
blue  = GL.Color4 0 0 1 1

z, n1, p1 :: GL.GLfloat
z  =  0
n1 = -1
p1 =  1
