module Display
  ( draw
  ) where

import qualified Cube as Cube

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

draw :: Float -> Float -> IO ()
draw xa ya = do
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.loadIdentity

    GL.preservingMatrix $ do
        GL.rotate (realToFrac xa) xVector3
        GL.rotate (realToFrac ya) yVector3
        Cube.draw w

    GLFW.swapBuffers
  where
    xVector3 = GL.Vector3 1 0 0 :: GL.Vector3 GL.GLfloat
    yVector3 = GL.Vector3 0 1 0 :: GL.Vector3 GL.GLfloat
    w = 0.5
