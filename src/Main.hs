module Main (main) where

import Control.Monad (liftM, unless, when)

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import qualified Display as Display

main :: IO ()
main = do
    configureDisplay
    start
    stop

configureDisplay :: IO ()
configureDisplay = do
    GLFW.initialize

    GLFW.setWindowResizeCallback windowResizeCallback
    GLFW.setWindowCloseCallback  windowCloseCallback

    GLFW.openWindow GLFW.defaultDisplayOptions
      { GLFW.displayOptions_numRedBits   = 5
      , GLFW.displayOptions_numGreenBits = 6
      , GLFW.displayOptions_numBlueBits  = 5
      , GLFW.displayOptions_numDepthBits = 1
      }

    GL.clearColor    GL.$= GL.Color4 0.05 0.05 0.05 1
    GL.depthFunc     GL.$= Just GL.Less
    GL.colorMaterial GL.$= Just (GL.FrontAndBack, GL.AmbientAndDiffuse)
    GL.shadeModel    GL.$= GL.Smooth

    GL.lighting              GL.$= GL.Enabled
    GL.lightModelAmbient     GL.$= GL.Color4 0.2 0.2 0.2 1
    GL.position (GL.Light 0) GL.$= GL.Vertex4 (-10) 10 (-10) 0
    GL.ambient  (GL.Light 0) GL.$= GL.Color4 0.4 0.4 0.4 1
    GL.diffuse  (GL.Light 0) GL.$= GL.Color4 0.8 0.8 0.8 1
    GL.light    (GL.Light 0) GL.$= GL.Enabled

  where
    windowResizeCallback :: Int -> Int -> IO ()
    windowResizeCallback w h = do
        GL.viewport   GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
        GL.matrixMode GL.$= GL.Projection
        GL.loadIdentity
        GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

    windowCloseCallback :: IO Bool
    windowCloseCallback = return True

start :: IO ()
start =
    loop 0 0
  where
    loop xa ya = do
        Display.draw xa ya
        GLFW.resetTime

        q <- GLFW.keyboardKeyIsPressed GLFW.KeyboardKeyEsc
        unless q $ do
            (xa', ya') <- adjustAngles
            t <- liftM (numSecondsBetweenFrames -) GLFW.getTime
            when (t > 0) (GLFW.sleep t)
            loop xa' ya'
      where
        adjustAngles :: IO (Float, Float)
        adjustAngles = do
            (lr, du) <- getJoystickDirections
            return (adjust du xa, adjust (negate lr) ya)

        adjust :: Float -> Float -> Float
        adjust factor angle = angle + factor * maxAngle

        maxAngle :: Float
        maxAngle = 1

        numSecondsBetweenFrames :: Double
        numSecondsBetweenFrames = recip (fromIntegral framesPerSecond)

        framesPerSecond :: Int
        framesPerSecond = 200

stop :: IO ()
stop = do
    GLFW.closeWindow
    GLFW.terminate

getJoystickDirections :: IO (Float, Float)
getJoystickDirections = do
    r <- take 2 `fmap` GLFW.getJoystickPosition GLFW.Joystick0 2
    if length r == 2
      then return (r !! 0, r !! 1)
      else return (0, 0)
