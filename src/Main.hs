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

    GLFW.openWindow GLFW.defaultDisplayOptions
      { GLFW.displayOptions_numRedBits   = 5
      , GLFW.displayOptions_numGreenBits = 6
      , GLFW.displayOptions_numBlueBits  = 5
      , GLFW.displayOptions_numDepthBits = 1
      }

    GLFW.setWindowResizeCallback windowResizeCallback

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

windowResizeCallback :: Int -> Int -> IO ()
windowResizeCallback w h = do
    putStrLn $ "windowResizeCallback: " ++ show w ++ "x" ++ show h
    GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))

start :: IO ()
start =
    loop 0 0
  where
    loop xa ya = do
        Display.draw xa ya
        GLFW.resetTime

        q <- GLFW.keyboardKeyIsPressed GLFW.KeyboardKeyEsc
        unless q $ do
            (jlr, jud) <- getJoystickDirections
            (klr, kud) <- getCursorKeyDirections

            let xa' = (xa +        jud * maxAngle) - kud
            let ya' = (ya + negate jlr * maxAngle) - klr

            t <- liftM (numSecondsBetweenFrames -) GLFW.getTime
            when (t > 0) (GLFW.sleep t)

            loop xa' ya'
      where
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
    return $
      case r of
        [x, y] -> (x, y)
        _      -> (0, 0)

getCursorKeyDirections :: IO (Float, Float)
getCursorKeyDirections = do
    l <- toFloat `fmap` GLFW.keyboardKeyIsPressed GLFW.KeyboardKeyLeft
    r <- toFloat `fmap` GLFW.keyboardKeyIsPressed GLFW.KeyboardKeyRight
    u <- toFloat `fmap` GLFW.keyboardKeyIsPressed GLFW.KeyboardKeyUp
    d <- toFloat `fmap` GLFW.keyboardKeyIsPressed GLFW.KeyboardKeyDown
    return (-l + r, -u + d)
  where
    toFloat b = if b then 1 else 0
