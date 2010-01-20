module Main (main) where

import Control.Monad (liftM, unless, when)

import qualified Graphics.Rendering.OpenGL as GL

import qualified Graphics.UI.GLFW as GLFW

import qualified Display as Display

main :: IO ()
main = do
    configureDisplay
    start
    stop

configureDisplay :: IO ()
configureDisplay = do
    _ <- GLFW.initialize

    _ <- GLFW.openWindow GLFW.defaultDisplayOptions
      { GLFW.displayOptions_numRedBits   = 5
      , GLFW.displayOptions_numGreenBits = 6
      , GLFW.displayOptions_numBlueBits  = 5
      , GLFW.displayOptions_numDepthBits = 1
      }

    GLFW.setWindowSizeCallback windowSizeCallback

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

windowSizeCallback :: Int -> Int -> IO ()
windowSizeCallback w h =
    GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))

start :: IO ()
start =
    loop 0 0
  where
    loop xa ya = do
        Display.draw xa ya
        GLFW.resetTime

        q0 <- GLFW.keyIsPressed GLFW.KeyEsc
        q1 <- GLFW.keyIsPressed (GLFW.CharKey 'Q')
        unless (q0 || q1) $ do
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
    l <- toFloat `fmap` GLFW.keyIsPressed GLFW.KeyLeft
    r <- toFloat `fmap` GLFW.keyIsPressed GLFW.KeyRight
    u <- toFloat `fmap` GLFW.keyIsPressed GLFW.KeyUp
    d <- toFloat `fmap` GLFW.keyIsPressed GLFW.KeyDown
    return (-l + r, -u + d)
  where
    toFloat b = if b then 1 else 0
