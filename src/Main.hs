module Main (main) where

import Control.Monad            (unless, void)
import Control.Monad.RWS.Strict (RWST, ask, evalRWST, get, liftIO, put)
import Data.Maybe               (catMaybes)

import qualified Graphics.Rendering.OpenGL as GL

import qualified Graphics.UI.GLFW as GLFW

import Gear (makeGear)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Env = Env
    { envWindow :: !GLFW.Window
    , envGear1  :: !GL.DisplayList
    , envGear2  :: !GL.DisplayList
    , envGear3  :: !GL.DisplayList
    }

data State = State
    { stateViewXAngle :: !Float
    , stateViewYAngle :: !Float
    , stateViewZAngle :: !Float
    , stateGearAngle  :: !Float
    }

type Demo = RWST Env () State IO

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

main :: IO ()
main = do
    GLFW.setErrorCallback (Just errorCallback)

    True     <- GLFW.initialize
    (Just w) <- GLFW.createWindow 640 480 "GLFW-b-demo" Nothing Nothing

    GLFW.setWindowPosCallback       w (Just windowPosCallback)
    GLFW.setWindowSizeCallback      w (Just windowSizeCallback)
    GLFW.setWindowCloseCallback     w (Just windowCloseCallback)
    GLFW.setWindowRefreshCallback   w (Just windowRefreshCallback)
    GLFW.setWindowFocusCallback     w (Just windowFocusCallback)
    GLFW.setWindowIconifyCallback   w (Just windowIconifyCallback)
    GLFW.setFramebufferSizeCallback w (Just framebufferSizeCallback)
    GLFW.setMouseButtonCallback     w (Just mouseButtonCallback)
    GLFW.setCursorPosCallback       w (Just cursorPosCallback)
    GLFW.setCursorEnterCallback     w (Just cursorEnterCallback)
    GLFW.setScrollCallback          w (Just scrollCallback)
    GLFW.setKeyCallback             w (Just keyCallback)
    GLFW.setCharCallback            w (Just charCallback)

    GLFW.makeContextCurrent w
    -- GLFW.swapInterval 1

    GL.position (GL.Light 0) GL.$= GL.Vertex4 5 5 10 0
    GL.light    (GL.Light 0) GL.$= GL.Enabled
    GL.lighting   GL.$= GL.Enabled
    GL.cullFace   GL.$= Just GL.Back
    GL.depthFunc  GL.$= Just GL.Less
    GL.clearColor GL.$= GL.Color4 0.05 0.05 0.05 1
    GL.normalize  GL.$= GL.Enabled

    gear1 <- makeGear 1   4 1   20 0.7 (GL.Color4 0.8 0.1 0   1)  -- red
    gear2 <- makeGear 0.5 2 2   10 0.7 (GL.Color4 0   0.8 0.2 1)  -- green
    gear3 <- makeGear 1.3 2 0.5 10 0.7 (GL.Color4 0.2 0.2 1   1)  -- blue

    let env = Env
          { envGear1  = gear1
          , envGear2  = gear2
          , envGear3  = gear3
          , envWindow = w
          }
        state = State
          { stateViewXAngle = 0
          , stateViewYAngle = 0
          , stateViewZAngle = 0
          , stateGearAngle  = 0
          }

    run env state

    GLFW.destroyWindow w
    GLFW.terminate

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

errorCallback :: GLFW.Error -> String -> IO ()
errorCallback e s =
    printFields "error" [show e, show s]

windowPosCallback :: GLFW.Window -> Int -> Int -> IO ()
windowPosCallback _ x y =
    printFields "windowPos" [show x, show y]

windowSizeCallback :: GLFW.Window -> Int -> Int -> IO ()
windowSizeCallback _ width height = do
    printFields "windowSize" [show width, show height]
    let pos  = GL.Position 0 0
        size = GL.Size (fromIntegral width) (fromIntegral height)
        h     = fromIntegral height / fromIntegral width :: Float
        znear = 5           :: Float
        zfar  = 30          :: Float
        xmax  = znear * 0.5 :: Float
    GL.viewport GL.$= (pos, size)
    GL.matrixMode GL.$= GL.Projection
    GL.loadIdentity
    GL.frustum (realToFrac $ -xmax)
               (realToFrac    xmax)
               (realToFrac $ -xmax * realToFrac h)
               (realToFrac $  xmax * realToFrac h)
               (realToFrac    znear)
               (realToFrac    zfar)
    GL.matrixMode GL.$= GL.Modelview 0
    GL.loadIdentity
    GL.translate (GL.Vector3 0 0 (-20) :: GL.Vector3 GL.GLfloat)

windowCloseCallback :: GLFW.Window -> IO ()
windowCloseCallback _ =
    printFields "windowClose" []

windowRefreshCallback :: GLFW.Window -> IO ()
windowRefreshCallback _ =
    printFields "windowRefresh" []

windowFocusCallback :: GLFW.Window -> GLFW.FocusAction -> IO ()
windowFocusCallback _ fa =
    printFields "windowFocus" [show fa]

windowIconifyCallback :: GLFW.Window -> GLFW.IconifyAction -> IO ()
windowIconifyCallback _ ia =
    printFields "windowIconify" [show ia]

framebufferSizeCallback :: GLFW.Window -> Int -> Int -> IO ()
framebufferSizeCallback _ width height =
    printFields "framebufferSize" [show width, show height]

mouseButtonCallback :: GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonAction -> GLFW.ModifierKeys -> IO ()
mouseButtonCallback _ mb mba mk =
    printFields "mouseButton" [show mb, show mba, showModifierKeys mk]

cursorPosCallback :: GLFW.Window -> Double -> Double -> IO ()
cursorPosCallback _ x y =
    printFields "cursorPos" [show x, show y]

cursorEnterCallback :: GLFW.Window -> GLFW.CursorAction -> IO ()
cursorEnterCallback _ ca =
    printFields "cursorEnter" [show ca]

scrollCallback :: GLFW.Window -> Double -> Double -> IO ()
scrollCallback _ x y =
    printFields "scroll" [show x, show y]

keyCallback :: GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyAction -> GLFW.ModifierKeys -> IO ()
keyCallback _ k sc ka mk =
    printFields "key" [show k, show sc, show ka, showModifierKeys mk]

charCallback :: GLFW.Window -> Char -> IO ()
charCallback _ c =
    printFields "char" [show c]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

printFields :: String -> [String] -> IO ()
printFields cbname fields = putStrLn $ cbname ++ ": " ++ unwords fields

showModifierKeys :: GLFW.ModifierKeys -> String
showModifierKeys mk =
    "[mod keys: " ++ keys ++ "]"
  where
    keys = if null xs then "none" else unwords xs
    xs = catMaybes ys
    ys = [ if GLFW.modifierKeysShift   mk then Just "shift"   else Nothing
         , if GLFW.modifierKeysControl mk then Just "control" else Nothing
         , if GLFW.modifierKeysAlt     mk then Just "alt"     else Nothing
         , if GLFW.modifierKeysSuper   mk then Just "super"   else Nothing
         ]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

run :: Env -> State -> IO ()
run env state =
    void $ evalRWST runDemo env state

runDemo :: Demo ()
runDemo = do
    draw
    env   <- ask
    state <- get
    let w = envWindow env
    liftIO $ do
        GLFW.swapBuffers w
        GLFW.pollEvents
    q0 <- liftIO $ GLFW.getKey w GLFW.KeyEscape
    q1 <- liftIO $ GLFW.getKey w GLFW.KeyQ
    unless (isPress q0 || isPress q1) $ do
        (kxrot, kyrot) <- liftIO $ getCursorKeyDirections w
        (jxrot, jyrot) <- liftIO $ getJoystickDirections GLFW.Joystick1
        mt             <- liftIO GLFW.getTime
        let xa = stateViewXAngle state
            ya = stateViewYAngle state
            xa' = xa + kxrot + jxrot
            ya' = ya + kyrot + jyrot
            ga' = maybe 0 (realToFrac . (100*)) mt
        put state
            { stateViewXAngle = xa'
            , stateViewYAngle = ya'
            , stateGearAngle  = ga'
            }
        runDemo

draw :: Demo ()
draw = do
    env   <- ask
    state <- get
    let gear1 = envGear1 env
        gear2 = envGear2 env
        gear3 = envGear3 env
        xa = stateViewXAngle state
        ya = stateViewYAngle state
        za = stateViewZAngle state
        ga = stateGearAngle  state
    liftIO $ do
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.preservingMatrix $ do
            GL.rotate (realToFrac xa) xunit
            GL.rotate (realToFrac ya) yunit
            GL.rotate (realToFrac za) zunit
            GL.preservingMatrix $ do
                GL.translate gear1vec
                GL.rotate (realToFrac ga) zunit
                GL.callList gear1
            GL.preservingMatrix $ do
                GL.translate gear2vec
                GL.rotate (-2 * realToFrac ga - 9) zunit
                GL.callList gear2
            GL.preservingMatrix $ do
                GL.translate gear3vec
                GL.rotate (-2 * realToFrac ga - 25) zunit
                GL.callList gear3
      where
        gear1vec = GL.Vector3 (-3)   (-2)  0 :: GL.Vector3 GL.GLfloat
        gear2vec = GL.Vector3   3.1  (-2)  0 :: GL.Vector3 GL.GLfloat
        gear3vec = GL.Vector3 (-3.1)   4.2 0 :: GL.Vector3 GL.GLfloat
        xunit = GL.Vector3 1 0 0 :: GL.Vector3 GL.GLfloat
        yunit = GL.Vector3 0 1 0 :: GL.Vector3 GL.GLfloat
        zunit = GL.Vector3 0 0 1 :: GL.Vector3 GL.GLfloat

getCursorKeyDirections :: GLFW.Window -> IO (Float, Float)
getCursorKeyDirections w = do
    x0 <- isPress `fmap` GLFW.getKey w GLFW.KeyUp
    x1 <- isPress `fmap` GLFW.getKey w GLFW.KeyDown
    y0 <- isPress `fmap` GLFW.getKey w GLFW.KeyLeft
    y1 <- isPress `fmap` GLFW.getKey w GLFW.KeyRight
    let x0n = if x0 then   1  else 0
        x1n = if x1 then (-1) else 0
        y0n = if y0 then   1  else 0
        y1n = if y1 then (-1) else 0
    return (x0n + x1n, y0n + y1n)

getJoystickDirections :: GLFW.Joystick -> IO (Float, Float)
getJoystickDirections js = do
    maxes <- GLFW.getJoystickAxes js
    return $ case maxes of
      (Just (x:y:_)) -> (y,x)
      _              -> (0,0)

isPress :: GLFW.KeyAction -> Bool
isPress GLFW.KeyPress  = True
isPress GLFW.KeyRepeat = True
isPress _              = False
