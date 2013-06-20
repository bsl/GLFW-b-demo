module Main (main) where

import Control.Concurrent.STM   (TChan, atomically, newTChanIO, tryReadTChan, writeTChan)
import Control.Monad            (unless, when, void)
import Control.Monad.RWS.Strict (RWST, ask, asks, evalRWST, get, liftIO, modify)
import Data.Maybe               (catMaybes)

import qualified Graphics.Rendering.OpenGL as GL

import qualified Graphics.UI.GLFW as GLFW

import Gear (makeGear)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Env = Env
    { envEventsChan :: TChan Event
    , envWindow     :: !GLFW.Window
    , envGear1      :: !GL.DisplayList
    , envGear2      :: !GL.DisplayList
    , envGear3      :: !GL.DisplayList
    }

data State = State
    { stateWindowWidth      :: !Int
    , stateWindowHeight     :: !Int
    , stateIsCursorInWindow :: !Bool
    , stateViewXAngle       :: !Float
    , stateViewYAngle       :: !Float
    , stateViewZAngle       :: !Float
    , stateGearAngle        :: !Float
    , stateZDistance        :: !Float
    }

type Demo = RWST Env () State IO

data Event =
    EventError           !GLFW.Error !String
  | EventWindowPos       !GLFW.Window !Int !Int
  | EventWindowSize      !GLFW.Window !Int !Int
  | EventWindowClose     !GLFW.Window
  | EventWindowRefresh   !GLFW.Window
  | EventWindowFocus     !GLFW.Window !GLFW.FocusAction
  | EventWindowIconify   !GLFW.Window !GLFW.IconifyAction
  | EventFramebufferSize !GLFW.Window !Int !Int
  | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonAction !GLFW.ModifierKeys
  | EventCursorPos       !GLFW.Window !Double !Double
  | EventCursorEnter     !GLFW.Window !GLFW.CursorAction
  | EventScroll          !GLFW.Window !Double !Double
  | EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyAction !GLFW.ModifierKeys
  | EventChar            !GLFW.Window !Char
  deriving Show

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

main :: IO ()
main = do
    let width  = 640
        height = 480

    eventsChan <- newTChanIO :: IO (TChan Event)

    GLFW.setErrorCallback $ Just $ errorCallback eventsChan

    True       <- GLFW.initialize
    (Just win) <- GLFW.createWindow width height "GLFW-b-demo" Nothing Nothing

    GLFW.setWindowPosCallback       win $ Just $ windowPosCallback       eventsChan
    GLFW.setWindowSizeCallback      win $ Just $ windowSizeCallback      eventsChan
    GLFW.setWindowCloseCallback     win $ Just $ windowCloseCallback     eventsChan
    GLFW.setWindowRefreshCallback   win $ Just $ windowRefreshCallback   eventsChan
    GLFW.setWindowFocusCallback     win $ Just $ windowFocusCallback     eventsChan
    GLFW.setWindowIconifyCallback   win $ Just $ windowIconifyCallback   eventsChan
    GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback eventsChan
    GLFW.setMouseButtonCallback     win $ Just $ mouseButtonCallback     eventsChan
    GLFW.setCursorPosCallback       win $ Just $ cursorPosCallback       eventsChan
    GLFW.setCursorEnterCallback     win $ Just $ cursorEnterCallback     eventsChan
    GLFW.setScrollCallback          win $ Just $ scrollCallback          eventsChan
    GLFW.setKeyCallback             win $ Just $ keyCallback             eventsChan
    GLFW.setCharCallback            win $ Just $ charCallback            eventsChan

    GLFW.makeContextCurrent win
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
          { envEventsChan = eventsChan
          , envWindow     = win
          , envGear1      = gear1
          , envGear2      = gear2
          , envGear3      = gear3
          }
        state = State
          { stateWindowWidth      = width
          , stateWindowHeight     = height
          , stateIsCursorInWindow = False
          , stateViewXAngle       = 0
          , stateViewYAngle       = 0
          , stateViewZAngle       = 0
          , stateGearAngle        = 0
          , stateZDistance        = -20
          }

    run env state

    GLFW.destroyWindow win
    GLFW.terminate
    putStrLn "ended!"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

errorCallback           :: TChan Event -> GLFW.Error -> String                                                             -> IO ()
windowPosCallback       :: TChan Event -> GLFW.Window -> Int -> Int                                                        -> IO ()
windowSizeCallback      :: TChan Event -> GLFW.Window -> Int -> Int                                                        -> IO ()
windowCloseCallback     :: TChan Event -> GLFW.Window                                                                      -> IO ()
windowRefreshCallback   :: TChan Event -> GLFW.Window                                                                      -> IO ()
windowFocusCallback     :: TChan Event -> GLFW.Window -> GLFW.FocusAction                                                  -> IO ()
windowIconifyCallback   :: TChan Event -> GLFW.Window -> GLFW.IconifyAction                                                -> IO ()
framebufferSizeCallback :: TChan Event -> GLFW.Window -> Int -> Int                                                        -> IO ()
mouseButtonCallback     :: TChan Event -> GLFW.Window -> GLFW.MouseButton   -> GLFW.MouseButtonAction -> GLFW.ModifierKeys -> IO ()
cursorPosCallback       :: TChan Event -> GLFW.Window -> Double -> Double                                                  -> IO ()
cursorEnterCallback     :: TChan Event -> GLFW.Window -> GLFW.CursorAction                                                 -> IO ()
scrollCallback          :: TChan Event -> GLFW.Window -> Double -> Double                                                  -> IO ()
keyCallback             :: TChan Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyAction -> GLFW.ModifierKeys            -> IO ()
charCallback            :: TChan Event -> GLFW.Window -> Char                                                              -> IO ()

errorCallback           tc e s            = atomically $ writeTChan tc $ EventError           e s
windowPosCallback       tc win x y        = atomically $ writeTChan tc $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = atomically $ writeTChan tc $ EventWindowSize      win w h
windowCloseCallback     tc win            = atomically $ writeTChan tc $ EventWindowClose     win
windowRefreshCallback   tc win            = atomically $ writeTChan tc $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = atomically $ writeTChan tc $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = atomically $ writeTChan tc $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = atomically $ writeTChan tc $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = atomically $ writeTChan tc $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = atomically $ writeTChan tc $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = atomically $ writeTChan tc $ EventCursorEnter     win ca
scrollCallback          tc win x y        = atomically $ writeTChan tc $ EventScroll          win x y
keyCallback             tc win k sc ka mk = atomically $ writeTChan tc $ EventKey             win k sc ka mk
charCallback            tc win c          = atomically $ writeTChan tc $ EventChar            win c

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

run :: Env -> State -> IO ()
run env state =
    void $ evalRWST runDemo env state

runDemo :: Demo ()
runDemo = do
    win <- asks envWindow

    draw
    liftIO $ do
        GLFW.swapBuffers win
        GLFW.pollEvents
    processEvents

    state <- get

    (kxrot, kyrot) <- liftIO $ getCursorKeyDirections win
    (jxrot, jyrot) <- liftIO $ getJoystickDirections GLFW.Joystick1
    (mxrot, myrot) <- if stateIsCursorInWindow state
                        then let w = stateWindowWidth  state
                                 h = stateWindowHeight state
                             in liftIO $ getMouseDirections win w h
                        else return (0, 0)
    mt <- liftIO GLFW.getTime

    let xa = stateViewXAngle state
        ya = stateViewYAngle state
        xa' = xa + kxrot + jxrot + mxrot
        ya' = ya + kyrot + jyrot + myrot
        ga' = maybe 0 (realToFrac . (100*)) mt

    modify $ \s -> s
      { stateViewXAngle = xa'
      , stateViewYAngle = ya'
      , stateGearAngle  = ga'
      }

    q <- liftIO $ GLFW.windowShouldClose win
    unless q runDemo

processEvents :: Demo ()
processEvents = do
    tc <- asks envEventsChan
    me <- liftIO $ atomically $ tryReadTChan tc
    case me of
      (Just e) -> do
          processEvent e
          processEvents
      Nothing -> return ()

processEvent :: Event -> Demo ()
processEvent ev =
    case ev of
      (EventError e s) -> do
          printEvent "error" [show e, show s]
          win <- asks envWindow
          liftIO $ GLFW.setWindowShouldClose win True

      (EventWindowPos _ x y) ->
          printEvent "window pos" [show x, show y]

      (EventWindowSize _ width height) -> do
          printEvent "window size" [show width, show height]
          modify $ \s -> s
            { stateWindowWidth  = width
            , stateWindowHeight = height
            }
          adjustWindow

      (EventWindowClose _) ->
          printEvent "window close" []

      (EventWindowRefresh _) ->
          printEvent "window refresh" []

      (EventWindowFocus _ fa) ->
          printEvent "window focus" [show fa]

      (EventWindowIconify _ ia) ->
          printEvent "window iconify" [show ia]

      (EventFramebufferSize _ w h) ->
          printEvent "framebuffer size" [show w, show h]

      (EventMouseButton _ mb mba mk) ->
          printEvent "mouse button" [show mb, show mba, showModifierKeys mk]

      (EventCursorPos _ x y) -> do
          let x' = round x :: Int
              y' = round y :: Int
          printEvent "cursor pos" [show x', show y']

      (EventCursorEnter _ ca) -> do
          printEvent "cursor enter" [show ca]
          modify $ \s -> s
            { stateIsCursorInWindow = ca == GLFW.CursorEnter
            }

      (EventScroll _ x y) -> do
          let x' = round x :: Int
              y' = round y :: Int
          printEvent "scroll" [show x', show y']
          modify $ \s -> s
            { stateZDistance = stateZDistance s + realToFrac y
            }
          adjustWindow

      (EventKey win k scancode ka mk) -> do
          printEvent "key" [show k, show scancode, show ka, showModifierKeys mk]
          when ((k == GLFW.KeyQ || k == GLFW.KeyEscape) && ka == GLFW.KeyPress) $
              liftIO $ GLFW.setWindowShouldClose win True

      (EventChar _ c) ->
          printEvent "char" [show c]

adjustWindow :: Demo ()
adjustWindow = do
    state <- get
    let width  = stateWindowWidth  state
        height = stateWindowHeight state
        zdist  = stateZDistance    state
        pos    = GL.Position 0 0
        size   = GL.Size (fromIntegral width) (fromIntegral height)
        h      = fromIntegral height / fromIntegral width :: Float
        znear  = 5           :: Float
        zfar   = 30          :: Float
        xmax   = znear * 0.5 :: Float
    liftIO $ do
        GL.viewport   GL.$= (pos, size)
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
        GL.translate (GL.Vector3 0 0 (realToFrac zdist) :: GL.Vector3 GL.GLfloat)

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
getCursorKeyDirections win = do
    x0 <- isPress `fmap` GLFW.getKey win GLFW.KeyUp
    x1 <- isPress `fmap` GLFW.getKey win GLFW.KeyDown
    y0 <- isPress `fmap` GLFW.getKey win GLFW.KeyLeft
    y1 <- isPress `fmap` GLFW.getKey win GLFW.KeyRight
    let x0n = if x0 then   1  else 0
        x1n = if x1 then (-1) else 0
        y0n = if y0 then   1  else 0
        y1n = if y1 then (-1) else 0
    return (x0n + x1n, y0n + y1n)

getJoystickDirections :: GLFW.Joystick -> IO (Float, Float)
getJoystickDirections js = do
    maxes <- GLFW.getJoystickAxes js
    return $ case maxes of
      (Just (x:y:_)) -> (y, x)
      _              -> (0, 0)

getMouseDirections :: GLFW.Window -> Int -> Int -> IO (Float, Float)
getMouseDirections win w h = do
    (x, y) <- GLFW.getCursorPos win
    let wd2 = realToFrac w / 2
        hd2 = realToFrac h / 2
        yrot = (x - wd2) / wd2
        xrot = (hd2 - y) / hd2
    return (realToFrac xrot, realToFrac yrot)

isPress :: GLFW.KeyAction -> Bool
isPress GLFW.KeyPress  = True
isPress GLFW.KeyRepeat = True
isPress _              = False

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

printEvent :: String -> [String] -> Demo ()
printEvent cbname fields =
    liftIO $ putStrLn $ cbname ++ ": " ++ unwords fields

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
