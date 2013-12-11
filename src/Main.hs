module Main (main) where

--------------------------------------------------------------------------------

import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Monad             (unless, when, void)
import Control.Monad.RWS.Strict  (RWST, ask, asks, evalRWST, get, liftIO, modify, put)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.List                 (intercalate)
import Data.Maybe                (catMaybes)
import Text.PrettyPrint

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import Gear (makeGear)

--------------------------------------------------------------------------------

data Env = Env
    { envEventsChan    :: TQueue Event
    , envWindow        :: !GLFW.Window
    , envGear1         :: !GL.DisplayList
    , envGear2         :: !GL.DisplayList
    , envGear3         :: !GL.DisplayList
    , envZDistClosest  :: !Double
    , envZDistFarthest :: !Double
    }

data State = State
    { stateWindowWidth     :: !Int
    , stateWindowHeight    :: !Int
    , stateXAngle          :: !Double
    , stateYAngle          :: !Double
    , stateZAngle          :: !Double
    , stateGearZAngle      :: !Double
    , stateZDist           :: !Double
    , stateMouseDown       :: !Bool
    , stateDragging        :: !Bool
    , stateDragStartX      :: !Double
    , stateDragStartY      :: !Double
    , stateDragStartXAngle :: !Double
    , stateDragStartYAngle :: !Double
    }

type Demo = RWST Env () State IO

--------------------------------------------------------------------------------

data Event =
    EventError           !GLFW.Error !String
  | EventWindowPos       !GLFW.Window !Int !Int
  | EventWindowSize      !GLFW.Window !Int !Int
  | EventWindowClose     !GLFW.Window
  | EventWindowRefresh   !GLFW.Window
  | EventWindowFocus     !GLFW.Window !GLFW.FocusState
  | EventWindowIconify   !GLFW.Window !GLFW.IconifyState
  | EventFramebufferSize !GLFW.Window !Int !Int
  | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos       !GLFW.Window !Double !Double
  | EventCursorEnter     !GLFW.Window !GLFW.CursorState
  | EventScroll          !GLFW.Window !Double !Double
  | EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar            !GLFW.Window !Char
  deriving Show

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let width  = 640
        height = 480

    eventsChan <- newTQueueIO :: IO (TQueue Event)

    withWindow width height "GLFW-b-demo" $ \win -> do
        GLFW.setErrorCallback               $ Just $ errorCallback           eventsChan
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

        GLFW.swapInterval 1

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

        (fbWidth, fbHeight) <- GLFW.getFramebufferSize win

        let zDistClosest  = 10
            zDistFarthest = zDistClosest + 20
            zDist         = zDistClosest + ((zDistFarthest - zDistClosest) / 2)
            env = Env
              { envEventsChan    = eventsChan
              , envWindow        = win
              , envGear1         = gear1
              , envGear2         = gear2
              , envGear3         = gear3
              , envZDistClosest  = zDistClosest
              , envZDistFarthest = zDistFarthest
              }
            state = State
              { stateWindowWidth     = fbWidth
              , stateWindowHeight    = fbHeight
              , stateXAngle          = 0
              , stateYAngle          = 0
              , stateZAngle          = 0
              , stateGearZAngle      = 0
              , stateZDist           = zDist
              , stateMouseDown       = False
              , stateDragging        = False
              , stateDragStartX      = 0
              , stateDragStartY      = 0
              , stateDragStartXAngle = 0
              , stateDragStartYAngle = 0
              }
        runDemo env state

    putStrLn "ended!"

--------------------------------------------------------------------------------

-- GLFW-b is made to be very close to the C API, so creating a window is pretty
-- clunky by Haskell standards. A higher-level API would have some function
-- like withWindow.

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

--------------------------------------------------------------------------------

-- Each callback does just one thing: write an appropriate Event to the events
-- TQueue.

errorCallback           :: TQueue Event -> GLFW.Error -> String                                                            -> IO ()
windowPosCallback       :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowSizeCallback      :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowCloseCallback     :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowRefreshCallback   :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowFocusCallback     :: TQueue Event -> GLFW.Window -> GLFW.FocusState                                                  -> IO ()
windowIconifyCallback   :: TQueue Event -> GLFW.Window -> GLFW.IconifyState                                                -> IO ()
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
mouseButtonCallback     :: TQueue Event -> GLFW.Window -> GLFW.MouseButton   -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback       :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
cursorEnterCallback     :: TQueue Event -> GLFW.Window -> GLFW.CursorState                                                 -> IO ()
scrollCallback          :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
keyCallback             :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys            -> IO ()
charCallback            :: TQueue Event -> GLFW.Window -> Char                                                             -> IO ()

errorCallback           tc e s            = atomically $ writeTQueue tc $ EventError           e s
windowPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = atomically $ writeTQueue tc $ EventWindowSize      win w h
windowCloseCallback     tc win            = atomically $ writeTQueue tc $ EventWindowClose     win
windowRefreshCallback   tc win            = atomically $ writeTQueue tc $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = atomically $ writeTQueue tc $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = atomically $ writeTQueue tc $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = atomically $ writeTQueue tc $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = atomically $ writeTQueue tc $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = atomically $ writeTQueue tc $ EventCursorEnter     win ca
scrollCallback          tc win x y        = atomically $ writeTQueue tc $ EventScroll          win x y
keyCallback             tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey             win k sc ka mk
charCallback            tc win c          = atomically $ writeTQueue tc $ EventChar            win c

--------------------------------------------------------------------------------

runDemo :: Env -> State -> IO ()
runDemo env state = do
    printInstructions
    void $ evalRWST (adjustWindow >> run) env state

run :: Demo ()
run = do
    win <- asks envWindow

    draw
    liftIO $ do
        GLFW.swapBuffers win
        GL.flush  -- not necessary, but someone recommended it
        GLFW.pollEvents
    processEvents

    state <- get
    if stateDragging state
      then do
          let sodx  = stateDragStartX      state
              sody  = stateDragStartY      state
              sodxa = stateDragStartXAngle state
              sodya = stateDragStartYAngle state
          (x, y) <- liftIO $ GLFW.getCursorPos win
          let myrot = (x - sodx) / 2
              mxrot = (y - sody) / 2
          put $ state
            { stateXAngle = sodxa + mxrot
            , stateYAngle = sodya + myrot
            }
      else do
          (kxrot, kyrot) <- liftIO $ getCursorKeyDirections win
          (jxrot, jyrot) <- liftIO $ getJoystickDirections GLFW.Joystick'1
          put $ state
            { stateXAngle = stateXAngle state + (2 * kxrot) + (2 * jxrot)
            , stateYAngle = stateYAngle state + (2 * kyrot) + (2 * jyrot)
            }

    mt <- liftIO GLFW.getTime
    modify $ \s -> s
      { stateGearZAngle = maybe 0 (realToFrac . (100*)) mt
      }

    q <- liftIO $ GLFW.windowShouldClose win
    unless q run

processEvents :: Demo ()
processEvents = do
    tc <- asks envEventsChan
    me <- liftIO $ atomically $ tryReadTQueue tc
    case me of
      Just e -> do
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

      (EventWindowSize _ width height) ->
          printEvent "window size" [show width, show height]

      (EventWindowClose _) ->
          printEvent "window close" []

      (EventWindowRefresh _) ->
          printEvent "window refresh" []

      (EventWindowFocus _ fs) ->
          printEvent "window focus" [show fs]

      (EventWindowIconify _ is) ->
          printEvent "window iconify" [show is]

      (EventFramebufferSize _ width height) -> do
          printEvent "framebuffer size" [show width, show height]
          modify $ \s -> s
            { stateWindowWidth  = width
            , stateWindowHeight = height
            }
          adjustWindow

      (EventMouseButton _ mb mbs mk) -> do
          printEvent "mouse button" [show mb, show mbs, showModifierKeys mk]
          when (mb == GLFW.MouseButton'1) $ do
              let pressed = mbs == GLFW.MouseButtonState'Pressed
              modify $ \s -> s
                { stateMouseDown = pressed
                }
              unless pressed $
                modify $ \s -> s
                  { stateDragging = False
                  }

      (EventCursorPos _ x y) -> do
          let x' = round x :: Int
              y' = round y :: Int
          printEvent "cursor pos" [show x', show y']
          state <- get
          when (stateMouseDown state && not (stateDragging state)) $
            put $ state
              { stateDragging        = True
              , stateDragStartX      = x
              , stateDragStartY      = y
              , stateDragStartXAngle = stateXAngle state
              , stateDragStartYAngle = stateYAngle state
              }

      (EventCursorEnter _ cs) ->
          printEvent "cursor enter" [show cs]

      (EventScroll _ x y) -> do
          let x' = round x :: Int
              y' = round y :: Int
          printEvent "scroll" [show x', show y']
          env <- ask
          modify $ \s -> s
            { stateZDist =
                let zDist' = stateZDist s + realToFrac (negate $ y / 2)
                in curb (envZDistClosest env) (envZDistFarthest env) zDist'
            }
          adjustWindow

      (EventKey win k scancode ks mk) -> do
          printEvent "key" [show k, show scancode, show ks, showModifierKeys mk]
          when (ks == GLFW.KeyState'Pressed) $ do
              -- Q, Esc: exit
              when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $
                liftIO $ GLFW.setWindowShouldClose win True
              -- ?: print instructions
              when (k == GLFW.Key'Slash && GLFW.modifierKeysShift mk) $
                liftIO printInstructions
              -- i: print GLFW information
              when (k == GLFW.Key'I) $
                liftIO $ printInformation win

      (EventChar _ c) ->
          printEvent "char" [show c]

adjustWindow :: Demo ()
adjustWindow = do
    state <- get
    let width  = stateWindowWidth  state
        height = stateWindowHeight state
        zDist  = stateZDist        state

    let pos   = GL.Position 0 0
        size  = GL.Size (fromIntegral width) (fromIntegral height)
        h     = fromIntegral height / fromIntegral width :: Double
        znear = 1           :: Double
        zfar  = 40          :: Double
        xmax  = znear * 0.5 :: Double
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
        GL.translate (GL.Vector3 0 0 (negate $ realToFrac zDist) :: GL.Vector3 GL.GLfloat)

draw :: Demo ()
draw = do
    env   <- ask
    state <- get
    let gear1 = envGear1 env
        gear2 = envGear2 env
        gear3 = envGear3 env
        xa = stateXAngle state
        ya = stateYAngle state
        za = stateZAngle state
        ga = stateGearZAngle  state
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

getCursorKeyDirections :: GLFW.Window -> IO (Double, Double)
getCursorKeyDirections win = do
    x0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Up
    x1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Down
    y0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Left
    y1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Right
    let x0n = if x0 then (-1) else 0
        x1n = if x1 then   1  else 0
        y0n = if y0 then (-1) else 0
        y1n = if y1 then   1  else 0
    return (x0n + x1n, y0n + y1n)

getJoystickDirections :: GLFW.Joystick -> IO (Double, Double)
getJoystickDirections js = do
    maxes <- GLFW.getJoystickAxes js
    return $ case maxes of
      (Just (x:y:_)) -> (-y, x)
      _              -> ( 0, 0)

isPress :: GLFW.KeyState -> Bool
isPress GLFW.KeyState'Pressed   = True
isPress GLFW.KeyState'Repeating = True
isPress _                       = False

--------------------------------------------------------------------------------

printInstructions :: IO ()
printInstructions =
    putStrLn $ render $
      nest 4 (
        text "------------------------------------------------------------" $+$
        text "'?': Print these instructions"                                $+$
        text "'i': Print GLFW information"                                  $+$
        text ""                                                             $+$
        text "* Mouse cursor, keyboard cursor keys, and/or joystick"        $+$
        text "  control rotation."                                          $+$
        text "* Mouse scroll wheel controls distance from scene."           $+$
        text "------------------------------------------------------------"
      )

printInformation :: GLFW.Window -> IO ()
printInformation win = do
    version       <- GLFW.getVersion
    versionString <- GLFW.getVersionString
    monitorInfos  <- runMaybeT getMonitorInfos
    joystickNames <- getJoystickNames
    clientAPI     <- GLFW.getWindowClientAPI              win
    cv0           <- GLFW.getWindowContextVersionMajor    win
    cv1           <- GLFW.getWindowContextVersionMinor    win
    cv2           <- GLFW.getWindowContextVersionRevision win
    robustness    <- GLFW.getWindowContextRobustness      win
    forwardCompat <- GLFW.getWindowOpenGLForwardCompat    win
    debug         <- GLFW.getWindowOpenGLDebugContext     win
    profile       <- GLFW.getWindowOpenGLProfile          win

    putStrLn $ render $
      nest 4 (
        text "------------------------------------------------------------" $+$
        text "GLFW C library:" $+$
        nest 4 (
          text "Version:"        <+> renderVersion version $+$
          text "Version string:" <+> renderVersionString versionString
        ) $+$
        text "Monitors:" $+$
        nest 4 (
          renderMonitorInfos monitorInfos
        ) $+$
        text "Joysticks:" $+$
        nest 4 (
          renderJoystickNames joystickNames
        ) $+$
        text "OpenGL context:" $+$
        nest 4 (
          text "Client API:"            <+> renderClientAPI clientAPI $+$
          text "Version:"               <+> renderContextVersion cv0 cv1 cv2 $+$
          text "Robustness:"            <+> renderContextRobustness robustness $+$
          text "Forward compatibility:" <+> renderForwardCompat forwardCompat $+$
          text "Debug:"                 <+> renderDebug debug $+$
          text "Profile:"               <+> renderProfile profile
        ) $+$
        text "------------------------------------------------------------"
      )
  where
    renderVersion (GLFW.Version v0 v1 v2) =
        text $ intercalate "." $ map show [v0, v1, v2]

    renderVersionString =
        text . show

    renderMonitorInfos =
        maybe (text "(error)") (vcat . map renderMonitorInfo)

    renderMonitorInfo (name, (x,y), (w,h), vms) =
        text (show name) $+$
        nest 4 (
          location <+> size $+$
          fsep (map renderVideoMode vms)
        )
      where
        location = int x <> text "," <> int y
        size     = int w <> text "x" <> int h <> text "mm"

    renderVideoMode (GLFW.VideoMode w h r g b rr) =
        brackets $ res <+> rgb <+> hz
      where
        res = int w <> text "x" <> int h
        rgb = int r <> text "x" <> int g <> text "x" <> int b
        hz  = int rr <> text "Hz"

    renderJoystickNames pairs =
        vcat $ map (\(js, name) -> text (show js) <+> text (show name)) pairs

    renderContextVersion v0 v1 v2 =
        hcat [int v0, text ".", int v1, text ".", int v2]

    renderClientAPI         = text . show
    renderContextRobustness = text . show
    renderForwardCompat     = text . show
    renderDebug             = text . show
    renderProfile           = text . show

type MonitorInfo = (String, (Int,Int), (Int,Int), [GLFW.VideoMode])

getMonitorInfos :: MaybeT IO [MonitorInfo]
getMonitorInfos =
    getMonitors >>= mapM getMonitorInfo
  where
    getMonitors :: MaybeT IO [GLFW.Monitor]
    getMonitors = MaybeT GLFW.getMonitors

    getMonitorInfo :: GLFW.Monitor -> MaybeT IO MonitorInfo
    getMonitorInfo mon = do
        name <- getMonitorName mon
        vms  <- getVideoModes mon
        MaybeT $ do
            pos  <- liftIO $ GLFW.getMonitorPos mon
            size <- liftIO $ GLFW.getMonitorPhysicalSize mon
            return $ Just (name, pos, size, vms)

    getMonitorName :: GLFW.Monitor -> MaybeT IO String
    getMonitorName mon = MaybeT $ GLFW.getMonitorName mon

    getVideoModes :: GLFW.Monitor -> MaybeT IO [GLFW.VideoMode]
    getVideoModes mon = MaybeT $ GLFW.getVideoModes mon

getJoystickNames :: IO [(GLFW.Joystick, String)]
getJoystickNames =
    catMaybes `fmap` mapM getJoystick joysticks
  where
    getJoystick js =
        fmap (maybe Nothing (\name -> Just (js, name)))
             (GLFW.getJoystickName js)

--------------------------------------------------------------------------------

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

curb :: Ord a => a -> a -> a -> a
curb l h x
  | x < l     = l
  | x > h     = h
  | otherwise = x

--------------------------------------------------------------------------------

joysticks :: [GLFW.Joystick]
joysticks =
  [ GLFW.Joystick'1
  , GLFW.Joystick'2
  , GLFW.Joystick'3
  , GLFW.Joystick'4
  , GLFW.Joystick'5
  , GLFW.Joystick'6
  , GLFW.Joystick'7
  , GLFW.Joystick'8
  , GLFW.Joystick'9
  , GLFW.Joystick'10
  , GLFW.Joystick'11
  , GLFW.Joystick'12
  , GLFW.Joystick'13
  , GLFW.Joystick'14
  , GLFW.Joystick'15
  , GLFW.Joystick'16
  ]
