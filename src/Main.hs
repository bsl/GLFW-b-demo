module Main (main) where

import Control.Monad            (forM_, unless, when, void)
import Control.Monad.RWS.Strict (RWST, ask, evalRWST, get, liftIO, put)

import qualified Graphics.Rendering.OpenGL as GL

import qualified Graphics.UI.GLFW as GLFW

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

    gear1 <- mkGear1
    gear2 <- mkGear2
    gear3 <- mkGear3

    let env = Env
          { envGear1  = gear1
          , envGear2  = gear2
          , envGear3  = gear3
          , envWindow = w
          }
        state = State
          { stateViewXAngle = 45
          , stateViewYAngle =  0
          , stateViewZAngle =  0
          , stateGearAngle  =  0
          }

    run env state

    GLFW.destroyWindow w
    GLFW.terminate

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

errorCallback :: GLFW.Error -> String -> IO ()
errorCallback e s =
    printFields ["error", show e, show s]

windowPosCallback :: GLFW.Window -> Int -> Int -> IO ()
windowPosCallback _ x y =
    printFields ["windowPos", show x, show y]

windowSizeCallback :: GLFW.Window -> Int -> Int -> IO ()
windowSizeCallback _ width height = do
    printFields ["windowSize", show width, show height]
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
               (realToFrac xmax)
               (realToFrac $ -xmax * realToFrac h)
               (realToFrac $ xmax * realToFrac h)
               (realToFrac znear)
               (realToFrac zfar)
    GL.matrixMode GL.$= GL.Modelview 0
    GL.loadIdentity
    GL.translate (GL.Vector3 0 0 (-20) :: GL.Vector3 GL.GLfloat)

windowCloseCallback :: GLFW.Window -> IO ()
windowCloseCallback _ =
    printFields ["windowClose"]

windowRefreshCallback :: GLFW.Window -> IO ()
windowRefreshCallback _ =
    printFields ["windowRefresh"]

windowFocusCallback :: GLFW.Window -> GLFW.FocusAction -> IO ()
windowFocusCallback _ fa =
    printFields ["windowFocus", show fa]

windowIconifyCallback :: GLFW.Window -> GLFW.IconifyAction -> IO ()
windowIconifyCallback _ ia =
    printFields ["windowIconify", show ia]

framebufferSizeCallback :: GLFW.Window -> Int -> Int -> IO ()
framebufferSizeCallback _ width height =
    printFields ["framebufferSize", show width, show height]

mouseButtonCallback :: GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonAction -> GLFW.ModifierKeys -> IO ()
mouseButtonCallback _ mb mba mk =
    printFields ["mouseButton", show mb, show mba, show mk]

cursorPosCallback :: GLFW.Window -> Double -> Double -> IO ()
cursorPosCallback _ x y =
    printFields ["cursorPos", show x, show y]

cursorEnterCallback :: GLFW.Window -> GLFW.CursorAction -> IO ()
cursorEnterCallback _ ca =
    printFields ["cursorEnter", show ca]

scrollCallback :: GLFW.Window -> Double -> Double -> IO ()
scrollCallback _ x y =
    printFields ["scroll", show x, show y]

keyCallback :: GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyAction -> GLFW.ModifierKeys -> IO ()
keyCallback _ k sc ka mk =
    printFields ["key", show k, show sc, show ka, show mk]

charCallback :: GLFW.Window -> Char -> IO ()
charCallback _ c =
    printFields ["char", show c]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

printFields :: [String] -> IO ()
printFields = putStrLn . unwords

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

run :: Env -> State -> IO ()
run env state =
    void $ evalRWST runDemo env state

runDemo :: Demo ()
runDemo = do
    env   <- ask
    state <- get
    let w = envWindow env
    draw
    liftIO $ GLFW.swapBuffers w
    liftIO GLFW.pollEvents
    q0 <- liftIO $ GLFW.getKey w GLFW.KeyEscape
    q1 <- liftIO $ GLFW.getKey w GLFW.KeyQ
    unless (isPress q0 || isPress q1) $ do
        let xa = stateViewXAngle state
            ya = stateViewYAngle state
        (xrot, yrot) <- liftIO $ getCursorKeyDirections w
        mt <- liftIO GLFW.getTime
        let xa' = xa + xrot
            ya' = ya + yrot
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

mkGear1 :: IO GL.DisplayList
mkGear1 =
    GL.defineNewList GL.Compile $ do
        GL.colorMaterial GL.$= Just (GL.Front, GL.AmbientAndDiffuse)
        GL.color red
        makeGear 1 4 1 20 0.7
      where
        red :: GL.Color4 GL.GLfloat
        red = GL.Color4 0.8 0.1 0 1

mkGear2 :: IO GL.DisplayList
mkGear2 =
    GL.defineNewList GL.Compile $ do
        GL.colorMaterial GL.$= Just (GL.Front, GL.AmbientAndDiffuse)
        GL.color green
        makeGear 0.5 2 2 10 0.7
      where
        green :: GL.Color4 GL.GLfloat
        green = GL.Color4 0 0.8 0.2 1

mkGear3 :: IO GL.DisplayList
mkGear3 =
    GL.defineNewList GL.Compile $ do
        GL.colorMaterial GL.$= Just (GL.Front, GL.AmbientAndDiffuse)
        GL.color blue
        makeGear 1.3 2 0.5 10 0.7
      where
        blue :: GL.Color4 GL.GLfloat
        blue = GL.Color4 0.2 0.2 1 1

makeGear :: Float -> Float -> Float -> Int -> Float -> IO ()
makeGear inradius outradius width teeth toothdepth = do
    let r0 = inradius
        r1 = outradius - toothdepth / 2
        r2 = outradius + toothdepth / 2
        da = 2 * pi / realToFrac teeth / 4 :: Float
    GL.shadeModel GL.$= GL.Flat
    GL.normal (GL.Normal3 0 0 1 :: GL.Normal3 GL.GLfloat)

    -- front face
    GL.renderPrimitive GL.QuadStrip $
        forM_ [0 .. teeth] $ \i -> do
            let a = realToFrac i * 2 * pi / realToFrac teeth :: Float
            GL.vertex $ mkVertex (r0 * cos a) (r0 * sin a) (width * 0.5)
            GL.vertex $ mkVertex (r1 * cos a) (r1 * sin a) (width * 0.5)
            when (i < teeth) $ do
                GL.vertex $ mkVertex (r0 * cos a)            (r0 * sin a)            (width * 0.5)
                GL.vertex $ mkVertex (r1 * cos (a + 3 * da)) (r1 * sin (a + 3 * da)) (width * 0.5)

    -- front sides of teeth
    GL.renderPrimitive GL.Quads $
        forM_ [0 .. pred teeth] $ \i -> do
            let a = realToFrac i * 2 * pi / realToFrac teeth :: Float
            GL.vertex $ mkVertex (r1 * cos a) (r1 * sin a) (width * 0.5)
            GL.vertex $ mkVertex (r2 * cos (a + da)) (r2 * sin (a + da)) (width * 0.5)
            GL.vertex $ mkVertex (r2 * cos (a + 2 * da)) (r2 * sin (a + 2 * da)) (width * 0.5)
            GL.vertex $ mkVertex (r1 * cos (a + 3 * da)) (r1 * sin (a + 3 * da)) (width * 0.5)

    GL.normal (GL.Normal3 0 0 (-1) :: GL.Normal3 GL.GLfloat)

    -- back face
    GL.renderPrimitive GL.QuadStrip $
        forM_ [0 .. teeth] $ \i -> do
            let a = realToFrac i * 2 * pi / realToFrac teeth :: Float
            GL.vertex $ mkVertex (r1 * cos a) (r1 * sin a) (negate width * 0.5)
            GL.vertex $ mkVertex (r0 * cos a) (r0 * sin a) (negate width * 0.5)
            when (i < teeth) $ do
                GL.vertex $ mkVertex (r1 * cos (a + 3 * da)) (r1 * sin (a + 3 * da)) (negate width * 0.5)
                GL.vertex $ mkVertex (r0 * cos a) (r0 * sin a) (negate width * 0.5)

    -- back sides of teeth
    GL.renderPrimitive GL.Quads $
        forM_ [0 .. pred teeth] $ \i -> do
            let a = realToFrac i * 2 * pi / realToFrac teeth :: Float
            GL.vertex $ mkVertex (r1 * cos (a + 3 * da)) (r1 * sin (a + 3 * da)) (negate width * 0.5)
            GL.vertex $ mkVertex (r2 * cos (a + 2 * da)) (r2 * sin (a + 2 * da)) (negate width * 0.5)
            GL.vertex $ mkVertex (r2 * cos (a + da)) (r2 * sin (a + da)) (negate width * 0.5)
            GL.vertex $ mkVertex (r1 * cos a) (r1 * sin a) (negate width * 0.5)

    -- outward faces of teeth
    GL.renderPrimitive GL.QuadStrip $ do
        forM_ [0 .. pred teeth] $ \i -> do
            let a = realToFrac i * 2 * pi / realToFrac teeth :: Float
            GL.vertex $ mkVertex (r1 * cos a) (r1 * sin a) (width * 0.5)
            GL.vertex $ mkVertex (r1 * cos a) (r1 * sin a) (negate width * 0.5)
            let u = r2 * cos (a + da) - r1 * cos a
                v = r2 * sin (a + da) - r1 * sin a
                len = sqrt (u * u + v * v)
                un = u / len
                vn = v / len
            GL.normal $ mkNormal vn (-un) 0
            GL.vertex $ mkVertex (r2 * cos (a + da)) (r2 * sin (a + da)) (width * 0.5)
            GL.vertex $ mkVertex (r2 * cos (a + da)) (r2 * sin (a + da)) (negate width * 0.5)
            GL.normal $ mkNormal (cos a) (sin a) 0
            GL.vertex $ mkVertex (r2 * cos (a + 2 * da)) (r2 * sin (a + 2 * da)) (width * 0.5)
            GL.vertex $ mkVertex (r2 * cos (a + 2 * da)) (r2 * sin (a + 2 * da)) (negate width * 0.5)
            let u' = r1 * cos (a + 3 * da) - r2 * cos (a + 2 * da)
                v' = r1 * sin (a + 3 * da) - r2 * sin (a + 2 * da)
            GL.normal $ mkNormal v' (-u') 0
            GL.vertex $ mkVertex (r1 * cos (a + 3 * da)) (r1 * sin (a + 3 * da)) (width * 0.5)
            GL.vertex $ mkVertex (r1 * cos (a + 3 * da)) (r1 * sin (a + 3 * da)) (negate width * 0.5)
            GL.normal $ mkNormal (cos a) (sin a) 0

        GL.vertex $ mkVertex (r1 * cos 0) (r1 * sin 0) (width * 0.5)
        GL.vertex $ mkVertex (r1 * cos 0) (r1 * sin 0) (negate width * 0.5)

    GL.shadeModel GL.$= GL.Smooth

    -- inside radius cylinder
    GL.renderPrimitive GL.QuadStrip $
        forM_ [0 .. teeth] $ \i -> do
            let a = realToFrac i * 2 * pi / realToFrac teeth :: Float
            GL.normal $ mkNormal (negate $ cos a) (negate $ sin a) 0
            GL.vertex $ mkVertex (r0 * cos a) (r0 * sin a) (negate width * 0.5)
            GL.vertex $ mkVertex (r0 * cos a) (r0 * sin a) (width * 0.5)

mkVertex :: Float -> Float -> Float -> GL.Vertex3 GL.GLfloat
mkVertex x y z =
    GL.Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

mkNormal :: Float -> Float -> Float -> GL.Normal3 GL.GLfloat
mkNormal x y z =
    GL.Normal3 (realToFrac x) (realToFrac y) (realToFrac z)

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

isPress :: GLFW.KeyAction -> Bool
isPress GLFW.KeyPress  = True
isPress GLFW.KeyRepeat = True
isPress _              = False
