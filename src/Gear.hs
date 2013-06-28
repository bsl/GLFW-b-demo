module Gear
  ( makeGear
  ) where

--------------------------------------------------------------------------------

import Control.Monad (forM_, when)

import qualified Graphics.Rendering.OpenGL as GL

--------------------------------------------------------------------------------

makeGear :: Float -> Float -> Float -> Int -> Float -> GL.Color4 GL.GLfloat -> IO GL.DisplayList
makeGear inradius outradius width teeth toothdepth color =
    GL.defineNewList GL.Compile $ do
        GL.colorMaterial GL.$= Just (GL.Front, GL.AmbientAndDiffuse)
        GL.color color
        drawGear
  where
    drawGear = do
        let r0   = inradius
            r1   = outradius - toothdepth / 2
            r2   = outradius + toothdepth / 2
            pi2  = 2 * pi
            da   = pi2 / realToFrac teeth / 4 :: Float
            wd2  = width * 0.5
            wd2n = negate wd2

            calca :: Int -> Float
            calca i = realToFrac i * pi2 / realToFrac teeth :: Float

        GL.shadeModel GL.$= GL.Flat
        normal 0 0 1

        -- front face
        GL.renderPrimitive GL.QuadStrip $
            forM_ [0 .. teeth] $ \i -> do
                let a = calca i
                vertex (r0 * cos a) (r0 * sin a) wd2
                vertex (r1 * cos a) (r1 * sin a) wd2
                when (i < teeth) $ do
                    vertex (r0 * cos a) (r0 * sin a) wd2
                    vertex (r1 * cos (a + 3 * da)) (r1 * sin (a + 3 * da)) wd2

        -- front sides of teeth
        GL.renderPrimitive GL.Quads $
            forM_ [0 .. pred teeth] $ \i -> do
                let a = calca i
                vertex (r1 * cos a) (r1 * sin a) wd2
                vertex (r2 * cos (a + da)) (r2 * sin (a + da)) wd2
                vertex (r2 * cos (a + 2 * da)) (r2 * sin (a + 2 * da)) wd2
                vertex (r1 * cos (a + 3 * da)) (r1 * sin (a + 3 * da)) wd2

        normal 0 0 (-1)

        -- back face
        GL.renderPrimitive GL.QuadStrip $
            forM_ [0 .. teeth] $ \i -> do
                let a = calca i
                vertex (r1 * cos a) (r1 * sin a) wd2n
                vertex (r0 * cos a) (r0 * sin a) wd2n
                when (i < teeth) $ do
                    vertex (r1 * cos (a + 3 * da)) (r1 * sin (a + 3 * da)) wd2n
                    vertex (r0 * cos a) (r0 * sin a) wd2n

        -- back sides of teeth
        GL.renderPrimitive GL.Quads $
            forM_ [0 .. pred teeth] $ \i -> do
                let a = calca i
                vertex (r1 * cos (a + 3 * da)) (r1 * sin (a + 3 * da)) wd2n
                vertex (r2 * cos (a + 2 * da)) (r2 * sin (a + 2 * da)) wd2n
                vertex (r2 * cos (a + da)) (r2 * sin (a + da)) wd2n
                vertex (r1 * cos a) (r1 * sin a) wd2n

        -- outward faces of teeth
        GL.renderPrimitive GL.QuadStrip $ do
            forM_ [0 .. pred teeth] $ \i -> do
                let a = calca i
                vertex (r1 * cos a) (r1 * sin a) wd2
                vertex (r1 * cos a) (r1 * sin a) wd2n
                let u = r2 * cos (a + da) - r1 * cos a
                    v = r2 * sin (a + da) - r1 * sin a
                    len = sqrt (u * u + v * v)
                    un = u / len
                    vn = v / len
                normal vn (-un) 0
                vertex (r2 * cos (a + da)) (r2 * sin (a + da)) wd2
                vertex (r2 * cos (a + da)) (r2 * sin (a + da)) wd2n
                normal (cos a) (sin a) 0
                vertex (r2 * cos (a + 2 * da)) (r2 * sin (a + 2 * da)) wd2
                vertex (r2 * cos (a + 2 * da)) (r2 * sin (a + 2 * da)) wd2n
                let u' = r1 * cos (a + 3 * da) - r2 * cos (a + 2 * da)
                    v' = r1 * sin (a + 3 * da) - r2 * sin (a + 2 * da)
                normal v' (-u') 0
                vertex (r1 * cos (a + 3 * da)) (r1 * sin (a + 3 * da)) wd2
                vertex (r1 * cos (a + 3 * da)) (r1 * sin (a + 3 * da)) wd2n
                normal (cos a) (sin a) 0

            vertex (r1 * cos 0) (r1 * sin 0) wd2
            vertex (r1 * cos 0) (r1 * sin 0) wd2n

        GL.shadeModel GL.$= GL.Smooth

        -- inside radius cylinder
        GL.renderPrimitive GL.QuadStrip $
            forM_ [0 .. teeth] $ \i -> do
                let a = calca i
                normal (negate $ cos a) (negate $ sin a) 0
                vertex (r0 * cos a) (r0 * sin a) wd2n
                vertex (r0 * cos a) (r0 * sin a) wd2

vertex :: Float -> Float -> Float -> IO ()
vertex x y z =
    GL.vertex $ mkVertex x y z

mkVertex :: Float -> Float -> Float -> GL.Vertex3 GL.GLfloat
mkVertex x y z =
    GL.Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

normal :: Float -> Float -> Float -> IO ()
normal x y z =
    GL.normal $ mkNormal x y z

mkNormal :: Float -> Float -> Float -> GL.Normal3 GL.GLfloat
mkNormal x y z =
    GL.Normal3 (realToFrac x) (realToFrac y) (realToFrac z)
