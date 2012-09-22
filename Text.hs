module Text (renderText, renderChar, TextPosition, TextSize) where

import Graphics.Rendering.OpenGL
import Control.Monad (forM_)

type TextPosition = (GLfloat, GLfloat)
type TextSize = GLfloat

renderText :: TextPosition -> TextSize -> String -> IO ()
renderText _ _ [] = return ()
renderText pos@(x, y) size (c:cs) = do
  renderChar pos size c
  renderText (x', y) size cs
    where x' = x + (charWidth + charPadding) * size

renderChar :: TextPosition -> TextSize -> Char -> IO ()
renderChar (x, y) size c = preservingMatrix $ do
  translate $ Vector3 x y 0
  scale size size size
  lineWidth $= charLineWidth
  renderPrimitive Lines $ do
  forM_ (charVertices c) $ \(x, y) -> vertex $ Vertex2 x y

charVertices :: Char -> [(GLfloat, GLfloat)]
charVertices c = let h = charHeight
                     w = charWidth
                 in case c of
  '0' -> [ (0,0), (w,0),
           (w,0), (w,h),
           (w,h), (0,h),
           (0,h), (0,0),
           (0,0), (w,h) ]
  '1' -> [ (w/2,0), (w/2,h) ]
  '2' -> [ (0,h), (w,h),
           (w,h), (w,h/2),
           (w,h/2), (0,h/2),
           (0,h/2), (0,0),
           (0,0), (w,0) ]
  '3' -> [ (0,h), (w,h),
           (0,h/2), (w,h/2),
           (0,0), (w,0),
           (w,h), (w,0) ]
  '4' -> [ (0,h), (0,h/2),
           (0,h/2), (w,h/2),
           (w,h), (w,0) ]
  '5' -> [ (0,h), (w,h),
           (0,h), (0,h/2),
           (w,h/2), (0,h/2),
           (w,h/2), (w,0),
           (0,0), (w,0) ]
  '6' -> [ (0,0), (0,h),
           (0,h), (w,h),
           (0,0), (w,0),
           (w,0), (w,h/2),
           (w,h/2), (0,h/2) ]
  '7' -> [ (0,h), (w,h),
           (w,h), (w,0) ]
  '8' -> [ (0,0), (w,0),
           (w,0), (w,h),
           (w,h), (0,h),
           (0,h), (0,0),
           (0,h/2), (w,h/2) ]
  '9' -> [ (0,h), (w,h),
           (0,h), (0,h/2),
           (0,h/2), (w,h/2),
           (w,0), (w,h) ]
  _   -> [ (0,0), (w,0) ]

charLineWidth = 1.0
charHeight    = 1.0
charWidth     = 0.6
charPadding   = 0.2
