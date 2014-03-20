module Main where
import Geom2D.CubicBezier
import Canvas
import Graphics.UI.WX hiding (Point)
import Graphics.UI.WXCore hiding (Point)

main = start demo
       
startElements =
  CanvasElements [
     CanvasDrawPath (
        ClosedPath  [(Point 0 0, JoinLine),
                     (Point 200 10, JoinCurve (Point 200 100) (Point 310 400)),
                     (Point 300 300, JoinCurve (Point 40 50) (Point 50 60))])
        white
        True 40,
     CanvasDrawHRule 0 blue,
     CanvasDrawVRule 100 blue,

     CanvasStrokePath (
       ClosedPath  [(Point 0 0, JoinLine),
                    (Point 200 10, JoinCurve (Point 200 100) (Point 310 400)),
                    (Point 300 300, JoinCurve (Point 40 50) (Point 50 60))]
       ) black,
     
     CanvasDrawSegment (Point 0 0) (Point 40 50) red
     ] [CanvasHandle (Point 0 0)
        (HandleStyle 9 RectangleHandle black Nothing)
        (HandleStyle 9 RectangleHandle black (Just red))
        (\_ _  -> print "OnMouseDown called." )
        (\_ _  -> print "OnmouseMove called.")
        (\_ _  -> print "OnMouseRelease called.")
       ]

demo = do
  f <- frame [text := "parametric demo"]
  elements <- varCreate startElements
  c <- makeCanvas f (Rect (-2000) (-2000) 4000 4000) 0.1 elements
  set f [layout := expand $ widget c]
  set f [clientSize := sz 400 400, visible := True]