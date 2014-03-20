module Canvas (CanvasElements(..), CanvasElement(..),
               CanvasHandle(..), HandleStyle(..), HandleShape(..),
               makeCanvas) where
import Graphics.UI.WX hiding (when)
import Graphics.UI.WXCore hiding (when)
import Control.Monad
import Geom2D.CubicBezier hiding (Point)
import qualified Geom2D.CubicBezier as CB (Point(..))
import Data.List
import Data.Foldable (for_)

data CanvasElements = CanvasElements [CanvasElement] [CanvasHandle]

data CanvasState = CanvasState {
  -- logical bounds
  canvasLeft :: !Double,
  canvasBottom :: !Double,
  canvasWidth :: !Double,
  canvasHeight :: !Double,
  canvasZoom :: !Double,
  canvasRulerUnit :: !Double,
  -- logical point of middle of screen
  canvasMidX :: Double,
  canvasMidY :: Double,
  canvasDragPoint :: Maybe (Double, Double),
  canvasMousePoint :: Var (Maybe Point),
  canvasOverHandle :: Maybe CanvasHandle,
  canvasElements :: Var CanvasElements}

data CanvasElement =
  CanvasDrawPath Path Color Bool Double |
  CanvasStrokePath Path Color |
  CanvasDrawHRule Double Color |
  CanvasDrawVRule Double Color |
  CanvasDrawSegment CB.Point CB.Point Color

data CanvasHandle = CanvasHandle {
  handlePos :: CB.Point,
  handleStyle :: HandleStyle,
  handleHoverStyle :: HandleStyle,
  handleOnMouseDown :: CB.Point -> Modifiers -> IO (),
  handleOnMouseMove :: CB.Point -> Modifiers -> IO (),
  handleOnMouseRelease :: CB.Point -> Modifiers -> IO () }

data HandleStyle = HandleStyle Int HandleShape Color (Maybe Color)
data HandleShape = RectangleHandle | TiltedHandle | RoundHandle | CrossHandle

-- this should be defined in WXCore...
wxALWAYS_SHOW_SB :: Int
wxALWAYS_SHOW_SB = 0x00800000

fromInt :: Num a => Int -> a
fromInt = fromIntegral

makeCanvas :: Window a -> Rect2D Double -> Double -> Var CanvasElements -> IO (Window ())
makeCanvas win (Rect l b w h) zoom elements = do
  canvas <- window win [style := bits [wxVSCROLL, wxHSCROLL, wxALWAYS_SHOW_SB]]
  mp <- varCreate Nothing
  let state = CanvasState l b w h zoom (calcRulerUnit zoom)
              (l+w/2) (b+h/2) Nothing mp Nothing elements
  stateVar <- varCreate state
  set canvas [on mouse := canvasMouse stateVar canvas,
              on paintGc := canvasPaint stateVar canvas,
              on resize := canvasResize stateVar canvas]
  void $ windowSetBackgroundColour canvas white
  setCanvasScrollBars canvas state True
  windowOnScroll canvas (canvasScroll stateVar canvas)
  return canvas

setCanvasScrollBars :: Window a -> CanvasState -> Bool -> IO ()
setCanvasScrollBars canvas state draw = do
  Size winWidth winHeight <- windowGetClientSize canvas
  let hSR = hScrollRange state
      vSR = vScrollRange state
      hSS = min (winWidth - rulerThickness) hSR
      vSS = min (winHeight - rulerThickness) vSR
      hSP = max 0 $ min (hSR-hSS) $ hScrollPos state winWidth
      vSP = max 0 $ min (vSR-vSS) $ vScrollPos state winHeight
  windowSetScrollbar canvas wxHORIZONTAL hSP hSS hSR draw
  windowSetScrollbar canvas wxVERTICAL vSP vSS vSR draw

updateScrollState :: Int -> Int -> Int -> Int -> CanvasState -> CanvasState
updateScrollState winWidth winHeight hSP vSP state =
  let midX = fromInt (hSP + (winWidth-rulerThickness)`div`2) /
             canvasZoom state + canvasLeft state
      midY = canvasHeight state + canvasBottom state -
             fromInt (vSP + (winHeight-rulerThickness)`div`2) /
             canvasZoom state
  in state {canvasMidX = midX, canvasMidY = midY}

scrollDir :: Int -> Int -> Var CanvasState -> Window a -> IO ()
scrollDir hShift vShift stateVar canvas = do
  Size winWidth winHeight <- windowGetClientSize canvas
  hSP <- windowGetScrollPos canvas wxHORIZONTAL
  vSP <- windowGetScrollPos canvas wxVERTICAL
  hSR <- windowGetScrollRange canvas wxHORIZONTAL
  vSR <- windowGetScrollRange canvas wxVERTICAL
  hSS <- windowGetScrollThumb canvas wxHORIZONTAL
  vSS <- windowGetScrollThumb canvas wxVERTICAL
  let hSP' = if hShift >= 0
             then min (hSP+hShift) (max 0 (hSR-hSS))
             else max 0 (hSP + hShift)
      vSP' = if vShift >= 0
             then min (vSP+vShift) (max 0 (vSR-vSS))
             else max 0 (vSP+vShift)
  void $ varUpdate stateVar $ updateScrollState winWidth winHeight hSP' vSP'
  state <- varGet stateVar
  setCanvasScrollBars canvas state True
  repaint canvas

moveCanvasTo :: Window a -> Var CanvasState -> Double -> Double
                -> Int -> Int -> Int -> Int -> IO ()
moveCanvasTo canvas stateVar lx ly rx ry winWidth winHeight = do
  state <- varGet stateVar
  let newX = realX lx state winWidth
      newY = realY ly state winHeight
  scrollDir (newX-rx) (newY-ry) stateVar canvas

sqrt2 :: Double
sqrt2 = sqrt 2

zoomCanvas :: Bool -> Window a -> Var CanvasState -> Point -> IO ()
zoomCanvas down canvas stateVar (Point x y) = do
  Size winWidth winHeight <- windowGetClientSize canvas
  state <- varGet stateVar
  let mouseX = logicalX x state winWidth
      mouseY = logicalY y state winHeight
      newZoom = min 500 $
                (if down then (/sqrt2) else (*sqrt2)) $
                canvasZoom state
      newState = state {canvasZoom = newZoom,
                        canvasRulerUnit = calcRulerUnit newZoom}
  varSet stateVar newState
  setCanvasScrollBars canvas newState False
  moveCanvasTo canvas stateVar mouseX mouseY x y winWidth winHeight

canvasMouse :: Var CanvasState -> Window a -> EventMouse -> IO ()
canvasMouse stateVar canvas mouseEv = do
  Size winWidth winHeight <- windowGetClientSize canvas
  state <- varGet stateVar
  let h = canvasOverHandle state
  case mouseEv of
    MouseWheel down p modifiers
      | shiftDown modifiers ->
        scrollDir (if down then 40 else -40) 0 stateVar canvas
      | controlDown modifiers -> zoomCanvas down canvas stateVar p
      | otherwise ->
        scrollDir 0 (if down then 40 else -40) stateVar canvas
      
    MouseMiddleDown (Point x y) _ -> 
      varSet stateVar (state {canvasDragPoint = Just (logicalX x state winWidth,
                                                      logicalY y state winHeight)})
    MouseMiddleUp _ _ ->
      varSet stateVar (state {canvasDragPoint = Nothing})

    MouseLeftDrag (Point x y) modifiers -> do
     resetMousePointer state canvas (Point x y)
     for_ h $ \handle ->
       handleOnMouseMove handle
       (CB.Point (logicalX x state winWidth )
        (logicalY y state winHeight)) modifiers

    MouseMiddleDrag (Point x y) _ -> do
      resetMousePointer state canvas (Point x y)
      for_ (canvasDragPoint state) $ \(lx, ly) -> 
        moveCanvasTo canvas stateVar lx ly x y winWidth winHeight

    MouseLeftUp (Point x y) modifiers -> do
      for_ h $ \handle ->
        handleOnMouseRelease handle
        (CB.Point (logicalX x state winWidth)
         (logicalY y state winHeight)) modifiers

    MouseLeftDown (Point x y) modifiers -> do
      for_ h $ \handle ->
        handleOnMouseDown handle
        (CB.Point (logicalX x state winWidth)
         (logicalY y state winHeight)) modifiers

    MouseMotion p _ -> do
      resetMousePointer state canvas p
      -- unset pointOverHandle when leaving handle
      for_ h $ \handle ->
        unless (pointOverHandle p state winWidth winHeight handle) $ do
          void $ varUpdate stateVar $ \s ->
            s {canvasOverHandle = Nothing}
          windowRefreshRect canvas True (handleRect state winWidth winHeight handle)
      h' <- canvasOverHandle `fmap` varGet stateVar
      -- set pointOverHandle when entering handle
      case h' of
        Nothing -> do
          CanvasElements _ handles <- varGet $ canvasElements state
          for_ (handleAtPoint p state winWidth winHeight handles) $ \handle -> do
            void $ varUpdate stateVar $ \s ->
              s {canvasOverHandle = Just handle}
            windowRefreshRect canvas True (handleRect state winWidth winHeight handle)
        _ -> return ()
      repaint canvas

    _ -> return ()

resetMousePointer :: CanvasState -> Window a -> Point -> IO ()
resetMousePointer state canvas (Point x y) = do
  prevPoint <- varGet $ canvasMousePoint state
  varSet (canvasMousePoint state) (Just $ Point x y)
  for_ prevPoint $ \(Point x' y') -> do
    windowRefreshRect canvas True
      (Rect (x'-rulerSmall) (rulerThickness-rulerSmall)
       (rulerThickness*2+1) (rulerSmall))
    windowRefreshRect canvas True
      (Rect (rulerThickness-rulerSmall) (y'-rulerSmall)
       (rulerSmall) (rulerThickness*2+1))
  
canvasResize :: Var CanvasState -> Window a -> IO ()
canvasResize stateVar canvas = do
  state <- varGet stateVar
  setCanvasScrollBars canvas state False
  repaint canvas

canvasTransform :: CanvasState -> Int -> Int -> Transform
canvasTransform state winWidth winHeight =
  translate (CB.Point (fromInt $ (winWidth+rulerThickness)`div`2)
             (fromInt $ (winHeight+rulerThickness)`div`2)) $*
  scale (canvasZoom state) (negate $ canvasZoom state) $*
  translate (CB.Point (negate $ canvasMidX state) (negate $ canvasMidY state))

canvasPaint :: Var CanvasState -> Window a -> GCDC b -> Rect -> IO ()
canvasPaint stateVar canvas dc view = do
  state <- varGet stateVar
  set dc [font := rulerFont]
  (Size _ ht, descend, _) <- getFullTextExtent dc "1"
  let textHt = ht - descend
  Size winWidth winHeight <- windowGetClientSize canvas  
  drawHorizRuler state dc view winWidth
  drawVertRuler state dc view textHt winHeight
  drawMousePointers state dc
  CanvasElements elements handles <- varGet $ canvasElements state
  let tf = canvasTransform state winWidth winHeight
  gc <- gcdcGetGraphicsContext dc
  graphicsContextClipByRectangle gc
    (Rect (fromInt rulerThickness) (fromInt rulerThickness)
     (fromInt $ winWidth-rulerThickness) (fromInt $ winHeight-rulerThickness))
  mapM_ (drawElement gc tf state winWidth winHeight) elements
  mp <- varGet $ canvasMousePoint state
  mapM_ (drawHandle dc tf mp state winWidth winHeight) handles
  
drawMousePointers :: CanvasState -> DC a -> IO ()
drawMousePointers state dc = do
  mp <- varGet $ canvasMousePoint state
  for_ mp $ \(Point x y) -> do
    set dc [brush := BrushStyle BrushSolid black]
    polygon dc [Point (x-rulerSmall+2) (rulerThickness-rulerSmall),
                Point (x+rulerSmall-2) (rulerThickness-rulerSmall),
                Point x (rulerThickness-2)] []
    polygon dc [Point (rulerThickness-rulerSmall) (y-rulerSmall+2),
                Point (rulerThickness-rulerSmall) (y+rulerSmall-2),
                Point (rulerThickness-2) y] []

canvasScroll :: Var CanvasState -> Window a -> EventScroll -> IO ()
canvasScroll stateVar canvas evScroll = do
  hSP <- windowGetScrollPos canvas wxHORIZONTAL
  vSP <- windowGetScrollPos canvas wxVERTICAL
  hSR <- windowGetScrollRange canvas wxHORIZONTAL
  vSR <- windowGetScrollRange canvas wxVERTICAL
  hSS <- windowGetScrollThumb canvas wxHORIZONTAL
  vSS <- windowGetScrollThumb canvas wxVERTICAL
  Size winWidth winHeight <- windowGetClientSize canvas  
  let hMax = hSR - hSS
      vMax = vSR - vSS
      (hSP', vSP') = case evScroll of
        ScrollLineUp Horizontal _ -> (min hMax (hSP+1-hSS`div`10), vSP)
        ScrollLineUp Vertical _ -> (hSP, min vMax (vSP+1-vSS`div`10))
        ScrollLineDown Horizontal _ -> (max 0 (hSP-1+hSS`div`10), vSP)
        ScrollLineDown Vertical _ -> (hSP, max 0 (vSP-1+vSS`div`10))
        _ -> (hSP, vSP)
  windowSetScrollbar canvas wxHORIZONTAL hSP' hSS hSR (hSP' /= hSP)
  windowSetScrollbar canvas wxVERTICAL vSP' vSS vSR (vSP' /= vSP)
  void $ varUpdate stateVar (updateScrollState winWidth winHeight hSP' vSP')
  repaint canvas

rulerThickness, rulerLarge, rulerSmall, rulerTiny, rulerMinUnit :: Int
rulerThickness = 16    -- ruler thickness in pixels
rulerLarge = 14     
rulerSmall = 6     -- minimum length of a ruler unit in pixels
rulerTiny = 3
rulerMinUnit = 70

calcRulerUnit :: Double -> Double
calcRulerUnit zoom = result
  where
    result | minUnit <= scale_ = scale_
           | scale_ < 10 && minUnit <= 2 = 2
           | scale_ >= 10 && minUnit <= 2.5*scale_ = 2.5*scale_
           | minUnit <= 5*scale_ = 5*scale_
           | otherwise = 10*scale_
    minUnit = fromInt rulerMinUnit/zoom
    scale_ = max 1 $ 10**(fromInt $ floor $ logBase 10.0 minUnit)

rulerColor :: Color
rulerColor = lightgrey

rulerFont  :: FontStyle
rulerFont = FontStyle 7 FontSwiss ShapeNormal WeightNormal False "" wxFONTENCODING_DEFAULT

logicalX, logicalY :: Int -> CanvasState -> Int -> Double
logicalX x state winWidth =
  fromInt (x - (winWidth+rulerThickness) `div` 2) / canvasZoom state +
  canvasMidX state
logicalY y state winHeight =
  fromInt ((winHeight+rulerThickness) `div` 2 - y) / canvasZoom state +
  canvasMidY state

realX, realY :: Double -> CanvasState -> Int -> Int
realX x state winWidth =
  round ((x - canvasMidX state) * canvasZoom state) + (winWidth+rulerThickness)`div`2
realY y state winHeight =
  round ((canvasMidY state - y) * canvasZoom state) + (winHeight+rulerThickness)`div`2

hScrollPos, vScrollPos :: CanvasState -> Int -> Int
hScrollPos state winWidth =
  round ((canvasMidX state - canvasLeft state) * canvasZoom state) -
  (winWidth - rulerThickness)`div`2
vScrollPos state winHeight =
  round ((canvasHeight state + canvasBottom state - canvasMidY state) * canvasZoom state) -
  (winHeight - rulerThickness)`div`2

hScrollRange, vScrollRange :: CanvasState -> Int
hScrollRange state =
  round $ canvasWidth state * canvasZoom state
vScrollRange state =
  round $ canvasHeight state * canvasZoom state

unitToStr :: Double -> Double -> [Char]
unitToStr unit x
  | x == 0 = "0"
  | unit >= 10000 = (show (round $ x/1000 :: Int)) ++ "k"
  | otherwise = show $ (round x :: Int)

drawHorizRuler :: CanvasState -> DC a -> Rect -> Int -> IO ()
drawHorizRuler state dc (Rect l t w _) winWidth
  | t >= rulerThickness = return ()
  | otherwise = do
    dcSetClippingRegion dc (Rect 0 0 winWidth rulerThickness)
    set dc [penKind := PenTransparent,
            brush := BrushStyle BrushSolid rulerColor]
    drawRect dc (Rect l 0 w rulerThickness) []
    dcSetClippingRegion dc (Rect rulerThickness 0 (winWidth-rulerThickness) rulerThickness)
    let unit = canvasRulerUnit state
        unitPix = unit * canvasZoom state
        unitStart = (fromInt $ floor $ logicalX l state winWidth/unit) * unit
        startX = fromInt $ realX unitStart state winWidth
        n = fromInt $ ceiling $ (fromInt (w+l)-startX)/unitPix :: Double
    set dc [brush := BrushStyle BrushSolid black,
            penKind := PenSolid,
            penColor := black]
    mapM_ (\x -> drawRect dc (Rect (round x) (rulerThickness - rulerLarge) 1 rulerLarge) [])
      [startX, startX+unitPix..startX+unitPix*n]

    mapM_ (\x -> drawRect dc (Rect (round x) (rulerThickness - rulerSmall) 1 (rulerSmall)) [])
      [startX, startX+unitPix/5..startX+unitPix*n]

    mapM_ (\x -> drawRect dc (Rect (round x) (rulerThickness - rulerTiny) 1 rulerTiny) [])
      [startX+unitPix/10, startX+unitPix/5..startX+unitPix*n]

    mapM_ (\i -> drawText dc (unitToStr unit (unitStart + unit*i))
                 (Point (round $ startX+unitPix*i+2) 0) [])
      [0..n]
    dcDestroyClippingRegion dc

drawVertRuler
  :: CanvasState -> DC a -> Rect2D Int -> Int -> Int -> IO ()
drawVertRuler state dc (Rect l t _ h) textHt winHeight
  | l >= rulerThickness = return ()
  | otherwise = do
    dcSetClippingRegion dc (Rect 0 rulerThickness rulerThickness h)
    set dc [penKind := PenTransparent,
            brush := BrushStyle BrushSolid rulerColor]
    drawRect dc (Rect 0 t rulerThickness h) []
    let unit = canvasRulerUnit state
        unitPix = unit * canvasZoom state
        unitStart = (fromInt $ ceiling $ logicalY t state winHeight / unit) * unit
        startY = fromInt $ realY unitStart state winHeight
        n = fromInt $ ceiling $ (fromInt (t+h)-startY)/unitPix
    set dc [brush := BrushStyle BrushSolid black,
            penKind := PenSolid,
            penColor := black,
            font := rulerFont]
    mapM_ (\y -> drawRect dc (Rect (rulerThickness - rulerLarge) (round y) rulerLarge 1) [])
      [startY, startY+unitPix..startY+unitPix*n]

    mapM_ (\y -> drawRect dc (Rect (rulerThickness - rulerSmall) (round y) rulerSmall 1) [])
      [startY, startY+unitPix/5..startY+unitPix*n]

    mapM_ (\y -> drawRect dc (Rect (rulerThickness - rulerTiny) (round y) rulerTiny 1) [])
      [startY+unitPix/10, startY+unitPix/5..startY+unitPix*n]

    mapM_ (\i -> drawTextVert dc (unitToStr unit (unitStart - unit*i)) (Point 2 (round (startY+unitPix*i+2))) textHt)
      [0..n]
    dcDestroyClippingRegion dc

drawTextVert :: DC a -> [Char] -> Point -> Int -> IO ()
drawTextVert dc txt (Point x y) textHt = mapM_ drawTextOne $
                                       zip (map (:[]) txt) [y, y+textHt ..]
  where drawTextOne (t, y') = drawText dc t (Point x y') []

pointToWx :: CB.Point -> Point2 Double
pointToWx (CB.Point x y) = Point x y

drawPathRest :: GraphicsPath a -> [(CB.Point, PathJoin)] -> CB.Point -> IO ()
drawPathRest _ [] _ = return ()
drawPathRest gp [(_, JoinCurve p q)] r =
  graphicsPathAddCurveToPoint gp (pointToWx p) (pointToWx q) (pointToWx r)
drawPathRest gp ((_, JoinCurve p q):rest@((r, _):_)) s = do
  graphicsPathAddCurveToPoint gp (pointToWx p) (pointToWx q) (pointToWx r)
  drawPathRest gp rest s
drawPathRest gp [(_, JoinLine)] r =
  graphicsPathAddLineToPoint gp (pointToWx r)
drawPathRest gp ((_, JoinLine):rest@((r, _):_)) s = do
  graphicsPathAddLineToPoint gp (pointToWx r)
  drawPathRest gp rest s


path2graphicsPath :: Path -> Transform -> GraphicsContext a -> IO (GraphicsPath ())
path2graphicsPath path tf gc = do
  gp <- graphicsContextCreatePath gc
  case transform tf path of
    ClosedPath [] -> return ()
    ClosedPath p@((q,_):_) -> do
      graphicsPathMoveToPoint gp (pointToWx q)
      drawPathRest gp p q
      graphicsPathCloseSubpath gp
    OpenPath [] _ -> return ()
    OpenPath p@((q,_):_) r -> do
      graphicsPathMoveToPoint gp (pointToWx q)
      drawPathRest gp p r
  return gp

drawElement :: GraphicsContext a -> Transform -> CanvasState ->
               Int -> Int -> CanvasElement -> IO ()
drawElement gc tf state _ _ (CanvasDrawPath path fillColor fillp lineWidth) = do
  gp <- path2graphicsPath path tf gc
  when fillp $ 
    withBrushStyle (BrushStyle BrushSolid fillColor) $
    \brush_ -> do
      graphicsContextSetBrush gc brush_
      graphicsContextFillPath gc gp wxWINDING_RULE
  when (lineWidth > 0) $
    withPenStyle (penColored fillColor (round $ min 10000 $ lineWidth*canvasZoom state)) $
    \pen_ -> do
      graphicsContextSetPen gc pen_
      graphicsContextStrokePath gc gp
  graphicsPathDelete gp

drawElement gc tf _ _ _ (CanvasStrokePath path strokeColor) = do
  gp <- path2graphicsPath path tf gc
  withPenStyle (penColored strokeColor 1) $
    \pen_ -> do
      graphicsContextSetPen gc pen_
      graphicsContextStrokePath gc gp
  graphicsPathDelete gp

--intersectRect (line p q) r s = undefined

drawElement gc _ state w h (CanvasDrawVRule x color_) =
  withBrushStyle (BrushStyle BrushSolid color_) $ \brush_ ->
  withPenStyle penTransparent $ \pen_ -> do
    graphicsContextSetBrush gc brush_
    graphicsContextSetPen gc pen_
    graphicsContextDrawRectangle gc
      (Rect (fromInt $ realX x state w) (fromInt rulerThickness)
       1 (fromInt h))

drawElement gc _ state w h (CanvasDrawHRule y color_) =
  withBrushStyle (BrushStyle BrushSolid color_) $ \brush_ ->
  withPenStyle penTransparent $ \pen_ -> do
    graphicsContextSetBrush gc brush_
    graphicsContextSetPen gc pen_
    graphicsContextDrawRectangle gc
      (Rect (fromInt rulerThickness) (fromInt $ realY y state h) 
        (fromInt w) 1)

drawElement gc tf _ _ _ (CanvasDrawSegment from to color_) =
  withPenStyle (penColored color_ 1) $
  \pen_ -> do
    graphicsContextSetPen gc pen_
    graphicsContextStrokeLine gc (pointToWx $ transform tf from)
      (pointToWx $ transform tf to)

drawHandle :: DC a -> Transform -> Maybe Point -> CanvasState
           -> Int -> Int -> CanvasHandle -> IO ()
drawHandle dc tf p state w h handle = 
  drawHandleStyle dc tf (handlePos handle) $
  case p of
    Just p' | pointOverHandle p' state w h handle ->
      handleHoverStyle handle
    _ -> handleStyle handle

drawHandleStyle :: DC a -> Transform -> CB.Point -> HandleStyle -> IO ()
drawHandleStyle dc tf pos (HandleStyle width' RectangleHandle stroke fill_) =
  let CB.Point x y = transform tf pos
      width = width' + if even width' then 1 else 0
  in drawRect dc (Rect (round x-width`div`2) (round y-width`div`2) width width)
     [brush := case fill_ of
         Nothing -> brushTransparent
         Just f -> BrushStyle BrushSolid f,
      pen := penColored stroke 1]

drawHandleStyle dc tf pos (HandleStyle width RoundHandle stroke fill_) =
  let CB.Point x y = transform tf pos
  in circle dc (Point (round x) (round y)) (width`div`2)
     [brush := case fill_ of
         Nothing -> brushTransparent
         Just f -> BrushStyle BrushSolid f,
      pen := penColored stroke 1]

drawHandleStyle dc tf pos (HandleStyle width' TiltedHandle stroke fill_) =
  let CB.Point x' y' = transform tf pos
      x = round x'
      y = round y'
      width = width' + if even width' then 1 else 0
      halfW = width`div`2
  in polygon dc [(Point (x - halfW) y), (Point x (y - halfW)),
                 (Point (x + halfW) y), (Point x (y + halfW))]
     [brush := case fill_ of
         Nothing -> brushTransparent
         Just f -> BrushStyle BrushSolid f,
      pen := penColored stroke 1]

drawHandleStyle dc tf pos (HandleStyle width' CrossHandle stroke fill_) = do
  let CB.Point x' y' = transform tf pos
      x = round x'
      y = round y'
      width = width' + if even width' then 1 else 0
      halfW = width`div`2
  for_ fill_ $ \f ->
    drawRect dc (Rect (x-halfW) (y-halfW) width width)
    [brush := BrushStyle BrushSolid f]
  set dc [pen := penColored stroke 1]
  line dc (Point (x-halfW) (y-halfW)) (Point (x+halfW) (y+halfW)) []
  line dc (Point (x-halfW) (y+halfW)) (Point (x+halfW) (y-halfW)) []

handleAtPoint :: Point -> CanvasState -> Int -> Int
     -> [CanvasHandle] -> Maybe CanvasHandle
handleAtPoint p state w h handles =
  find (pointOverHandle p state w h) handles


pointOverHandle :: Point -> CanvasState -> Int -> Int -> CanvasHandle -> Bool
pointOverHandle (Point x y) state w h handle =
  let (CB.Point cx cy) = handlePos handle
      rx = realX cx state w
      ry = realY cy state h
      HandleStyle radius _ _ _ = handleStyle handle
  in x >= rx - radius`div`2 && x <= rx + radius`div`2 &&
     y >= ry - radius`div`2 && y <= ry + radius`div`2

handleRect :: CanvasState -> Int -> Int -> CanvasHandle -> Rect2D Int
handleRect state w h handle =
  let (CB.Point cx cy) = handlePos handle
      rx = realX cx state w
      ry = realY cy state h
      HandleStyle radius _ _ _ = handleStyle handle
  in Rect (rx - radius`div`2) (ry - radius`div`2)
     (radius+1) (radius+1)

