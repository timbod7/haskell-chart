-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Renderable
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)

module Graphics.Rendering.Chart.Renderable where

import qualified Graphics.Rendering.Cairo as C
import Control.Monad
import Data.List ( nub, partition, transpose, sort )

import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Plot

-- | A Renderable is a record of functions required to layout a
-- graphic element.
data Renderable = Renderable {

   -- | a Cairo action to calculate a minimum size,
   minsize :: CRender RectSize,

   -- | a Cairo action for drawing it within a specified rectangle.
   render ::  Rect -> CRender ()
}

-- | A type class abtracting the conversion of a value to a
-- Renderable.

class ToRenderable a where
   toRenderable :: a -> Renderable

emptyRenderable = spacer (0,0)

spacer sz = Renderable {
   minsize = return sz,
   render  = \_ -> return ()
}

addMargins :: (Double,Double,Double,Double) -> Renderable -> Renderable
addMargins (t,b,l,r) rd = Renderable { minsize = mf, render = rf }
  where
    mf = do
        (w,h) <- minsize rd
        return (w+l+r,h+t+b)

    rf r1@(Rect p1 p2) = do
        render rd (Rect (p1 `pvadd` (Vector l t)) (p2 `pvsub` (Vector r b)))

fillBackground :: CairoFillStyle -> Renderable -> Renderable
fillBackground fs r = Renderable { minsize = minsize r, render = rf }
  where
    rf rect@(Rect p1 p2) = do
        preserveCState $ do
            setClipRegion p1 p2
            setFillStyle fs
            c $ C.paint
	render r rect

vertical, horizontal :: [(Double,Renderable)] -> Renderable 
vertical rs = grid [1] (map fst rs) [[(0,snd r)] | r <- rs]
horizontal rs = grid (map fst rs) [1] [[(0,snd r) | r <- rs]]

-- | Layout multiple Renderables into a grid.
-- Arg 1 is the weights for the allocation of extra horizontal space
-- to columns, Arg 2 is the weights for the allocation of extra
-- vertical space to rows, and Arg 3 is the grid of renderables to be
-- layed out. Each element of the grid is a tuple - the first item of
-- the tuple is the drawing priority.  Lower priorities get drawn
-- first. Drawing order is significant when Renderables draw outside
-- their edges.
grid :: [Double] -> [Double] -> [[(Int,Renderable)]] -> Renderable
grid we he rss = Renderable { minsize = mf, render = rf }
  where
    mf = do
      msizes <- getSizes
      let widths = (map.map) fst msizes
      let heights = (map.map) snd msizes
      return ((sum.map maximum.transpose) widths,(sum.map maximum) heights)

    rf (Rect p1 p2) = do
      msizes <- getSizes
      let widths = (map maximum.(map.map) fst.transpose) msizes
      let heights = (map maximum.(map.map) snd) msizes
      let widths1 = allocate (p_x p2 - p_x p1 - sum widths) we widths
      let heights1 = allocate (p_y p2 - p_y p1 - sum heights) he heights
      let xs = scanl (+) (p_x p1) widths1
      let ys = scanl (+) (p_y p1) heights1
      
      forM_ priorities $ \pr->
        forM_ (zip3 rss ys (tail ys))  $ \(rs,y0,y1) ->
          forM_ (zip3 rs xs (tail xs))  $ \((n,r),x0,x1) ->
            when (n==pr) $ render r (Rect (Point x0 y0) (Point x1 y1))

    getSizes = (mapM.mapM) (\(n,r)-> minsize r) rss
    priorities = sort (nub ((concatMap.map) fst rss))

allocate :: Double -> [Double] -> [Double] -> [Double]
allocate extra ws vs = zipWith (+) vs (extras++[0,0..])
  where
    total = sum ws 
    extras = [ extra * v / total | v <- ws ]

-- | Output the given renderable to a PNG file of the specifed size
-- (in pixels), to the specified file.
renderableToPNGFile :: Renderable -> Int -> Int -> FilePath -> IO ()
renderableToPNGFile chart width height path = 
    C.withImageSurface C.FormatARGB32 width height $ \result -> do
    C.renderWith result $ runCRender rfn bitmapEnv
    C.surfaceWriteToPNG result path
  where
    rfn = do
        alignPixels
	render chart rect

    rect = Rect (Point 0 0) (Point (fromIntegral width) (fromIntegral height))

-- | Output the given renderable to a PDF file of the specifed size
-- (in points), to the specified file.
renderableToPDFFile :: Renderable -> Int -> Int -> FilePath -> IO ()
renderableToPDFFile chart width height path = 
    C.withPDFSurface path (fromIntegral width) (fromIntegral height) $ \result -> do
    C.renderWith result $ runCRender rfn vectorEnv
    C.surfaceFinish result
  where
    rfn = do
	render chart rect
        c $ C.showPage

    rect = Rect (Point 0 0) (Point (fromIntegral width) (fromIntegral height))

-- | Output the given renderable to a postscript file of the specifed size
-- (in points), to the specified file.
renderableToPSFile :: Renderable -> Int -> Int -> FilePath -> IO ()
renderableToPSFile chart width height path = 
    C.withPSSurface path (fromIntegral width) (fromIntegral height) $ \result -> do
    C.renderWith result $ runCRender rfn vectorEnv
    C.surfaceFinish result
  where
    rfn = do
	render chart rect
        c $ C.showPage

    rect = Rect (Point 0 0) (Point (fromIntegral width) (fromIntegral height))

bitmapEnv = CEnv adjfn
  where
    adjfn (Point x y)= Point (fromIntegral (round x)) (fromIntegral (round y))

vectorEnv = CEnv id

alignPixels :: CRender ()
alignPixels = do
    -- move to centre of pixels so that stroke width of 1 is
    -- exactly one pixel 
    c $ C.translate 0.5 0.5

embedRenderable :: CRender Renderable -> Renderable
embedRenderable ca = Renderable {
   minsize = do { a <- ca; minsize a },
   render = \ r -> do { a <- ca; render a r }
}


----------------------------------------------------------------------
-- Legend

data LegendStyle = LegendStyle {
   legend_label_style :: CairoFontStyle,
   legend_margin :: Double,
   legend_plot_size :: Double
}

data Legend = Legend Bool LegendStyle [(String,Plot)]

instance ToRenderable Legend where
  toRenderable l = Renderable {
    minsize=minsizeLegend l,
    render=renderLegend l
  }

minsizeLegend :: Legend -> CRender RectSize
minsizeLegend (Legend _ ls plots) = do
    let labels = nub $ map fst plots
    setFontStyle $ legend_label_style ls
    lsizes <- mapM textSize labels
    lgap <- legendSpacer
    let lm = legend_margin ls
    let pw = legend_plot_size ls
    let h = maximum  [h | (w,h) <- lsizes]
    let n = fromIntegral (length lsizes)
    let w = sum [w + lgap | (w,h) <- lsizes] + pw * (n+1) + lm * (n-1)
    return (w,h)

renderLegend :: Legend -> Rect -> CRender ()
renderLegend (Legend _ ls plots) (Rect rp1 rp2) = do
    foldM_ rf rp1 $ join_nub plots
  where
    lm = legend_margin ls
    lps = legend_plot_size ls

    rf :: Point -> (String,[Plot]) -> CRender Point
    rf p1 (label,theseplots) = do
        setFontStyle $ legend_label_style ls
        (w,h) <- textSize label
	lgap <- legendSpacer
	let p2 = (p1 `pvadd` Vector lps 0)
        mapM_ (\p -> plot_render_legend p (mkrect p1 rp1 p2 rp2)) theseplots
	let p3 = Point (p_x p2 + lgap) (p_y rp1)
	drawText HTA_Left VTA_Top p3 label
        return (p3 `pvadd` Vector (w+lm) 0)
    join_nub :: [(String, a)] -> [(String, [a])]
    join_nub ((x,a1):ys) = case partition ((==x) . fst) ys of
                           (xs, rest) -> (x, a1:map snd xs) : join_nub rest
    join_nub [] = []

legendSpacer = do
    (lgap,_) <- textSize "X"
    return lgap

defaultLegendStyle = LegendStyle {
    legend_label_style=defaultFontStyle,
    legend_margin=20,
    legend_plot_size=20
}


----------------------------------------------------------------------
-- Labels

label :: CairoFontStyle -> HTextAnchor -> VTextAnchor -> String -> Renderable
label fs hta vta = rlabel fs hta vta 0

rlabel :: CairoFontStyle -> HTextAnchor -> VTextAnchor -> Double -> String -> Renderable
rlabel fs hta vta rot s = Renderable { minsize = mf, render = rf }
  where
    mf = preserveCState $ do
       setFontStyle fs
       (w,h) <- textSize s
       return (w*acr+h*asr,w*asr+h*acr)
    rf (Rect p1 p2) = preserveCState $ do
       setFontStyle fs
       sz@(w,h) <- textSize s
       c $ C.translate (xadj sz hta (p_x p1) (p_x p2)) (yadj sz vta (p_y p1) (p_y p2))
       c $ C.rotate rot'
       c $ C.moveTo (-w/2) (h/2)
       c $ C.showText s
    xadj (w,h) HTA_Left x1 x2 =  x1 +(w*acr+h*asr)/2
    xadj (w,h) HTA_Centre x1 x2 = (x1 + x2)/2
    xadj (w,h) HTA_Right x1 x2 =  x2 -(w*acr+h*asr)/2
    yadj (w,h) VTA_Top y1 y2 =  y1 +(w*asr+h*acr)/2
    yadj (w,h) VTA_Centre y1 y2 = (y1+y2)/2
    yadj (w,h) VTA_Bottom y1 y2 =  y2 - (w*asr+h*acr)/2

    rot' = rot / 180 * pi
    (cr,sr) = (cos rot', sin rot')
    (acr,asr) = (abs cr, abs sr)

-- a quick test to display labels with all combinations
-- of anchors
labelTest rot = renderableToPNGFile r 800 800 "labels.png"
  where
    r = fillBackground fwhite $ grid [1,1,1] [1,1,1] ls
    ls = [ [(0,addMargins (20,20,20,20) $ fillBackground fblue $ crossHairs $ rlabel fs h v rot s) | h <- hs] | v <- vs ]
    s = "Labels"
    hs = [HTA_Left, HTA_Centre, HTA_Right]
    vs = [VTA_Top, VTA_Centre, VTA_Bottom]
    fwhite = solidFillStyle white
    fblue = solidFillStyle (Color 0.8 0.8 1)
    fs = fontStyle "sans" 30 C.FontSlantNormal C.FontWeightBold
    crossHairs r =Renderable {
      minsize = minsize r,
      render = \rect@(Rect (Point x1 y1) (Point x2 y2)) -> do
          let xa = (x1 + x2) / 2
          let ya = (y1 + y2) / 2
          strokeLines [Point x1 ya,Point x2 ya]
          strokeLines [Point xa y1,Point xa y2]
          render r rect
    }
    
