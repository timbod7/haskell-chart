module Renderable where

import qualified Graphics.Rendering.Cairo as C
import Control.Monad
import Types
import Plot

-- | A Renderable has an Cairo action to calculate a minimum size,
-- and a Cairo action for drawing it within a specified rectangle.

data Renderable = Renderable {
   minsize :: C.Render RectSize,
   render ::  Rect -> C.Render ()
}

emptyRenderable = Renderable {
   minsize = return (0,0),
   render  = \_ -> return ()
}

addMargins :: (Double,Double,Double,Double) -> Renderable -> Renderable
addMargins (t,b,l,r) rd = Renderable { minsize = mf, render = rf }
  where
    mf = do
        (w,h) <- minsize rd
        return (w+l+r,h+t+b)

    rf r1@(Rect p1 p2) = do
        render rd (Rect (p1 `padd` (Point l t)) (p2 `psub` (Point r b)))

fillBackground :: CairoFillStyle -> Renderable -> Renderable
fillBackground fs r = Renderable { minsize = minsize r, render = rf }
  where
    rf rect@(Rect p1 p2) = do
        C.save
        setClipRegion p1 p2
        setFillStyle fs
        C.paint
        C.restore
	render r rect

vertical :: [(Double,Renderable)] -> Renderable 
vertical rs = Renderable { minsize = mf, render = rf }
  where
    mf = do
        (_,wmin,hmin) <- calcSizes
	return (wmin, hmin)

    rf (Rect p1 p2) = do
        (sizes,wmin,hmin) <- calcSizes
	let wactual = p_x p2 - p_x p1
	let hextra = p_y p2 - p_y p1 - hmin
	let etotal = sum (map fst rs)
	let rs' = [ (wactual,h + hextra * e / etotal,r)
		    | ((e,r),(w,h)) <- zip rs sizes ]
	foldM_ render1 p1 rs'

    calcSizes = do
        sizes <- mapM minsize [ r | (_,r) <- rs]
	let wmin = maximum [ w | (w,h) <- sizes ]
	let hmin = sum [ h | (w,h) <- sizes ]
	return (sizes,wmin,hmin)
    
    render1 :: Point -> (Double,Double,Renderable) -> C.Render Point
    render1 p (w,h,r) = do
        render r (Rect p (p `padd` Point w h))
	return (p `padd` Point 0 h)

class ToRenderable a where
   toRenderable :: a -> Renderable

renderableToPNGFile :: Renderable -> Int -> Int -> FilePath -> IO ()
renderableToPNGFile chart width height path = 
    C.withImageSurface C.FormatARGB32 width height $ \result -> do
    C.renderWith result $ rfn
    C.surfaceWriteToPNG result path
  where
    rfn = do
        setupRender
	render chart rect

    rect = Rect (Point 0 0) (Point (fromIntegral width) (fromIntegral height))


setupRender :: C.Render ()
setupRender = do
    -- move to centre of pixels so that stroke width of 1 is
    -- exactly one pixel 
    C.translate 0.5 0.5

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

minsizeLegend :: Legend -> C.Render RectSize
minsizeLegend (Legend _ ls plots) = do
    let labels = map fst plots
    lsizes <- mapM textSize labels
    lgap <- legendSpacer
    let lm = legend_margin ls
    let pw = legend_plot_size ls
    let h = maximum  [h | (w,h) <- lsizes]
    let n = fromIntegral (length lsizes)
    let w = sum [w + lgap | (w,h) <- lsizes] + pw * (n+1) + lm * (n-1)
    return (w,h)

renderLegend :: Legend -> Rect -> C.Render ()
renderLegend (Legend _ ls plots) (Rect rp1 rp2) = do
    foldM_ rf rp1 plots
  where
    lm = legend_margin ls
    lps = legend_plot_size ls

    rf :: Point -> (String,Plot) -> C.Render Point
    rf p1 (label,plot) = do
        (w,h) <- textSize label
	lgap <- legendSpacer
	let p2 = (p1 `padd` Point lps 0)
        plot_render_legend plot (mkrect p1 rp1 p2 rp2)
	let p3 = Point (p_x p2 + lgap) (p_y rp1)
	drawText HTA_Left VTA_Top p3 label
        return (p3 `padd` Point (w+lm) 0)

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
label fs hta vta s = Renderable { minsize = mf, render = rf }
  where
    mf = do
       C.save
       setFontStyle fs
       sz <- textSize s
       C.restore
       return sz
    rf (Rect p1 p2) = do
       C.save
       setFontStyle fs
       let p = Point (xp hta (p_x p1) (p_x p2)) (yp vta (p_y p1) (p_y p2))
       drawText hta vta p s
       C.restore
    xp HTA_Left x1 x2 = x1
    xp HTA_Centre x1 x2 = (x1+x2)/2
    xp HTA_Right x1 x2 = x2
    yp VTA_Top y1 y2 = y2
    yp VTA_Centre y1 y2 = (y1+y2)/2
    yp VTA_Bottom y1 y2 = y1

