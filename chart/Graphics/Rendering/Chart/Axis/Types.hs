-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Axis.Types
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Type definitions for Axes
--

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Axis.Types(
    AxisData(..),
    AxisT(..),
    AxisStyle(..),
    PlotValue(..),
    AxisFn,

    defaultAxisLineStyle,
    defaultAxisStyle,
    defaultGridLineStyle,

    makeAxis,
    makeAxis',

    axisToRenderable,
    renderAxisGrid,
    axisOverhang,
    vmap,
    invmap,

    linMap,
    invLinMap,

    axisGridAtTicks,
    axisGridAtBigTicks,
    axisGridAtLabels,
    axisGridHide,
    axisTicksHide,
    axisLabelsHide,
    axisLabelsOverride,

    axis_viewport,
    axis_tropweiv,
    axis_ticks,
    axis_labels,
    axis_grid,

    axis_line_style,
    axis_label_style,
    axis_grid_style,
    axis_label_gap,

) where

import Data.Time
import Data.Fixed
import Data.Maybe
import System.Locale (defaultTimeLocale)
import Control.Monad
import Data.List(sort,intersperse)
import Data.Accessor.Template
import Data.Colour (opaque)
import Data.Colour.Names (black, lightgrey)
import Data.Default

import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Renderable

-- | A typeclass abstracting the functions we need
-- to be able to plot against an axis of type a
class Ord a => PlotValue a where
    toValue  :: a -> Double
    fromValue:: Double -> a
    autoAxis :: AxisFn a

-- | The basic data associated with an axis showing values of type x.
data AxisData x = AxisData {

    -- | The axis_viewport_ function maps values into device coordinates.
    axis_viewport_ :: Range -> x -> Double,

    -- | The axis_tropweiv_ function maps device coordinates back to values.
    axis_tropweiv_ :: Range -> Double -> x,

    -- | The tick marks on the axis as pairs.
    --   The first element is the position on the axis
    --   (in viewport units) and the second element is the
    --   length of the tick in output coordinates.
    --   The tick starts on the axis, and positive numbers are drawn
    --   towards the plot area.
    axis_ticks_    :: [(x,Double)],

    -- | The labels on an axis as pairs. The first element of the pair
    --   is the position on the axis (in viewport units) and the
    --   second is the label text string. Note that multiple sets of
    --   labels can be specified, and are shown successively further
    --   away from the axis line.
    axis_labels_   :: [[(x, String)]],

    -- | The positions on the axis (in viewport units) where
    --   we want to show grid lines.
    axis_grid_     :: [ x ]
}

-- | Control values for how an axis gets displayed.
data AxisStyle = AxisStyle {
    axis_line_style_  :: LineStyle,
    axis_label_style_ :: FontStyle,
    axis_grid_style_  :: LineStyle,

    -- | How far the labels are to be drawn from the axis.
    axis_label_gap_   :: Double
}

-- | A function to generate the axis data, given the data values
--   to be plotted against it.
type AxisFn x = [x] -> AxisData x

-- | Collect the information we need to render an axis. The
--   bool is true if the axis direction is reversed.
data AxisT x = AxisT RectEdge AxisStyle Bool (AxisData x)

-- | Construct a renderable from an axis, in order that
-- it can be composed with other renderables and drawn. This
-- does not include the drawing of the grid, which must be done
-- separately by the `renderAxisGrid` function.
axisToRenderable :: AxisT x -> Renderable x
axisToRenderable at = Renderable {
     minsize = minsizeAxis at,
     render  = renderAxis at
  }

-- | Modifier to remove grid lines from an axis
axisGridHide         :: AxisData x -> AxisData x
axisGridHide ad       = ad{ axis_grid_ = [] }

-- | Modifier to position grid lines to line up with the ticks
axisGridAtTicks      :: AxisData x -> AxisData x
axisGridAtTicks ad    = ad{ axis_grid_ = map fst (axis_ticks_ ad) }

-- | Modifier to position grid lines to line up with only the major ticks
axisGridAtBigTicks   :: AxisData x -> AxisData x
axisGridAtBigTicks ad = ad{ axis_grid_ =
                            map fst $
                            filter ((> minimum (map (abs.snd) (axis_ticks_ ad))).snd) $
                            axis_ticks_ ad }

-- | Modifier to position grid lines to line up with the labels
axisGridAtLabels     :: AxisData x -> AxisData x
axisGridAtLabels ad   = ad{ axis_grid_ = map fst vs }
  where
    vs = case axis_labels_ ad of
        [] -> []
        ls -> head ls

-- | Modifier to remove ticks from an axis
axisTicksHide       :: AxisData x -> AxisData x
axisTicksHide ad     = ad{ axis_ticks_  = [] }

-- | Modifier to remove labels from an axis
axisLabelsHide      :: AxisData x -> AxisData x
axisLabelsHide ad    = ad{ axis_labels_ = []}

-- | Modifier to change labels on an axis
axisLabelsOverride  :: [(x,String)] -> AxisData x -> AxisData x
axisLabelsOverride o ad = ad{ axis_labels_ = [o] }

minsizeAxis :: AxisT x -> ChartBackend RectSize
minsizeAxis (AxisT at as rev ad) = do
    labelSizes <- withFontStyle (axis_label_style_ as) $ do
      mapM (mapM textDimension) (labelTexts ad)

    let ag      = axis_label_gap_ as
    let tsize   = maximum ([0] ++ [ max 0 (-l) | (v,l) <- axis_ticks_ ad ])

    let hw = maximum0 (map (maximum0.map fst) labelSizes)
    let hh = ag + tsize + (sum . intersperse ag . map (maximum0.map snd) $ labelSizes)

    let vw = ag + tsize + (sum . intersperse ag . map (maximum0.map fst) $ labelSizes)
    let vh = maximum0 (map (maximum0.map snd) labelSizes)

    let sz      = case at of
		     E_Top    -> (hw,hh)
		     E_Bottom -> (hw,hh)
		     E_Left   -> (vw,vh)
		     E_Right  -> (vw,vh)
    return sz

labelTexts :: AxisData a -> [[String]]
labelTexts ad = map (map snd) (axis_labels_ ad)

maximum0 [] = 0
maximum0 vs = maximum vs

-- | Calculate the amount by which the labels extend beyond
--   the ends of the axis.
axisOverhang :: (Ord x) => AxisT x -> ChartBackend (Double,Double)
axisOverhang (AxisT at as rev ad) = do
    let labels = map snd . sort . concat . axis_labels_ $ ad
    labelSizes <- withFontStyle (axis_label_style_ as) $ do
      mapM textDimension labels
    case labelSizes of
      []  -> return (0,0)
      ls  -> let l1     = head ls
                 l2     = last ls
                 ohangv = return (snd l1 / 2, snd l2 / 2)
                 ohangh = return (fst l1 / 2, fst l2 / 2)
             in case at of
                 E_Top    -> ohangh
                 E_Bottom -> ohangh
                 E_Left   -> ohangv
                 E_Right  -> ohangh

renderAxis :: AxisT x -> RectSize -> ChartBackend (PickFn x)
renderAxis at@(AxisT et as rev ad) sz = do
  let ls = axis_line_style_ as
  withLineStyle (ls {line_cap_ = LineCapSquare}) $ do
    strokePointPath [Point sx sy,Point ex ey]
  withLineStyle (ls {line_cap_ = LineCapButt}) $ do
    mapM_ drawTick (axis_ticks_ ad)
  withFontStyle (axis_label_style_ as) $ do
    labelSizes <- mapM (mapM textDimension) (labelTexts ad)
    let sizes = map ((+ag).maximum0.map coord) labelSizes
    let offsets = scanl (+) ag sizes
    mapM_ drawLabels (zip offsets  (axis_labels_ ad))
  return pickfn
 where
   (sx,sy,ex,ey,tp,axisPoint,invAxisPoint) = axisMapping at sz

   drawTick (value,length) =
       let t1 = axisPoint value
	   t2 = t1 `pvadd` (vscale length tp)
       in strokePointPath [t1,t2]

   (hta,vta,coord,awayFromAxis) = case et of
       E_Top    -> (HTA_Centre, VTA_Bottom, snd, \v -> (Vector 0 (-v)))
       E_Bottom -> (HTA_Centre, VTA_Top,    snd, \v -> (Vector 0 v))
       E_Left   -> (HTA_Right,  VTA_Centre, fst, \v -> (Vector (-v) 0))
       E_Right  -> (HTA_Left,   VTA_Centre, fst, \v -> (Vector v 0))

   avoidOverlaps labels = do
       rects <- mapM labelDrawRect labels
       return $ map snd . head . filter (noOverlaps . map fst)
              $ map (\n -> eachNth n rects) [0 .. length rects]

   labelDrawRect (value,s) = do
       let pt = axisPoint value `pvadd` (awayFromAxis ag)
       r <- textDrawRect hta vta pt s
       return (hBufferRect r,(value,s))

   drawLabels (offset,labels) = do
        labels' <- avoidOverlaps labels
        mapM_ drawLabel labels'
     where
       drawLabel (value,s) = do
           drawTextA hta vta (axisPoint value `pvadd` (awayFromAxis offset)) s
           textDimension s

   ag = axis_label_gap_ as
   pickfn = Just . invAxisPoint

hBufferRect :: Rect -> Rect
hBufferRect (Rect p (Point x y)) = Rect p $ Point x' y
  where x' = x + w/2
        w = x - (p_x p)

noOverlaps :: [Rect] -> Bool
noOverlaps [] = True
noOverlaps [_] = True
noOverlaps (x:y:l) | rectsOverlap x y = False
                   | otherwise        = noOverlaps (y:l)

rectsOverlap :: Rect -> Rect -> Bool
rectsOverlap (Rect p1 p2) r = any (withinRect r) ps
  where (Point x1 y1) = p1
        (Point x2 y2) = p2
        p3 = Point x1 y2
        p4 = Point x2 y1
        ps = [p1,p2,p3,p4]

eachNth n = skipN
  where
    n' = n - 1
    skipN [] = []
    skipN (x:xs) = x : skipN (drop n' xs)

withinRect :: Rect -> Point -> Bool
withinRect (Rect (Point x1 y1) (Point x2 y2)) (Point x y)
    = and [x >= x1 && x <= x2,
           y >= y1 && y <= y2]

axisMapping :: AxisT z -> RectSize
               -> (Double,Double,Double,Double,Vector,z->Point,Point->z)
axisMapping (AxisT et as rev ad) (x2,y2) = case et of
    E_Top    -> (x1,y2,x2,y2, (Vector 0 1),    mapx y2, imapx)
    E_Bottom -> (x1,y1,x2,y1, (Vector 0 (-1)), mapx y1, imapx)
    E_Left   -> (x2,y2,x2,y1, (Vector (1) 0),  mapy x2, imapy) 
    E_Right  -> (x1,y2,x1,y1, (Vector (-1) 0), mapy x1, imapy)
  where
    (x1,y1) = (0,0)
    xr = reverse (x1,x2)
    yr = reverse (y2,y1)

    mapx y x = Point (axis_viewport_ ad xr x) y
    mapy x y = Point x (axis_viewport_ ad yr y)

    imapx (Point x _) = axis_tropweiv_ ad xr x
    imapy (Point _ y) = axis_tropweiv_ ad yr y

    reverse r@(r0,r1)  = if rev then (r1,r0) else r

-- 
renderAxisGrid :: RectSize -> AxisT z -> ChartBackend ()
renderAxisGrid sz@(w,h) at@(AxisT re as rev ad) = do
    withLineStyle (axis_grid_style_ as) $ do
      mapM_ (drawGridLine re) (axis_grid_ ad)
  where
    (sx,sy,ex,ey,tp,axisPoint,invAxisPoint) = axisMapping at sz

    drawGridLine E_Top    = vline
    drawGridLine E_Bottom = vline
    drawGridLine E_Left   = hline
    drawGridLine E_Right  = hline

    vline v = let v' = p_x (axisPoint v)
	      in strokePointPath [Point v' 0,Point v' h]

    hline v = let v' = p_y (axisPoint v)
	      in strokePointPath [Point 0 v',Point w v']


-- | Construct an axis given the positions for ticks, grid lines, and 
-- labels, and the labelling function
makeAxis :: PlotValue x => (x -> String) -> ([x],[x],[x]) -> AxisData x
makeAxis labelf (labelvs, tickvs, gridvs) = AxisData {
    axis_viewport_ = newViewport,
    axis_tropweiv_ = newTropweiv,
    axis_ticks_    = newTicks,
    axis_grid_     = gridvs,
    axis_labels_   = [newLabels]
    }
  where
    newViewport = vmap (min',max')
    newTropweiv = invmap (min',max')
    newTicks    = [ (v,2)        | v <- tickvs  ] ++ [ (v,5) | v <- labelvs ]
    newLabels   = [ (v,labelf v) | v <- labelvs ]
    min'        = minimum labelvs
    max'        = maximum labelvs

-- | Construct an axis given the positions for ticks, grid lines, and 
-- labels, and the positioning and labelling functions
makeAxis' :: Ord x => (x -> Double) -> (Double -> x) -> (x -> String)
                   -> ([x],[x],[x]) -> AxisData x
makeAxis' t f labelf (labelvs, tickvs, gridvs) = AxisData {
    axis_viewport_ = linMap t (minimum labelvs, maximum labelvs),
    axis_tropweiv_ = invLinMap f t (minimum labelvs, maximum labelvs),
    axis_ticks_    = zip tickvs (repeat 2)  ++  zip labelvs (repeat 5),
    axis_grid_     = gridvs,
    axis_labels_   = [[ (v,labelf v) | v <- labelvs ]]
    }


----------------------------------------------------------------------

defaultAxisLineStyle :: LineStyle
defaultAxisLineStyle = solidLine 1 $ opaque black

defaultGridLineStyle :: LineStyle
defaultGridLineStyle = dashedLine 1 [5,5] $ opaque lightgrey

{-# DEPRECATED defaultAxisStyle "Use the according Data.Default instance!" #-}
defaultAxisStyle :: AxisStyle
defaultAxisStyle = def

instance Default AxisStyle where
  def = AxisStyle 
    { axis_line_style_  = defaultAxisLineStyle
    , axis_label_style_ = def
    , axis_grid_style_  = defaultGridLineStyle
    , axis_label_gap_   = 10
    }

----------------------------------------------------------------------

-- | A linear mapping of points in one range to another.
vmap :: PlotValue x => (x,x) -> Range -> x -> Double
vmap (v1,v2) (v3,v4) v = v3 + (toValue v - toValue v1) * (v4-v3)
                              / (toValue v2 - toValue v1)

-- | The inverse mapping from device co-ordinate range back to
--   interesting values.
invmap :: PlotValue x => (x,x) -> Range -> Double -> x
invmap (v3,v4) (d1,d2) d = fromValue (toValue v3 + ( (d-d1) * doubleRange
                                                   / (d2-d1) ))
    where doubleRange = toValue v4 - toValue v3

-- | A linear mapping of points in one range to another.
linMap :: (a -> Double) -> (a,a) -> Range -> a -> Double
linMap f (x1,x2) (d1,d2) x =
    d1 + (d2 - d1) * (f x - f x1) / (f x2 - f x1)

-- | An inverse linear mapping of points from one range to another.
invLinMap :: (Double -> a) -> (a -> Double) -> (a,a) -> Range -> Double -> a
invLinMap f t (v3,v4) (d1,d2) d =
    f (t v3 + ( (d-d1) * doubleRange / (d2-d1) ))
  where
    doubleRange = t v4 - t v3

----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor for
-- each field.
$( deriveAccessors ''AxisData )
$( deriveAccessors ''AxisStyle )

