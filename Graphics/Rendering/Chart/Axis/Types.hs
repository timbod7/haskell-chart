-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Axis.Types
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Type definitions for Axes
--
-- Note that template haskell is used to derive accessor functions
-- (see 'Data.Accessor') for each field of the following data types:
--
--     * 'AxisData'
--
--     * 'AxisStyle'
--
-- These accessors are not shown in this API documentation.  They have
-- the same name as the field, but with the trailing underscore
-- dropped. Hence for data field f_::F in type D, they have type
--
-- @
--   f :: Data.Accessor.Accessor D F
-- @
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -XTemplateHaskell #-}

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
    axisGridAtLabels,
    axisGridHide,
    axisTicksHide,
    axisLabelsHide,

    axis_viewport,
    axis_tropweiv,
    axis_ticks,
    axis_labels,
    axis_context,
    axis_grid,

    axis_line_style,
    axis_label_style,
    axis_grid_style,
    axis_label_gap,

) where

import qualified Graphics.Rendering.Cairo as C
import Data.Time
import Data.Fixed
import System.Locale (defaultTimeLocale)
import Control.Monad
import Data.List(sort)
import Data.Accessor.Template
import Data.Colour (opaque)
import Data.Colour.Names (black, lightgrey)

import Graphics.Rendering.Chart.Types
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

    -- | The labels on an axis as pairs. The first element
    --   is the position on the axis (in viewport units) and
    --   the second is the label text string.
    axis_labels_   :: [ (x, String) ],

    -- | A secondary set of labels on an axis.
    --   Used to display context as well as detail, e.g. for time series,
    --   where two levels of navigational aid can help orient the user.
    axis_context_  :: [ (x, String) ],

    -- | The positions on the axis (in viewport units) where
    --   we want to show grid lines.
    axis_grid_     :: [ x ]
}

-- | Control values for how an axis gets displayed.
data AxisStyle = AxisStyle {
    axis_line_style_  :: CairoLineStyle,
    axis_label_style_ :: CairoFontStyle,
    axis_grid_style_  :: CairoLineStyle,

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
axisGridHide        :: AxisData x -> AxisData x
axisGridHide ad      = ad{ axis_grid_   = [] }

-- | Modifier to position grid lines to line up with the ticks
axisGridAtTicks     :: AxisData x -> AxisData x
axisGridAtTicks ad   = ad{ axis_grid_   = map fst (axis_ticks_ ad) }

-- | Modifier to position grid lines to line up with the labels
axisGridAtLabels    :: AxisData x -> AxisData x
axisGridAtLabels ad  = ad{ axis_grid_   = map fst (axis_labels_ ad) }

-- | Modifier to remove ticks from an axis
axisTicksHide       :: AxisData x -> AxisData x
axisTicksHide ad     = ad{ axis_ticks_  = [] }

-- | Modifier to remove labels from an axis
axisLabelsHide      :: AxisData x -> AxisData x
axisLabelsHide ad    = ad{ axis_labels_ = [], axis_context_ = [] }

minsizeAxis :: AxisT x -> CRender RectSize
minsizeAxis (AxisT at as rev ad) = do
    let labels = map snd (axis_labels_ ad)
    labelSizes <- preserveCState $ do
        setFontStyle (axis_label_style_ as)
        mapM textSize labels
    let context = map snd (axis_context_ ad)
    contextSizes <- preserveCState $ do
        setFontStyle (axis_label_style_ as)
        mapM textSize context
    let (lw,lh) = foldl maxsz (0,0) labelSizes
    let (cw,ch) = foldl maxsz (0,0) contextSizes
    let ag      = axis_label_gap_ as
    let tsize   = maximum ([0] ++ [ max 0 (-l) | (v,l) <- axis_ticks_ ad ])
    let sz      = case at of
		     E_Top    -> (lw,max (addTwoIfNZ lh ch ag) tsize)
		     E_Bottom -> (lw,max (addTwoIfNZ lh ch ag) tsize)
		     E_Left   -> (max (addTwoIfNZ lw cw ag) tsize, lh)
		     E_Right  -> (max (addTwoIfNZ lw cw ag) tsize, lh)
    return sz

  where
    maxsz (w1,h1) (w2,h2)   = (max w1 w2, max h1 h2)
    addIfNZ a b | a == 0    = 0
                | otherwise = a+b
    addTwoIfNZ a b gap | a == 0    = addIfNZ b gap
                       | otherwise = a + gap + addIfNZ b gap


-- | Calculate the amount by which the labels extend beyond
--   the ends of the axis.
axisOverhang :: Ord x => AxisT x -> CRender (Double,Double)
axisOverhang (AxisT at as rev ad) = do
    let labels = map snd (sort (axis_labels_ ad ++ axis_context_ ad))
    labelSizes <- preserveCState $ do
        setFontStyle (axis_label_style_ as)
        mapM textSize labels
    case labelSizes of
        []  -> return (0,0)
	ls  -> let l1     = head ls
		   l2     = last ls
		   ohangv = return (snd l1 / 2, snd l2 / 2)
		   ohangh = return (fst l1 / 2, fst l2 / 2)
		   in
		   case at of
		       E_Top    -> ohangh
		       E_Bottom -> ohangh
		       E_Left   -> ohangv
		       E_Right  -> ohangh

renderAxis :: AxisT x -> RectSize -> CRender (PickFn x)
renderAxis at@(AxisT et as rev ad) sz = do
   let ls = axis_line_style_ as
   preserveCState $ do
       setLineStyle ls{line_cap_=C.LineCapSquare}
       strokePath [Point sx sy,Point ex ey]
   preserveCState $ do
       setLineStyle ls{line_cap_=C.LineCapButt}
       mapM_ drawTick (axis_ticks_ ad)
   sizes <- preserveCState $ do
       setFontStyle (axis_label_style_ as)
       mapM drawLabel (axis_labels_ ad)
   preserveCState $ do
       setFontStyle (axis_label_style_ as)
       mapM_ (drawContext (avoid sizes)) (axis_context_ ad)
   return pickfn
 where
   (sx,sy,ex,ey,tp,axisPoint,invAxisPoint) = axisMapping at sz

   drawTick (value,length) =
       let t1 = axisPoint value
	   t2 = t1 `pvadd` (vscale length tp)
       in strokePath [t1,t2]

   (hta,vta,lp) =
       let g = axis_label_gap_ as
       in case et of
              E_Top    -> (HTA_Centre, VTA_Bottom, (Vector 0 (-g)))
              E_Bottom -> (HTA_Centre, VTA_Top,    (Vector 0 g))
              E_Left   -> (HTA_Right,  VTA_Centre, (Vector (-g) 0))
              E_Right  -> (HTA_Left,   VTA_Centre, (Vector g 0))

   avoid minorlabels =
       let (lw,lh) = foldl maxsz (0,0) minorlabels
           maxsz (w1,h1) (w2,h2)   = (max w1 w2, max h1 h2)
       in case et of
              E_Top    -> (Vector 0 (-lh))
              E_Bottom -> (Vector 0    lh)
              E_Left   -> (Vector (-lw) 0)
              E_Right  -> (Vector lw    0)

   drawLabel (value,s) = do
       drawText hta vta (axisPoint value `pvadd` lp) s
       textSize s

   drawContext minor (value,s) = do
       drawText hta vta (axisPoint value `pvadd` minor `pvadd` lp) s

   pickfn = Just . invAxisPoint

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
renderAxisGrid :: RectSize -> AxisT z -> CRender ()
renderAxisGrid sz@(w,h) at@(AxisT re as rev ad) = do
    preserveCState $ do
        setLineStyle (axis_grid_style_ as)
        mapM_ (drawGridLine re) (axis_grid_ ad)
  where
    (sx,sy,ex,ey,tp,axisPoint,invAxisPoint) = axisMapping at sz

    drawGridLine E_Top    = vline
    drawGridLine E_Bottom = vline
    drawGridLine E_Left   = hline
    drawGridLine E_Right  = hline

    vline v = let v' = p_x (axisPoint v)
	      in strokePath [Point v' 0,Point v' h]

    hline v = let v' = p_y (axisPoint v)
	      in strokePath [Point 0 v',Point w v']


-- | Construct an axis given the positions for ticks, grid lines, and 
-- labels, and the labelling function
makeAxis :: PlotValue x => (x -> String) -> ([x],[x],[x]) -> AxisData x
makeAxis labelf (labelvs, tickvs, gridvs) = AxisData {
    axis_viewport_ = newViewport,
    axis_tropweiv_ = newTropweiv,
    axis_ticks_    = newTicks,
    axis_grid_     = gridvs,
    axis_labels_   = newLabels,
    axis_context_  = []
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
    axis_labels_   = [ (v,labelf v) | v <- labelvs ],
    axis_context_  = []
    }



----------------------------------------------------------------------

defaultAxisLineStyle :: CairoLineStyle
defaultAxisLineStyle = solidLine 1 $ opaque black

defaultGridLineStyle :: CairoLineStyle
defaultGridLineStyle = dashedLine 1 [5,5] $ opaque lightgrey

defaultAxisStyle :: AxisStyle
defaultAxisStyle = AxisStyle {
    axis_line_style_  = defaultAxisLineStyle,
    axis_label_style_ = defaultFontStyle,
    axis_grid_style_  = defaultGridLineStyle,
    axis_label_gap_   = 10
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

