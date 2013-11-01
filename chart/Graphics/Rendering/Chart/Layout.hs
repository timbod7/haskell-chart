-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Layout
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- This module glues together axes and plots to actually create a renderable
-- for a chart.
--
-- Note that Template haskell is used to derive accessor functions
-- (see 'Control.Lens') for each field of the following data types:
--
--     * 'Layout'
--     
--     * 'LayoutLR'
-- 
--     * 'StackedLayouts'
--
--     * 'LayoutAxis'
--
-- These accessors are not shown in this API documentation.  They have
-- the same name as the field, but with the leading underscore
-- dropped. Hence for data field _f::F in type D, they have type
--
-- @
--   f :: Control.Lens.Lens' D F
-- @
--
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Rendering.Chart.Layout
  ( Layout(..)
  , LayoutLR(..)
  , LayoutAxis(..)
  , LayoutPick(..)
  , StackedLayouts(..)
  , StackedLayout(..)
  , MAxisFn
  
  , layoutToRenderable
  , layoutLRToRenderable

  , setLayoutForeground
  , updateAllAxesStyles
  , setLayoutLRForeground
  , updateAllAxesStylesLR

  , defaultLayoutAxis
  , laxis_title_style
  , laxis_title
  , laxis_style
  , laxis_generate
  , laxis_override
  , laxis_reverse
    
  , layout_background
  , layout_plot_background
  , layout_title
  , layout_title_style
  , layout_x_axis
  , layout_top_axis_visibility
  , layout_bottom_axis_visibility
  , layout_y_axis
  , layout_left_axis_visibility
  , layout_right_axis_visibility
  , layout_margin
  , layout_plots
  , layout_legend
  , layout_grid_last
      
  , layoutlr_background
  , layoutlr_plot_background
  , layoutlr_title
  , layoutlr_title_style
  , layoutlr_x_axis
  , layoutlr_top_axis_visibility
  , layoutlr_bottom_axis_visibility
  , layoutlr_left_axis
  , layoutlr_right_axis
  , layoutlr_left_axis_visibility
  , layoutlr_right_axis_visibility
  , layoutlr_plots
  , layoutlr_legend
  , layoutlr_margin
  , layoutlr_grid_last

  , defaultStackedLayouts
  , slayouts_layouts
  , slayouts_compress_legend

  , renderStackedLayouts
  ) where

import Graphics.Rendering.Chart.Axis
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Utils
import Graphics.Rendering.Chart.Plot
import Graphics.Rendering.Chart.Legend
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Grid
import Control.Monad
import Control.Monad.Reader (local)
import Control.Lens
import Data.Colour
import Data.Colour.Names (white)
import Data.Default.Class

-- | A @MAxisFn@ is a function that generates an (optional) axis
--   given the points plotted against that axis.
type MAxisFn t = [t] -> Maybe (AxisData t)

-- | Type of axis that is used in 'Layout' and 'LayoutLR'.
--   
--   To generate the actual axis type ('AxisData' and 'AxisT')
--   the '_laxis_generate' function is called and custom settings
--   are applied with '_laxis_override'. Note that the 'AxisVisibility'
--   values in 'Layout' and 'LayoutLR' override visibility related 
--   settings of the axis.
data LayoutAxis x = LayoutAxis
  { _laxis_title_style :: FontStyle
    -- ^ Font style to use for the axis title.
  , _laxis_title       :: String
    -- ^ Title displayed for the axis.
  , _laxis_style       :: AxisStyle
    -- ^ Axis style applied.

  , _laxis_generate    :: AxisFn x
    -- ^ Function that generates the axis data, based upon the
    --   points plotted. The default value is 'autoAxis'.
  
  , _laxis_override    :: AxisData x -> AxisData x
    -- ^ Function that can be used to override the generated axis data.
    --   The default value is 'id'.
  
  , _laxis_reverse     :: Bool
    -- ^ True if left to right (bottom to top) is to show descending values.
  
  }

-- | Information on what is at a specifc location of a 'Layout' or 'LayoutLR'.
--   This is delivered by the 'PickFn' of a 'Renderable'.
data LayoutPick x y1 y2 = LayoutPick_Legend String -- ^ A legend entry.
                          | LayoutPick_Title String  -- ^ The title.
                          | LayoutPick_XTopAxisTitle String      -- ^ The title of the top x axis.
                          | LayoutPick_XBottomAxisTitle String   -- ^ The title of the bottom x axis.
                          | LayoutPick_YLeftAxisTitle String  -- ^ The title of the left y axis.
                          | LayoutPick_YRightAxisTitle String -- ^ The title of the right y axis.
                          | LayoutPick_PlotArea x y1 y2 -- ^ The plot area at the given plot coordinates.
                          | LayoutPick_XTopAxis x       -- ^ The top x axis at the given plot coordinate.
                          | LayoutPick_XBottomAxis x    -- ^ The bottom x axis at the given plot coordinate.
                          | LayoutPick_YLeftAxis y1  -- ^ The left y axis at the given plot coordinate.
                          | LayoutPick_YRightAxis y2 -- ^ The right y axis at the given plot coordinate.
                          deriving (Show)

type LegendItem = (String,Rect -> ChartBackend ())

-- | A Layout value is a single plot area, with single x and y
--   axis. The title is at the top and the legend at the bottom. It's
--   parametrized by the types of values to be plotted on the x
--   and y axes.
data Layout x y = Layout 
  { _layout_background      :: FillStyle
    -- ^ How to fill the background of everything.
  , _layout_plot_background :: Maybe FillStyle
    -- ^ How to fill the background of the plot, 
    --   if different from the overall background.

  , _layout_title           :: String
    -- ^ Title to display above the chart.
  , _layout_title_style     :: FontStyle
    -- ^ Font style to use for the title.

  , _layout_x_axis                 :: LayoutAxis x
    -- ^ Rules to generate the x axis.
  , _layout_top_axis_visibility    :: AxisVisibility
    -- ^ Visibility options for the top axis.
  , _layout_bottom_axis_visibility :: AxisVisibility
    -- ^ Visibility options for the bottom axis.

  , _layout_y_axis                :: LayoutAxis y
    -- ^ Rules to generate the y axis.
  , _layout_left_axis_visibility  :: AxisVisibility
    -- ^ Visibility options for the left axis.
  , _layout_right_axis_visibility :: AxisVisibility
    -- ^ Visibility options for the right axis.

  , _layout_plots           :: [Plot x y]
    -- ^ The data sets to plot in the chart.
    --   The are ploted over each other.

  , _layout_legend          :: Maybe LegendStyle
    -- ^ How to style the legend.
  , _layout_margin          :: Double
    -- ^ The margin distance to use.
  , _layout_grid_last       :: Bool
    -- ^ If the grid shall be rendered
    --   beneath (@False@) or over (@True@) all plots.
  }

instance (Ord x, Ord y) => ToRenderable (Layout x y) where
  toRenderable = setPickFn nullPickFn . layoutToRenderable

-- | Render the given 'Layout'.
layoutToRenderable :: forall x y . (Ord x, Ord y) => Layout x y -> Renderable (LayoutPick x y y)
layoutToRenderable l = fillBackground (_layout_background l) 
                     $ gridToRenderable (layoutToGrid l)
  where
    layoutToGrid l = aboveN
           [  tval $ titleToRenderable (_layout_margin l) (_layout_title_style l) (_layout_title l)
           ,  weights (1,1) $ tval $ gridToRenderable $
                  addMarginsToGrid (lm,lm,lm,lm) (layoutPlotAreaToGrid l)
           ,  tval $ renderLegend l (getLegendItems l)
           ]

    lm = _layout_margin l
  
getLayoutXVals :: Layout x y -> [x]
getLayoutXVals l = concatMap (fst . _plot_all_points) (_layout_plots l)

-- | Extract all 'LegendItem's from the plots of a 'Layout'.
getLegendItems :: Layout x y -> [LegendItem]
getLegendItems l = concat [ _plot_legend p | p <- _layout_plots l ]

-- | Render the given 'LegendItem's for a 'Layout'.
renderLegend :: Layout x y -> [LegendItem] -> Renderable (LayoutPick x y y)
renderLegend l legItems = gridToRenderable g
  where
    g      = besideN [ tval $ mkLegend (_layout_legend l) (_layout_margin l) legItems
                     , weights (1,1) $ tval $ emptyRenderable ]

-- | Render the plot area of a 'Layout'. This consists of the 
--   actual plot area with all plots, the axis and their titles.
layoutPlotAreaToGrid :: forall x y. (Ord x, Ord y) =>
                        Layout x y -> Grid (Renderable (LayoutPick x y y))
layoutPlotAreaToGrid l = buildGrid LayoutGridElements{
  lge_plots = mfill (_layout_plot_background l) $ plotsToRenderable l,
  lge_taxis = (tAxis,_laxis_title $ _layout_x_axis l, _laxis_title_style $ _layout_x_axis l),
  lge_baxis = (bAxis,_laxis_title $ _layout_x_axis l, _laxis_title_style $ _layout_x_axis l),
  lge_laxis = (lAxis,_laxis_title $ _layout_y_axis l, _laxis_title_style $ _layout_y_axis l),
  lge_raxis = (rAxis,"", def),
  lge_margin = _layout_margin l
  }
  where
    xvals = [ x | p <- (_layout_plots l), x <- fst $ _plot_all_points p]
    yvals = [ y | p <- (_layout_plots l), y <- snd $ _plot_all_points p]

    bAxis = mkAxis E_Bottom (overrideAxisVisibility l _layout_x_axis _layout_bottom_axis_visibility) xvals
    tAxis = mkAxis E_Top    (overrideAxisVisibility l _layout_x_axis _layout_top_axis_visibility   ) xvals
    lAxis = mkAxis E_Left   (overrideAxisVisibility l _layout_y_axis _layout_left_axis_visibility  ) yvals
    rAxis = mkAxis E_Right  (overrideAxisVisibility l _layout_y_axis _layout_right_axis_visibility ) yvals
    axes = (bAxis,lAxis,tAxis,rAxis)

    plotsToRenderable l = Renderable {
        minsize = return (0,0),
        render  = renderPlots l
    }

    -- | Render the plots of a 'Layout' to a plot area of given size.
    renderPlots :: Layout x y -> RectSize -> ChartBackend (PickFn (LayoutPick x y y))
    renderPlots l sz@(w,h) = do
        when (not (_layout_grid_last l)) (renderGrids sz axes)
        withClipRegion (Rect (Point 0 0) (Point w h)) $ do
          mapM_ rPlot (_layout_plots l)
        when (_layout_grid_last l) (renderGrids sz axes)
        return pickfn
      where
        rPlot p = renderSinglePlot sz bAxis lAxis p

        xr = (0, w)
        yr = (h, 0)

        pickfn :: PickFn (LayoutPick x y y)
        pickfn (Point x y) = do  -- Maybe monad
            xat <- mxat
            yat <- myat
            return (LayoutPick_PlotArea (mapx xat x) (mapy yat y) (mapy yat y))
          where
            mxat = case (bAxis,tAxis) of
                (Just at,_)       -> Just at
                (_,Just at)       -> Just at
                (Nothing,Nothing) -> Nothing
            myat = case (lAxis,rAxis) of
                (Just at,_)   -> Just at
                (_,Just at)   -> Just at
                (Nothing,Nothing)   -> Nothing
            mapx (AxisT _ _ rev ad) x = _axis_tropweiv ad (optPairReverse rev xr) x
            mapy (AxisT _ _ rev ad) y = _axis_tropweiv ad (optPairReverse rev yr) y

-- | Empty 'Layout' without title and plots. The background is white and 
--   the grid is drawn beneath all plots. There will be a legend. The top
--   and right axis will not be visible.
instance (PlotValue x, PlotValue y) => Default (Layout x y) where
  def = Layout
    { _layout_background      = solidFillStyle $ opaque white
    , _layout_plot_background = Nothing

    , _layout_title           = ""
    , _layout_title_style     = def { _font_size   = 15
                                    , _font_weight = FontWeightBold }
    
    , _layout_x_axis                 = def
    , _layout_top_axis_visibility    = def { _axis_show_line   = False
                                           , _axis_show_ticks  = False
                                           , _axis_show_labels = False }
    , _layout_bottom_axis_visibility = def
    , _layout_y_axis                 = def
    , _layout_left_axis_visibility   = def
    , _layout_right_axis_visibility  = def { _axis_show_line   = False
                                           , _axis_show_ticks  = False
                                           , _axis_show_labels = False }

    , _layout_margin          = 10
    , _layout_plots           = []
    , _layout_legend          = Just def
    , _layout_grid_last       = False
    }

----------------------------------------------------------------------
  
-- | A LayoutLR value is a single plot area, with an x axis and
--   independent left and right y axes, with a title at the top;
--   legend at the bottom. It's parametrized by the types of values
--   to be plotted on the x and two y axes.
data LayoutLR x y1 y2 = LayoutLR 
  { _layoutlr_background      :: FillStyle
    -- ^ How to fill the background of everything.
  , _layoutlr_plot_background :: Maybe FillStyle
    -- ^ How to fill the background of the plot, 
    --   if different from the overall background.

  , _layoutlr_title           :: String
    -- ^ Title to display above the chart.
  , _layoutlr_title_style     :: FontStyle
    -- ^ Font style to use for the title.

  , _layoutlr_x_axis                 :: LayoutAxis x
    -- ^ Rules to generate the x axis.
  , _layoutlr_top_axis_visibility    :: AxisVisibility
    -- ^ Visibility options for the top axis.
  , _layoutlr_bottom_axis_visibility :: AxisVisibility
    -- ^ Visibility options for the bottom axis.

  , _layoutlr_left_axis             :: LayoutAxis y1
    -- ^ Rules to generate the left y axis.
  , _layoutlr_left_axis_visibility  :: AxisVisibility
    -- ^ Visibility options for the left axis.
  , _layoutlr_right_axis            :: LayoutAxis y2
    -- ^ Rules to generate the right y axis.
  , _layoutlr_right_axis_visibility :: AxisVisibility
    -- ^ Visibility options for the right axis.
  
  , _layoutlr_plots      :: [Either (Plot x y1) (Plot x y2)]
    -- ^ The data sets to plot in the chart.
    --   The are ploted over each other.
    --   The either type associates the plot with the
    --   left or right y axis.

  , _layoutlr_legend          :: Maybe LegendStyle
    -- ^ How to style the legend.
  , _layoutlr_margin          :: Double
    -- ^ The margin distance to use.
  , _layoutlr_grid_last       :: Bool
    -- ^ If the grid shall be rendered
    --   beneath (@False@) or over (@True@) all plots.
  }

instance (Ord x, Ord yl, Ord yr) => ToRenderable (LayoutLR x yl yr) where
  toRenderable = setPickFn nullPickFn . layoutLRToRenderable

-- | Render the given 'LayoutLR'.
layoutLRToRenderable :: forall x yl yr . (Ord x, Ord yl, Ord yr) 
                     => LayoutLR x yl yr -> Renderable (LayoutPick x yl yr)
layoutLRToRenderable l = fillBackground (_layoutlr_background l) 
                       $ gridToRenderable (layoutLRToGrid l)
  where
    layoutLRToGrid l = aboveN
           [  tval $ titleToRenderable (_layoutlr_margin l) (_layoutlr_title_style l) (_layoutlr_title l)
           ,  weights (1,1) $ tval $ gridToRenderable $
                  addMarginsToGrid (lm,lm,lm,lm) (layoutLRPlotAreaToGrid l)
           ,  tval $ renderLegendLR l (getLegendItemsLR l)
           ]

    lm = _layoutlr_margin l

getLayoutLRXVals :: LayoutLR x yl yr -> [x]
getLayoutLRXVals l = concatMap deEither $ _layoutlr_plots l
  where
    deEither :: Either (Plot x yl) (Plot x yr) -> [x]
    deEither (Left x)  = fst $ _plot_all_points x
    deEither (Right x) = fst $ _plot_all_points x

-- | Extract all 'LegendItem's from the plots of a 'LayoutLR'.
--   Left and right plot legend items are still separated.
getLegendItemsLR :: LayoutLR x yl yr -> ([LegendItem],[LegendItem])
getLegendItemsLR l = (
    concat [ _plot_legend p | (Left p ) <- (_layoutlr_plots l) ],
    concat [ _plot_legend p | (Right p) <- (_layoutlr_plots l) ]
    )

-- | Render the given 'LegendItem's for a 'LayoutLR'.
renderLegendLR :: LayoutLR x yl yr -> ([LegendItem],[LegendItem]) -> Renderable (LayoutPick x yl yr)
renderLegendLR l (lefts,rights) = gridToRenderable g
  where
    g      = besideN [ tval $ mkLegend (_layoutlr_legend l) (_layoutlr_margin l) lefts
                     , weights (1,1) $ tval $ emptyRenderable
                     , tval $ mkLegend (_layoutlr_legend l) (_layoutlr_margin l) rights ]
    lm     = _layoutlr_margin l

layoutLRPlotAreaToGrid :: forall x yl yr. (Ord x, Ord yl, Ord yr) 
                       => LayoutLR x yl yr 
                       -> Grid (Renderable (LayoutPick x yl yr))
layoutLRPlotAreaToGrid l = buildGrid LayoutGridElements{
  lge_plots = mfill (_layoutlr_plot_background l) $ plotsToRenderable l,
  lge_taxis = (tAxis,_laxis_title $ _layoutlr_x_axis l, _laxis_title_style $ _layoutlr_x_axis l),
  lge_baxis = (bAxis,_laxis_title $ _layoutlr_x_axis l, _laxis_title_style $ _layoutlr_x_axis l),
  lge_laxis = (lAxis,_laxis_title $ _layoutlr_left_axis l, _laxis_title_style $ _layoutlr_left_axis l),
  lge_raxis = (rAxis,_laxis_title $ _layoutlr_right_axis l, _laxis_title_style $ _layoutlr_right_axis l),
  lge_margin = _layoutlr_margin l
  }
  where
    xvals =  [ x | (Left p)  <- _layoutlr_plots l, x <- fst $ _plot_all_points p]
          ++ [ x | (Right p) <- _layoutlr_plots l, x <- fst $ _plot_all_points p]
    yvalsL = [ y | (Left p)  <- _layoutlr_plots l, y <- snd $ _plot_all_points p]
    yvalsR = [ y | (Right p) <- _layoutlr_plots l, y <- snd $ _plot_all_points p]
    
    bAxis = mkAxis E_Bottom (overrideAxisVisibility l _layoutlr_x_axis _layoutlr_bottom_axis_visibility) xvals
    tAxis = mkAxis E_Top    (overrideAxisVisibility l _layoutlr_x_axis _layoutlr_top_axis_visibility   ) xvals
    lAxis = mkAxis E_Left   (overrideAxisVisibility l _layoutlr_left_axis  _layoutlr_left_axis_visibility ) yvalsL
    rAxis = mkAxis E_Right  (overrideAxisVisibility l _layoutlr_right_axis _layoutlr_right_axis_visibility) yvalsR
    axes = (bAxis,lAxis,tAxis,rAxis)

    plotsToRenderable l = Renderable {
        minsize = return (0,0),
        render  = renderPlots l
    }

    renderPlots :: LayoutLR x yl yr -> RectSize -> ChartBackend (PickFn (LayoutPick x yl yr))
    renderPlots l sz@(w,h) = do
        when (not (_layoutlr_grid_last l)) (renderGrids sz axes)
        withClipRegion (Rect (Point 0 0) (Point w h)) $ do
          mapM_ rPlot (_layoutlr_plots l)
        when (_layoutlr_grid_last l) (renderGrids sz axes)
        return pickfn
      where
        rPlot (Left  p) = renderSinglePlot sz bAxis lAxis p
        rPlot (Right p) = renderSinglePlot sz bAxis rAxis p

        xr = (0, w)
        yr = (h, 0)

        pickfn (Point x y) = do  -- Maybe monad
            xat <- mxat
            (yatL,yatR) <- myats
            return (LayoutPick_PlotArea (mapx xat x) (mapy yatL y) (mapy yatR y))
          where
            mxat = case (bAxis,tAxis) of
                (Just at,_)       -> Just at
                (_,Just at)       -> Just at
                (Nothing,Nothing) -> Nothing
            myats = case (lAxis,rAxis) of
                (Just at1,Just at2) -> Just (at1,at2)
                (_,_)   -> Nothing
            mapx (AxisT _ _ rev ad) x = _axis_tropweiv ad (optPairReverse rev xr) x
            mapy (AxisT _ _ rev ad) y = _axis_tropweiv ad (optPairReverse rev yr) y

----------------------------------------------------------------------

-- | A layout with its y type hidden, so that it can be stacked
--   with other layouts with differing y axis, but the same x axis.
--   See 'StackedLayouts'.
data StackedLayout x = forall y     . (Ord y)          => StackedLayout (Layout x y)
                       -- ^ A 'Layout' to stack.
                     | forall yl yr . (Ord yl, Ord yr) => StackedLayoutLR (LayoutLR x yl yr)
                       -- ^ A 'LayoutLR' to stack.

-- | A container for a set of vertically 'StackedLayout's.
--   The x axis of the different layouts will be aligned.
data StackedLayouts x = StackedLayouts 
  { _slayouts_layouts :: [StackedLayout x]
    -- ^ The stacked layouts from top (first element) to bottom (last element).
  , _slayouts_compress_legend :: Bool
    -- ^ If the different legends shall be combined in one legend at the bottom.
  }

{-# DEPRECATED defaultStackedLayouts  "Use the according Data.Default instance!" #-}
defaultStackedLayouts :: StackedLayouts x
defaultStackedLayouts = def

-- | A empty 'StackedLayout' with compressions applied.
instance Default (StackedLayouts x) where
  def = StackedLayouts [] True

-- | Render several layouts with the same x-axis type and range,
--   vertically stacked so that their origins and x-values are aligned.
--
--   The legends from all the charts may be optionally combined, and shown
--   once on the bottom chart. See 'StackedLayouts' for further information.
renderStackedLayouts :: forall x. (Ord x) => StackedLayouts x -> Renderable ()
renderStackedLayouts (StackedLayouts{_slayouts_layouts=[]}) = emptyRenderable
renderStackedLayouts slp@(StackedLayouts{_slayouts_layouts=sls@(sl1:_)}) = gridToRenderable g
  where
    g = fullOverlayUnder (fillBackground bg emptyRenderable)
      $ foldr (above.mkGrid) nullt (zip sls [0,1..])
    
    mkGrid :: (StackedLayout x, Int) -> Grid (Renderable ())
    mkGrid (sl, i)
        = titleR
          `wideAbove`
          (addMarginsToGrid (lm,lm,lm,lm) $ mkPlotArea usedAxis)
          `aboveWide`
          (if showLegend then legendR else emptyRenderable)
      where
        titleR = case sl of
                   StackedLayout l -> noPickFn $ titleToRenderable (_layout_margin l) (_layout_title_style l) (_layout_title l)
                   StackedLayoutLR l -> noPickFn $ titleToRenderable (_layoutlr_margin l) (_layoutlr_title_style l) (_layoutlr_title l)
        legendR = case sl of
                    StackedLayout l -> noPickFn $ renderLegend l $ fst legenditems
                    StackedLayoutLR l -> noPickFn $ renderLegendLR l legenditems
        
        legenditems = case (_slayouts_compress_legend slp,isBottomPlot) of
            (False,_) -> case sl of
                           StackedLayout l -> (getLegendItems l, [])
                           StackedLayoutLR l -> getLegendItemsLR l
            (True,True) -> allLegendItems
            (True,False) -> ([],[])
        
        mkPlotArea :: LayoutAxis x -> Grid (Renderable ())
        mkPlotArea axis = case sl of
          StackedLayout l -> fmap noPickFn 
                           $ layoutPlotAreaToGrid 
                           $ l { _layout_x_axis = axis }
          StackedLayoutLR l -> fmap noPickFn 
                             $ layoutLRPlotAreaToGrid 
                             $ l { _layoutlr_x_axis = axis }

        showLegend = not (null (fst legenditems)) || not (null (snd legenditems))

        isBottomPlot = i == length sls - 1

        lm = case sl of
          StackedLayout l -> _layout_margin l
          StackedLayoutLR l -> _layoutlr_margin l
        
        xAxis :: LayoutAxis x
        xAxis = case sl of
          StackedLayout l -> _layout_x_axis l
          StackedLayoutLR l -> _layoutlr_x_axis l
        
        usedAxis :: LayoutAxis x
        usedAxis = xAxis 
          { _laxis_generate = const (_laxis_generate xAxis all_xvals) }
        
    bg = case sl1 of
           StackedLayout l -> _layout_background l
           StackedLayoutLR l -> _layoutlr_background l
    
    getXVals :: StackedLayout x -> [x]
    getXVals (StackedLayout l) = getLayoutXVals l
    getXVals (StackedLayoutLR l) = getLayoutLRXVals l
    
    all_xvals = concatMap getXVals sls

    allLegendItems = (concatMap (fst.legendItems) sls, concatMap (snd.legendItems) sls)
    
    legendItems :: StackedLayout x -> ([LegendItem], [LegendItem])
    legendItems (StackedLayout l)   = (getLegendItems l, [])
    lebendItems (StackedLayoutLR l) = getLegendItemsLR l
    
    noPickFn :: Renderable a -> Renderable ()
    noPickFn = mapPickFn (const ())

----------------------------------------------------------------------
    
addMarginsToGrid :: (Double,Double,Double,Double) -> Grid (Renderable a)
                 -> Grid (Renderable a)
addMarginsToGrid (t,b,l,r) g = aboveN [
     besideN [er, ts, er],
     besideN [ls, g,  rs],
     besideN [er, bs, er]
  ]
  where
    er = empty
    ts = tval $ spacer (0,t)
    ls = tval $ spacer (l,0)
    bs = tval $ spacer (0,b)
    rs = tval $ spacer (r,0)

titleToRenderable :: Double -> FontStyle -> String -> Renderable (LayoutPick x yl yr)
titleToRenderable lm fs "" = emptyRenderable
titleToRenderable lm fs s = addMargins (lm/2,0,0,0) (mapPickFn LayoutPick_Title title)
  where
    title = label fs HTA_Centre VTA_Centre s

mkLegend :: Maybe LegendStyle -> Double -> [LegendItem] -> Renderable (LayoutPick x yl yr)
mkLegend ls lm vals = case ls of
    Nothing -> emptyRenderable
    Just ls ->  case filter ((/="").fst) vals of
        []  -> emptyRenderable ;
        lvs -> addMargins (0,lm,lm,lm) $
                   mapPickFn LayoutPick_Legend $ legendToRenderable (Legend ls lvs)


data LayoutGridElements x yl yr = LayoutGridElements {
  lge_plots :: Renderable (LayoutPick x yl yr),
  
  lge_taxis :: (Maybe (AxisT x),String,FontStyle),
  lge_baxis :: (Maybe (AxisT x),String,FontStyle),
  lge_laxis :: (Maybe (AxisT yl),String,FontStyle),
  lge_raxis :: (Maybe (AxisT yr),String,FontStyle),

  lge_margin :: Double
}

buildGrid :: (Ord x, Ord yl, Ord yr) => LayoutGridElements x yl yr -> Grid (Renderable (LayoutPick x yl yr))
buildGrid lge = layer2 `overlay` layer1
  where
    layer1 = aboveN
         [ besideN [er,     er,  er,    er   ]
         , besideN [er,     er,  er,    weights (1,1) plots ]
         ]

    layer2 = aboveN
         [ besideN [er,     er,  tl,    taxis,  tr,    er,  er       ]
         , besideN [ltitle, lam, laxis, er,     raxis, ram, rtitle   ]
         , besideN [er,     er,  bl,    baxis,  br,    er,  er       ]
         , besideN [er,     er,  er,    btitle, er,    er,  er       ]
         ]

    er = tval $ emptyRenderable

    plots = tval $ lge_plots lge

    (tdata,tlbl,tstyle) = lge_taxis lge
    (bdata,blbl,bstyle) = lge_baxis lge
    (ldata,llbl,lstyle) = lge_laxis lge
    (rdata,rlbl,rstyle) = lge_raxis lge

    (ttitle,_) = mktitle HTA_Centre VTA_Bottom   0 tlbl tstyle LayoutPick_XTopAxisTitle
    (btitle,_) = mktitle HTA_Centre VTA_Top      0 blbl bstyle LayoutPick_XBottomAxisTitle
    (ltitle,lam) = mktitle HTA_Right  VTA_Centre 270 llbl lstyle LayoutPick_YLeftAxisTitle
    (rtitle,ram) = mktitle HTA_Left   VTA_Centre 270 rlbl rstyle LayoutPick_YRightAxisTitle
    
    baxis = tval $ maybe emptyRenderable
                         (mapPickFn LayoutPick_XBottomAxis . axisToRenderable) bdata
    taxis = tval $ maybe emptyRenderable
                         (mapPickFn LayoutPick_XTopAxis . axisToRenderable) tdata
    laxis = tval $ maybe emptyRenderable
                         (mapPickFn LayoutPick_YLeftAxis . axisToRenderable) ldata
    raxis = tval $ maybe emptyRenderable
                         (mapPickFn LayoutPick_YRightAxis . axisToRenderable) rdata

    tl = tval $ axesSpacer fst tdata fst ldata
    bl = tval $ axesSpacer fst bdata snd ldata
    tr = tval $ axesSpacer snd tdata fst rdata
    br = tval $ axesSpacer snd bdata snd rdata

    mktitle :: HTextAnchor -> VTextAnchor 
            -> Double
            -> String -> FontStyle
            -> (String -> LayoutPick x yl yr) 
            -> ( Grid (Renderable (LayoutPick x yl yr))
               , Grid (Renderable (LayoutPick x yl yr)) )
    mktitle ha va rot lbl style pf = if lbl == "" then (er,er) else (label,gap)
      where
        label = tval $ mapPickFn pf $ rlabel style ha va rot lbl
        gap = tval $ spacer (lge_margin lge,0)

-- | Render the grids of the given axis to a plot area of given size.
renderGrids :: RectSize -> (Maybe (AxisT x), Maybe (AxisT yl), Maybe (AxisT x), Maybe (AxisT yr)) -> ChartBackend ()
renderGrids sz (bAxis, lAxis, tAxis, rAxis) = do
  maybeM () (renderAxisGrid sz) tAxis
  maybeM () (renderAxisGrid sz) bAxis
  maybeM () (renderAxisGrid sz) lAxis
  maybeM () (renderAxisGrid sz) rAxis

-- | Swap the contents of the pair depending on the flag.
optPairReverse :: Bool -> (a,a) -> (a,a)
optPairReverse rev (a,b) = if rev then (b,a) else (a,b)

-- | Render a single set of plot data onto a plot area of given size using
--   the given x and y axis.
renderSinglePlot :: RectSize -> Maybe (AxisT x) -> Maybe (AxisT y) -> Plot x y -> ChartBackend ()
renderSinglePlot (w, h) (Just (AxisT _ xs xrev xaxis)) (Just (AxisT _ ys yrev yaxis)) p =
  let xr = optPairReverse xrev (0, w)
      yr = optPairReverse yrev (h, 0)
      yrange = if yrev then (0, h) else (h, 0)
      pmfn (x,y) = Point (mapv xr (_axis_viewport xaxis xr) x)
                         (mapv yr (_axis_viewport yaxis yr) y)
      mapv (min,max) _ LMin       = min
      mapv (min,max) _ LMax       = max
      mapv _         f (LValue v) = f v
  in _plot_render p pmfn
renderSinglePlot _ _ _ _ = return ()

axesSpacer :: (Ord x, Ord y) 
           => ((Double, Double) -> Double) -> Maybe (AxisT x)
           -> ((Double, Double) -> Double) -> Maybe (AxisT y)
           -> Renderable a
axesSpacer f1 a1 f2 a2 = embedRenderable $ do
    oh1 <- maybeM (0,0) axisOverhang a1
    oh2 <- maybeM (0,0) axisOverhang a2
    return (spacer (f1 oh1, f2 oh2))

-- | Construct a axis for the given edge using the attributes 
--   from a 'LayoutAxis' the given values.
mkAxis :: RectEdge -> LayoutAxis z -> [z] -> Maybe (AxisT z)
mkAxis edge laxis vals = case axisVisible of
    False -> Nothing
    True  -> Just $ AxisT edge style rev adata
  where
    style = _laxis_style laxis
    rev   = _laxis_reverse laxis
    adata = (_laxis_override laxis) (_laxis_generate laxis vals)
    vis   = _axis_visibility adata
    axisVisible = _axis_show_labels vis || _axis_show_line vis || _axis_show_ticks vis

-- | Override the visibility of a selected axis with the selected 'AxisVisibility'.
overrideAxisVisibility :: layout 
                       -> (layout -> LayoutAxis z) 
                       -> (layout -> AxisVisibility) 
                       -> LayoutAxis z 
overrideAxisVisibility ly selAxis selVis = 
  let vis = selVis ly
  in (selAxis ly) { _laxis_override = (\ad -> ad { _axis_visibility = selVis ly }) 
                                    . _laxis_override (selAxis ly)
                  }

mfill :: Maybe FillStyle -> Renderable a -> Renderable a
mfill Nothing   = id
mfill (Just fs) = fillBackground fs

-- | Empty 'LayoutLR' without title and plots. The background is white and 
--   the grid is drawn beneath all plots. There will be a legend. The top
--   axis will not be visible.
instance (PlotValue x, PlotValue y1, PlotValue y2) => Default (LayoutLR x y1 y2) where
  def = LayoutLR
    { _layoutlr_background      = solidFillStyle $ opaque white
    , _layoutlr_plot_background = Nothing

    , _layoutlr_title           = ""
    , _layoutlr_title_style     = def { _font_size   = 15
                                      , _font_weight = FontWeightBold }

    , _layoutlr_x_axis                 = def
    , _layoutlr_top_axis_visibility    = def { _axis_show_line   = False
                                             , _axis_show_ticks  = False
                                             , _axis_show_labels = False }
    , _layoutlr_bottom_axis_visibility = def

    , _layoutlr_left_axis           = def
    , _layoutlr_left_axis_visibility  = def
    , _layoutlr_right_axis          = def
    , _layoutlr_right_axis_visibility = def
    
    , _layoutlr_plots      = []

    , _layoutlr_legend          = Just def
    , _layoutlr_margin          = 10
    , _layoutlr_grid_last       = False
    }

{-# DEPRECATED defaultLayoutAxis "Use the according Data.Default instance!" #-}
defaultLayoutAxis :: PlotValue t => LayoutAxis t
defaultLayoutAxis = def

instance PlotValue t => Default (LayoutAxis t) where
  def = LayoutAxis
    { _laxis_title_style = def { _font_size=10 }
    , _laxis_title       = ""
    , _laxis_style       = def
    , _laxis_generate    = autoAxis
    , _laxis_override    = id
    , _laxis_reverse     = False
    }

----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor
-- for each field.
$( makeLenses ''Layout )
$( makeLenses ''LayoutLR )
$( makeLenses ''LayoutAxis )
$( makeLenses ''StackedLayouts )

-- | Helper to update all axis styles on a Layout1 simultaneously.
updateAllAxesStyles :: (AxisStyle -> AxisStyle) -> Layout x y -> Layout x y
updateAllAxesStyles uf = (layout_x_axis . laxis_style %~ uf) .
                         (layout_y_axis . laxis_style %~ uf)

-- | Helper to update all axis styles on a LayoutLR simultaneously.
updateAllAxesStylesLR :: (AxisStyle -> AxisStyle) -> LayoutLR x yl yr -> LayoutLR x yl yr
updateAllAxesStylesLR uf = (layoutlr_x_axis       . laxis_style %~ uf)
                         . (layoutlr_left_axis  . laxis_style %~ uf)
                         . (layoutlr_right_axis . laxis_style %~ uf)

-- | Helper to set the forground color uniformly on a Layout.
setLayoutForeground :: AlphaColour Double -> Layout x y -> Layout x y
setLayoutForeground fg =
    updateAllAxesStyles  ( (axis_line_style  . line_color .~ fg)
                         . (axis_label_style . font_color .~ fg))
                         . (layout_title_style . font_color .~ fg)
                         . (layout_legend %~ fmap (legend_label_style .> font_color .~ fg))

-- | Helper to set the forground color uniformly on a LayoutLR.
setLayoutLRForeground :: AlphaColour Double -> LayoutLR x yl yr -> LayoutLR x yl yr
setLayoutLRForeground fg = updateAllAxesStylesLR 
  ( (axis_line_style  . line_color .~ fg)
  . (axis_label_style . font_color .~ fg))
  . (layoutlr_title_style . font_color .~ fg)
  . (layoutlr_legend %~ fmap (legend_label_style .> font_color .~ fg))
