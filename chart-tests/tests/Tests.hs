
module Tests where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Simple
import Graphics.Rendering.Chart.Grid

import System.Time
import System.Random
import Data.Time.LocalTime
import Control.Lens
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Data.List(sort,nub,scanl1)
import Data.Default.Class
import qualified Data.Map as Map
import Control.Monad
import Prices
import qualified Test1
import qualified Test2
import qualified Test3
import qualified Test4
import qualified Test5
import qualified Test6
import qualified Test7
import qualified Test8
import qualified Test9
import qualified Test14
import qualified Test14a
import qualified Test15
import qualified Test17
import qualified TestParametric
import qualified TestSparkLines

type LineWidth = Double

fwhite = solidFillStyle $ opaque white

test1a :: Double -> Renderable (LayoutPick Double Double Double)
test1a lwidth = fillBackground fwhite $ (gridToRenderable t)
  where
    t = weights (1,1) $ aboveN [ besideN [rf g1, rf g2, rf g3],
                                 besideN [rf g4, rf g5, rf g6] ]

    g1 = layout_title .~ "minimal"
       $ layout_bottom_axis_visibility . axis_show_ticks .~ False
       $ layout_left_axis_visibility   . axis_show_ticks .~ False
       $ layout_x_axis . laxis_override .~ axisGridHide
       $ layout_y_axis . laxis_override .~ axisGridHide
       $ Test1.layout lwidth

    g2 = layout_title .~ "with borders"
       $ layout_bottom_axis_visibility . axis_show_ticks .~ False
       $ layout_left_axis_visibility   . axis_show_ticks .~ False
       $ layout_top_axis_visibility    . axis_show_line   .~ True
       $ layout_right_axis_visibility  . axis_show_line   .~ True
       $ layout_x_axis . laxis_override .~ axisGridHide
       $ layout_y_axis . laxis_override .~ axisGridHide
       $ Test1.layout lwidth

    g3 = layout_title .~ "default"
       $ Test1.layout lwidth

    g4 = layout_title .~ "tight grid"
       $ layout_y_axis . laxis_generate .~ axis
       $ layout_y_axis . laxis_override .~ axisGridAtTicks
       $ layout_x_axis . laxis_generate .~ axis
       $ layout_x_axis . laxis_override .~ axisGridAtTicks
       $ Test1.layout lwidth
      where
        axis = autoScaledAxis (
            la_nLabels .~ 5
          $ la_nTicks .~ 20
          $ def
          )

    g5 = layout_title .~ "y linked"
       $ layout_right_axis_visibility . axis_show_line   .~ True
       $ layout_right_axis_visibility . axis_show_ticks  .~ True
       $ layout_right_axis_visibility . axis_show_labels .~ True
       $ Test1.layout lwidth

    g6 = layout_title .~ "everything"
       $ layout_right_axis_visibility . axis_show_line   .~ True
       $ layout_right_axis_visibility . axis_show_ticks  .~ True
       $ layout_right_axis_visibility . axis_show_labels .~ True
       $ layout_top_axis_visibility . axis_show_line   .~ True
       $ layout_top_axis_visibility . axis_show_ticks  .~ True
       $ layout_top_axis_visibility . axis_show_labels .~ True
       $ Test1.layout lwidth

    rf = tval . layoutToRenderable

----------------------------------------------------------------------
test4d :: LineWidth -> Renderable (LayoutPick Double Double Double)
test4d lw = layoutToRenderable layout
  where

    points = plot_points_style .~ filledCircles 3 (opaque red)
           $ plot_points_values .~ [ (x, 10**x) | x <- [0.5,1,1.5,2,2.5::Double] ]
           $ plot_points_title .~ "values"
           $ def

    lines = plot_lines_values .~ [ [(x, 10**x) | x <- [0,3]] ]
          $ plot_lines_title .~ "values"
          $ def

    layout = layout_title .~ "Log/Linear Example"
           $ layout_x_axis . laxis_title .~ "horizontal"
           $ layout_x_axis . laxis_reverse .~ False
           $ layout_y_axis . laxis_generate .~ autoScaledLogAxis def
           $ layout_y_axis . laxis_title .~ "vertical"
           $ layout_y_axis . laxis_reverse .~ False
	   $ layout_plots .~ [ toPlot points `joinPlot` toPlot lines ]
           $ def

----------------------------------------------------------------------

test9 :: PlotBarsAlignment -> LineWidth -> Renderable (LayoutPick PlotIndex Double Double)
test9 alignment lw = fillBackground fwhite $ (gridToRenderable t)
  where
    t = weights (1,1) $ aboveN [ besideN [rf g0, rf g1, rf g2],
                                 besideN [rf g3, rf g4, rf g5] ]

    g0 = layout "clustered 1"
       $ plot_bars_style .~ BarsClustered
       $ plot_bars_spacing .~ BarsFixWidth 25
       $ bars1

    g1 = layout "clustered/fix width "
       $ plot_bars_style .~ BarsClustered
       $ plot_bars_spacing .~ BarsFixWidth 25
       $ bars2

    g2 = layout "clustered/fix gap "
       $ plot_bars_style .~ BarsClustered
       $ plot_bars_spacing .~ BarsFixGap 10 5
       $ bars2

    g3 = layout "stacked 1"
       $ plot_bars_style .~ BarsStacked
       $ plot_bars_spacing .~ BarsFixWidth 25
       $ bars1

    g4 = layout "stacked/fix width"
       $ plot_bars_style .~ BarsStacked
       $ plot_bars_spacing .~ BarsFixWidth 25
       $ bars2

    g5 = layout "stacked/fix gap"
       $ plot_bars_style .~ BarsStacked
       $ plot_bars_spacing .~ BarsFixGap 10 5
       $ bars2

    rf = tval . layoutToRenderable

    alabels = [ "Jun", "Jul", "Aug", "Sep", "Oct" ]


    layout title bars =
             layout_title .~ (show alignment ++ "/" ++ title)
           $ layout_title_style . font_size .~ 10
           $ layout_x_axis . laxis_generate .~ autoIndexAxis alabels
           $ layout_y_axis . laxis_override .~ axisGridHide
           $ layout_left_axis_visibility . axis_show_ticks .~ False
           $ layout_plots .~ [ plotBars bars ]
           $ def :: Layout PlotIndex Double

    bars1 = plot_bars_titles .~ ["Cash"]
          $ plot_bars_values .~ addIndexes [[20],[45],[30],[70]]
          $ plot_bars_alignment .~ alignment
          $ def

    bars2 = plot_bars_titles .~ ["Cash","Equity"]
          $ plot_bars_values .~ addIndexes [[20,45],[45,30],[30,20],[70,25]]
          $ plot_bars_alignment .~ alignment
          $ def

-------------------------------------------------------------------------------

test10 :: [(LocalTime,Double,Double)] -> LineWidth -> Renderable (LayoutPick LocalTime Double Double)
test10 prices lw = layoutLRToRenderable layout
  where

    lineStyle c = line_width .~ 3 * lw
                $ line_color .~ c
                $ def ^. plot_lines_style

    price1 = plot_lines_style .~ lineStyle (opaque blue)
           $ plot_lines_values .~ [[ (d,v) | (d,v,_) <- prices]]
           $ plot_lines_title .~ "price 1"
           $ def

    price1_area = plot_fillbetween_values .~ [(d, (v * 0.95, v * 1.05)) | (d,v,_) <- prices]
                $ plot_fillbetween_style  .~ solidFillStyle (withOpacity blue 0.2)
                $ def

    price2 = plot_lines_style .~ lineStyle (opaque red)
	   $ plot_lines_values .~ [[ (d, v) | (d,_,v) <- prices]]
           $ plot_lines_title .~ "price 2"
           $ def

    price2_area = plot_fillbetween_values .~ [(d, (v * 0.95, v * 1.05)) | (d,_,v) <- prices]
                $ plot_fillbetween_style  .~ solidFillStyle (withOpacity red 0.2)
                $ def

    fg = opaque black
    fg1 = opaque $ sRGB 0.0 0.0 0.15

    layout = layoutlr_title .~"Price History"
           $ layoutlr_background .~ solidFillStyle (opaque white)
           $ layoutlr_right_axis . laxis_override .~ axisGridHide
 	   $ layoutlr_plots .~ [ Left (toPlot price1_area), Right (toPlot price2_area)
                               , Left (toPlot price1),      Right (toPlot price2)
                               ]
           $ setLayoutLRForeground fg
           $ def

-------------------------------------------------------------------------------
-- A quick test of stacked layouts

test11_ f = f layout1 layout2
  where
    vs1 :: [(Int,Int)]
    vs1 = [ (2,2), (3,40), (8,400), (12,60) ]

    vs2 :: [(Int,Double)]
    vs2 = [ (0,0.7), (3,0.35), (4,0.25), (7, 0.6), (10,0.4) ]

    plot1 = plot_points_style .~ filledCircles 5 (opaque red)
          $ plot_points_values .~ vs1
          $ plot_points_title .~ "spots"
          $ def

    layout1 = layout_title .~ "Multi typed stack"
            $ layout_plots .~ [toPlot plot1]
            $ layout_y_axis . laxis_title .~ "integer values"
            $ def

    plot2 = plot_lines_values .~ [vs2]
          $ plot_lines_title .~ "lines"
          $ def

    layout2 = layout_plots .~ [toPlot plot2]
            $ layout_y_axis . laxis_title .~ "double values"
            $ def

test11a :: LineWidth -> Renderable ()
test11a lw = test11_ f
   where
     f l1 l2 = renderStackedLayouts 
             $ slayouts_layouts .~ [StackedLayout l1, StackedLayout l2]
             $ slayouts_compress_legend .~ False
             $ def
 
test11b :: LineWidth -> Renderable ()
test11b lw = test11_ f
  where
    f l1 l2 = renderStackedLayouts 
            $ slayouts_layouts .~ [StackedLayout l1', StackedLayout l2]
            $ slayouts_compress_legend .~ True
            $ def
      where
        l1' = layout_bottom_axis_visibility . axis_show_labels .~ False
            $ l1

-------------------------------------------------------------------------------
-- More of an example that a test:
-- configuring axes explicitly configured axes

test12 :: LineWidth -> Renderable (LayoutPick Int Int Int)
test12 lw = layoutToRenderable layout
  where
    vs1 :: [(Int,Int)]
    vs1 = [ (2,10), (3,40), (8,400), (12,60) ]

    baxis = AxisData {
        _axis_visibility = def,
        _axis_viewport = vmap (0,15),
        _axis_tropweiv = invmap (0,15),
        _axis_ticks    = [(v,3) | v <- [0,1..15]],
        _axis_grid     = [0,5..15],
        _axis_labels   = [[(v,show v) | v <- [0,5..15]]]
    }    

    laxis = AxisData {
        _axis_visibility = def,
        _axis_viewport = vmap (0,500),
        _axis_tropweiv = invmap (0,500),
        _axis_ticks    = [(v,3) | v <- [0,25..500]],
        _axis_grid     = [0,100..500],
        _axis_labels   = [[(v,show v) | v <- [0,100..500]]]
    }    

    plot = plot_lines_values .~ [vs1]
         $ def

    layout = layout_plots .~ [toPlot plot]
           $ layout_x_axis . laxis_generate .~ const baxis
           $ layout_y_axis . laxis_generate .~ const laxis
           $ layout_title .~ "Explicit Axes"
           $ def


-------------------------------------------------------------------------------
-- Plot annotations test

test13 lw = fillBackground fwhite $ (gridToRenderable t)
  where
    t = weights (1,1) $ aboveN [ besideN [tval (annotated h v) | h <- hs] | v <- vs ]
    hs = [HTA_Left, HTA_Centre, HTA_Right]
    vs = [VTA_Top, VTA_Centre, VTA_Bottom]
    points=[-2..2]
    pointPlot :: PlotPoints Int Int
    pointPlot = plot_points_style.~ filledCircles 2 (opaque red)
                $  plot_points_values .~ [(x,x)|x<-points]
                $  def
    p = toPlot pointPlot
    annotated h v = layoutToRenderable ( layout_plots .~ [toPlot labelPlot, toPlot rotPlot, p] $ def )
      where labelPlot = plot_annotation_hanchor .~ h
                      $ plot_annotation_vanchor .~ v
                      $ plot_annotation_values  .~ [(x,x,"Hello World\n(plain)")|x<-points]
                      $ def
            rotPlot =   plot_annotation_angle .~ -45.0
                      $ plot_annotation_style .~ def {_font_size=10, _font_weight=FontWeightBold, _font_color=(opaque blue) }
                      $ plot_annotation_values  .~ [(x,x,"Hello World\n(fancy)")|x<-points]
                      $ labelPlot


----------------------------------------------------------------------
-- a quick test to display labels with all combinations
-- of anchors
misc1 rot lw = fillBackground fwhite $ (gridToRenderable t)
  where
    t = weights (1,1) $ aboveN [ besideN [tval (lb h v) | h <- hs] | v <- vs ]
    lb h v = addMargins (20,20,20,20) $ fillBackground fblue $ crossHairs $ rlabel fs h v rot s
    s = "Labelling"
    hs = [HTA_Left, HTA_Centre, HTA_Right]
    vs = [VTA_Top, VTA_Centre, VTA_Bottom]
    fwhite = solidFillStyle $ opaque white
    fblue = solidFillStyle $ opaque $ sRGB 0.8 0.8 1
    fs = def {_font_size=20,_font_weight=FontWeightBold}
    crossHairs r =Renderable {
      minsize = minsize r,
      render = \sz@(w,h) -> do
          let xa = w / 2
          let ya = h / 2
          alignStrokePoints [Point 0 ya,Point w ya] >>= strokePointPath 
          alignStrokePoints [Point xa 0,Point xa h] >>= strokePointPath 
          render r sz
    }

----------------------------------------------------------------------
stdSize = (640,480)

allTests :: [ (String, (Int,Int), LineWidth -> Renderable ()) ]
allTests =
     [ ("test1",  stdSize, \lw -> simple $ Test1.chart lw )
     , ("test1a", stdSize, \lw -> simple $ test1a lw )
     , ("test2a", stdSize, \lw -> simple $ Test2.chart prices    False lw)
     , ("test2b", stdSize, \lw -> simple $ Test2.chart prices1   False lw)
     , ("test2c", stdSize, \lw -> simple $ Test2.chart prices2   False lw)
     , ("test2d", stdSize, \lw -> simple $ Test2.chart prices5   True  lw)
     , ("test2e", stdSize, \lw -> simple $ Test2.chart prices6   True  lw)
     , ("test2f", stdSize, \lw -> simple $ Test2.chart prices7   True  lw)
     , ("test2g", stdSize, \lw -> simple $ Test2.chart prices3   False lw)
     , ("test2h", stdSize, \lw -> simple $ Test2.chart prices8   True  lw)
     , ("test2i", stdSize, \lw -> simple $ Test2.chart prices9   True  lw)
     , ("test2j", stdSize, \lw -> simple $ Test2.chart prices10  True  lw)
     , ("test2k", stdSize, \lw -> simple $ Test2.chart prices10a True  lw)
     , ("test2m", stdSize, \lw -> simple $ Test2.chart prices11  True  lw)
     , ("test2n", stdSize, \lw -> simple $ Test2.chart prices10b True  lw)
     , ("test2o", stdSize, \lw -> simple $ Test2.chart prices12  True  lw)
     , ("test2p", stdSize, \lw -> simple $ Test2.chart prices13  True  lw)
     , ("test2q", stdSize, \lw -> simple $ Test2.chart prices13a True  lw)
     , ("test2r", stdSize, \lw -> simple $ Test2.chart prices13b True  lw)
     , ("test2s", stdSize, \lw -> simple $ Test2.chart prices14  True  lw)
     , ("test2t", stdSize, \lw -> simple $ Test2.chart prices14a True  lw)
     , ("test2u", stdSize, \lw -> simple $ Test2.chart prices14b True  lw)
     , ("test2v", stdSize, \lw -> simple $ Test2.chart prices14c True  lw)
     , ("test2w", stdSize, \lw -> simple $ Test2.chart prices14d True  lw)
     , ("test3",  stdSize, const $ simple Test3.chart)
     , ("test4a", stdSize, const $ simple (Test4.chart False False))
     , ("test4b", stdSize, const $ simple (Test4.chart True False))
     , ("test4c", stdSize, const $ simple (Test4.chart False True))
     , ("test4d", stdSize, \lw -> simple $ test4d lw)
     , ("test5",  stdSize, \lw -> simple $ Test5.chart lw)
     , ("test6",  stdSize, const $ simple Test6.chart)
     , ("test7",  stdSize, const $ simple Test7.chart)
     , ("test8",  stdSize, const $ simple Test8.chart)
     , ("test9",  stdSize, const $ simple (Test9.chart True))
     , ("test9b", stdSize, const $ simple (Test9.chart False))
     , ("test9c", stdSize, \lw -> simple $ test9 BarsCentered lw)
     , ("test9l", stdSize, \lw -> simple $ test9 BarsLeft lw)
     , ("test9r", stdSize, \lw -> simple $ test9 BarsRight lw)
     , ("test10", stdSize, \lw -> simple $ test10 prices1 lw)
     , ("test11a", stdSize, \lw -> simple $ test11a lw)
     , ("test11b", stdSize, \lw -> simple $ test11b lw)
     , ("test12", stdSize, \lw -> simple $ test12 lw)
     , ("test13", stdSize, \lw -> simple $ test13 lw)
     , ("test14", stdSize, \lw -> simple $ Test14.chart lw )
     , ("test14a", stdSize, \lw -> simple $ Test14a.chart lw )
     , ("test15a", stdSize, const $ simple (Test15.chart (LORows 2)))
     , ("test15b", stdSize, const $ simple (Test15.chart (LOCols 2)))
     , ("test17", stdSize,  \lw -> simple $ Test17.chart lw)
     , ("misc1",  stdSize, setPickFn nullPickFn . misc1 0)
     , ("misc1a", stdSize, setPickFn nullPickFn . misc1 45)
     , ("parametric", stdSize, \lw -> simple $ TestParametric.chart lw )
     , ("sparklines", TestSparkLines.chartSize, const $ simple TestSparkLines.chart )
     ]
  where simple :: Renderable a -> Renderable ()
        simple = mapPickFn (const ())



showTests :: [String] -> ((String,(Int,Int),LineWidth -> Renderable ()) -> IO()) -> IO ()
showTests tests ofn = mapM_ doTest (filter (match tests) allTests)
   where
     doTest (s,size,f) = do
       putStrLn (s ++ "... ")
       ofn (s,size,f)
     

getTests :: [String] -> [(String,(Int,Int),LineWidth -> Renderable ())]
getTests names = filter (match names) allTests

match :: [String] -> (String,s,a) -> Bool
match [] t = True
match ts (s,_,_) = s `elem` ts
