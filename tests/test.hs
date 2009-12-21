import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Simple
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Grid

import System.Environment(getArgs)
import System.Time
import System.Random
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Accessor
import Data.Accessor.Tuple
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Data.List(sort,nub,scanl1)
import qualified Data.Map as Map
import Control.Monad
import Prices

data OutputType = Window | PNG | PS | PDF | SVG

chooseLineWidth Window = 1.0
chooseLineWidth PNG = 1.0
chooseLineWidth PDF = 0.25
chooseLineWidth PS = 0.25
chooseLineWidth SVG = 0.25

green1 = opaque $ sRGB 0.5 1 0.5
red1 = opaque $ sRGB 0.5 0.5 1
fwhite = solidFillStyle $ opaque white
fparchment = solidFillStyle $ opaque $ sRGB 1.0 0.99 0.90

----------------------------------------------------------------------
test1Layout otype = layout
  where
    am :: Double -> Double
    am x = (sin (x*pi/45) + 1) / 2 * (sin (x*pi/5))

    sinusoid1 = plot_lines_values ^= [[ (x,(am x)) | x <- [0,(0.5)..400]]]
              $ plot_lines_style  ^= solidLine lineWidth (opaque blue)
              $ plot_lines_title ^="am"
              $ defaultPlotLines

    sinusoid2 = plot_points_style ^= filledCircles 2 (opaque red)
              $ plot_points_values ^= [ (x,(am x)) | x <- [0,7..400]]
              $ plot_points_title ^="am points"
              $ defaultPlotPoints

    layout = layout1_plots ^= [Left (toPlot sinusoid1),
			       Left (toPlot sinusoid2)]
           $ layout1_background ^= fparchment
           $ layout1_plot_background ^= Just fwhite
           $ defaultLayout1

    lineWidth = chooseLineWidth otype

test1 :: OutputType -> Renderable ()
test1 otype = toRenderable layout
  where
    layout = layout1_title ^= "Amplitude Modulation"
           $ (test1Layout otype)

test1a :: OutputType -> Renderable ()
test1a otype = fillBackground fwhite $ (gridToRenderable t)
  where
    t = weights (1,1) $ aboveN [ besideN [rf g1, rf g2, rf g3],
                                 besideN [rf g4, rf g5, rf g6] ]

    g1 = layout1_title ^= "minimal"
       $ layout1_bottom_axis ^: laxis_override ^= (axisGridHide.axisTicksHide)
       $ layout1_left_axis ^: laxis_override ^= (axisGridHide.axisTicksHide)
       $ test1Layout otype

    g2 = layout1_title ^= "with borders"
       $ layout1_bottom_axis ^: laxis_override ^= (axisGridHide.axisTicksHide)
       $ layout1_left_axis ^: laxis_override ^= (axisGridHide.axisTicksHide)
       $ layout1_top_axis ^: axisBorderOnly
       $ layout1_right_axis ^: axisBorderOnly
       $ test1Layout otype

    g3 = layout1_title ^= "default"
       $ test1Layout otype

    g4 = layout1_title ^= "tight grid"
       $ layout1_left_axis ^: laxis_generate ^= axis
       $ layout1_left_axis ^: laxis_override ^= axisGridAtTicks
       $ layout1_bottom_axis ^: laxis_generate ^= axis
       $ layout1_bottom_axis ^: laxis_override ^= axisGridAtTicks
       $ test1Layout otype
      where
        axis = autoScaledAxis (
            la_nLabels ^= 5
          $ la_nTicks ^= 20
          $ defaultLinearAxis
          )

    g5 = layout1_title ^= "y linked"
       $ layout1_yaxes_control ^= linkAxes
       $ test1Layout otype

    g6 = layout1_title ^= "everything"
       $ layout1_yaxes_control ^= linkAxes
       $ layout1_top_axis ^: laxis_visible ^= const True
       $ test1Layout otype

    rf = tval.toRenderable

    axisBorderOnly = (laxis_visible ^= const True)
                   . (laxis_override ^=  (axisGridHide.axisTicksHide.axisLabelsHide))

----------------------------------------------------------------------
test2 :: [(Int,Int,Int,Double,Double)] -> Bool -> OutputType -> Renderable ()
test2 prices showMinMax otype = toRenderable layout
  where

    lineStyle c = line_width ^= 3 * chooseLineWidth otype
                $ line_color ^= c
                $ defaultPlotLines ^. plot_lines_style

    limitLineStyle c = line_width ^= chooseLineWidth otype
                $ line_color ^= opaque c
                $ line_dashes ^= [5,10]
                $ defaultPlotLines ^. plot_lines_style

    price1 = plot_lines_style ^= lineStyle (opaque blue)
           $ plot_lines_values ^= [[ ((date d m y), v) | (d,m,y,v,_) <- prices]]
           $ plot_lines_title ^= "price 1"
           $ defaultPlotLines

    price2 = plot_lines_style ^= lineStyle (opaque green)
	   $ plot_lines_values ^= [[ ((date d m y), v) | (d,m,y,_,v) <- prices]]
           $ plot_lines_title ^= "price 2"
           $ defaultPlotLines

    (min1,max1) = (minimum [v | (_,_,_,v,_) <- prices],maximum [v | (_,_,_,v,_) <- prices])
    (min2,max2) = (minimum [v | (_,_,_,_,v) <- prices],maximum [v | (_,_,_,_,v) <- prices])
    limits | showMinMax = [ Left $ hlinePlot "min/max" (limitLineStyle blue) min1,
                            Left $ hlinePlot "" (limitLineStyle blue) max1,
                            Right $ hlinePlot "min/max" (limitLineStyle green) min2,
                            Right $ hlinePlot "" (limitLineStyle green) max2 ]
           | otherwise  = []

    bg = opaque $ sRGB 0 0 0.25
    fg = opaque white
    fg1 = opaque $ sRGB 0.0 0.0 0.15

    layout = layout1_title ^="Price History"
           $ layout1_background ^= solidFillStyle bg
           $ updateAllAxesStyles (axis_grid_style ^= solidLine 1 fg1)
           $ layout1_left_axis ^: laxis_override ^= axisGridHide
           $ layout1_right_axis ^: laxis_override ^= axisGridHide
           $ layout1_bottom_axis ^: laxis_override ^= axisGridHide
 	   $ layout1_plots ^= ([Left (toPlot price1), Right (toPlot price2)] ++ limits)
           $ layout1_grid_last ^= False
           $ setLayout1Foreground fg
           $ defaultLayout1

date dd mm yyyy = (LocalTime (fromGregorian (fromIntegral yyyy) mm dd) midnight)

----------------------------------------------------------------------
test3 :: OutputType -> Renderable ()
test3 otype = toRenderable layout
  where

    price1 = plot_fillbetween_style ^= solidFillStyle green1
           $ plot_fillbetween_values ^= [ (date d m y,(0,v2)) | (d,m,y,v1,v2) <- prices]
           $ plot_fillbetween_title ^= "price 1"
           $ defaultPlotFillBetween

    price2 = plot_fillbetween_style ^= solidFillStyle red1
           $ plot_fillbetween_values ^= [ (date d m y,(0,v1)) | (d,m,y,v1,v2) <- prices]
           $ plot_fillbetween_title ^= "price 2"
           $ defaultPlotFillBetween

    layout = layout1_title ^= "Price History"
           $ layout1_grid_last ^= True
 	   $ layout1_plots ^= [Left (toPlot price1),
                               Left (toPlot price2)]
           $ defaultLayout1

----------------------------------------------------------------------
test4 :: Bool -> Bool -> OutputType -> Renderable ()
test4 xrev yrev otype = toRenderable layout
  where

    points = plot_points_style ^= filledCircles 3 (opaque red)
           $ plot_points_values ^= [ (x, LogValue (10**x)) | x <- [0.5,1,1.5,2,2.5] ]
           $ plot_points_title ^= "values"
           $ defaultPlotPoints

    lines = plot_lines_values ^= [ [(x, LogValue (10**x)) | x <- [0,3]] ]
          $ plot_lines_title ^= "values"
          $ defaultPlotLines

    layout = layout1_title ^= "Log/Linear Example"
           $ layout1_bottom_axis ^: laxis_title ^= "horizontal"
           $ layout1_bottom_axis ^: laxis_reverse ^= xrev
           $ layout1_left_axis ^: laxis_title ^= "vertical"
           $ layout1_left_axis ^: laxis_reverse ^= yrev
	   $ layout1_plots ^= [Left (toPlot points), Left (toPlot lines) ]
           $ defaultLayout1

----------------------------------------------------------------------
test4d :: OutputType -> Renderable ()
test4d otype = toRenderable layout
  where

    points = plot_points_style ^= filledCircles 3 (opaque red)
           $ plot_points_values ^= [ (x, LogValue (10**x)) | x <- [0.5,1,1.5,2,2.5] ]
           $ plot_points_title ^= "values"
           $ defaultPlotPoints

    lines = plot_lines_values ^= [ [(x, LogValue (10**x)) | x <- [0,3]] ]
          $ plot_lines_title ^= "values"
          $ defaultPlotLines

    layout = layout1_title ^= "Log/Linear Example"
           $ layout1_bottom_axis ^: laxis_title ^= "horizontal"
           $ layout1_bottom_axis ^: laxis_reverse ^= False
           $ layout1_left_axis ^: laxis_title ^= "vertical"
           $ layout1_left_axis ^: laxis_reverse ^= False
	   $ layout1_plots ^= [Left (toPlot points `joinPlot` toPlot lines) ]
           $ defaultLayout1

----------------------------------------------------------------------
-- Example thanks to Russell O'Connor

test5 :: OutputType -> Renderable ()
test5 otype = toRenderable (layout 1001 (trial bits) :: Layout1 Double LogValue)
  where
    bits = randoms $ mkStdGen 0

    layout n t = layout1_title ^= "Simulation of betting on a biased coin"
               $ layout1_plots ^= [
                      Left (toPlot (plot "f=0.05" s1 n 0 (t 0.05))),
                      Left (toPlot (plot "f=0.1" s2 n 0 (t 0.1)))
                     ]
               $ defaultLayout1

    plot tt s n m t = plot_lines_style ^= s
                 $ plot_lines_values ^=
                       [[(fromIntegral x, LogValue y) | (x,y) <-
                         filter (\(x,_)-> x `mod` (m+1)==0) $ take n $ zip [0..] t]]
                 $ plot_lines_title ^= tt
                 $ defaultPlotLines

    b = 0.1

    trial bits frac = scanl (*) 1 (map f bits)
      where
        f True = (1+frac*(1+b))
        f False = (1-frac)

    s1 = solidLine lineWidth $ opaque green
    s2 = solidLine lineWidth $ opaque blue

    lineWidth = chooseLineWidth otype


----------------------------------------------------------------------
-- Test the Simple interface

test6 :: OutputType -> Renderable ()
test6 otype = toRenderable (plotLayout pp){layout1_title_="Graphics.Rendering.Chart.Simple example"}
  where
    pp = plot xs sin "sin"
                 cos "cos" "o"
                 (sin.sin.cos) "sin.sin.cos" "."
                 (/3) "- "
                 (const 0.5)
                 [0.1,0.7,0.5::Double] "+"
    xs = [0,0.3..3] :: [Double]

----------------------------------------------------------------------
test7 :: OutputType -> Renderable ()
test7 otype = toRenderable layout
  where
    vals :: [(Double,Double,Double,Double)]
    vals = [ (x,sin (exp x),sin x/2,cos x/10) | x <- [1..20]]
    bars = plot_errbars_values ^= [symErrPoint x y dx dy | (x,y,dx,dy) <- vals]
         $ plot_errbars_title ^="test"
         $ defaultPlotErrBars

    points = plot_points_style ^= filledCircles 2 (opaque red)
	   $ plot_points_values ^= [(x,y) |  (x,y,dx,dy) <- vals]
           $ plot_points_title ^= "test"
           $ defaultPlotPoints

    layout = layout1_title ^= "errorbars example"
           $ layout1_plots ^= [Left (toPlot bars),
                               Left (toPlot points)]
           $ defaultLayout1

----------------------------------------------------------------------

test8 :: OutputType -> Renderable ()
test8 otype = toRenderable layout
  where
    values = [ ("eggs",38,e), ("milk",45,e), ("bread",11,e1), ("salmon",8,e) ]
    e = 0
    e1 = 25
    layout = pie_title ^= "Pie Chart Example"
           $ pie_plot ^: pie_data ^= [ defaultPieItem{pitem_value_=v,pitem_label_=s,pitem_offset_=o}
                                       | (s,v,o) <- values ]
           $ defaultPieLayout

----------------------------------------------------------------------


test9 :: PlotBarsAlignment -> OutputType -> Renderable ()
test9 alignment otype = fillBackground fwhite $ (gridToRenderable t)
  where
    t = weights (1,1) $ aboveN [ besideN [rf g0, rf g1, rf g2],
                                 besideN [rf g3, rf g4, rf g5] ]

    g0 = layout "clustered 1"
       $ plot_bars_style ^= BarsClustered
       $ plot_bars_spacing ^= BarsFixWidth 25
       $ bars1

    g1 = layout "clustered/fix width "
       $ plot_bars_style ^= BarsClustered
       $ plot_bars_spacing ^= BarsFixWidth 25
       $ bars2

    g2 = layout "clustered/fix gap "
       $ plot_bars_style ^= BarsClustered
       $ plot_bars_spacing ^= BarsFixGap 10
       $ bars2

    g3 = layout "stacked 1"
       $ plot_bars_style ^= BarsStacked
       $ plot_bars_spacing ^= BarsFixWidth 25
       $ bars1

    g4 = layout "stacked/fix width"
       $ plot_bars_style ^= BarsStacked
       $ plot_bars_spacing ^= BarsFixWidth 25
       $ bars2

    g5 = layout "stacked/fix gap"
       $ plot_bars_style ^= BarsStacked
       $ plot_bars_spacing ^= BarsFixGap 10
       $ bars2

    rf = tval.toRenderable

    alabels = [ "Jun", "Jul", "Aug", "Sep", "Oct" ]


    layout title bars =
             layout1_title ^= (show alignment ++ "/" ++ title)
           $ layout1_title_style ^: font_size ^= 10
           $ layout1_bottom_axis ^: laxis_generate ^= autoIndexAxis alabels
           $ layout1_left_axis ^: laxis_override ^= (axisGridHide.axisTicksHide)
           $ layout1_plots ^= [ Left (plotBars bars) ]
           $ defaultLayout1 :: Layout1 PlotIndex Double

    bars1 = plot_bars_titles ^= ["Cash"]
          $ plot_bars_values ^= addIndexes [[20],[45],[30],[70]]
          $ plot_bars_alignment ^= alignment
          $ defaultPlotBars

    bars2 = plot_bars_titles ^= ["Cash","Equity"]
          $ plot_bars_values ^= addIndexes [[20,45],[45,30],[30,20],[70,25]]
          $ plot_bars_alignment ^= alignment
          $ defaultPlotBars

-------------------------------------------------------------------------------

test10 :: [(Int,Int,Int,Double,Double)] -> OutputType -> Renderable ()
test10 prices otype = toRenderable layout
  where

    lineStyle c = line_width ^= 3 * chooseLineWidth otype
                $ line_color ^= c
                $ defaultPlotLines ^. plot_lines_style

    price1 = plot_lines_style ^= lineStyle (opaque blue)
           $ plot_lines_values ^= [[ ((date d m y), v) | (d,m,y,v,_) <- prices]]
           $ plot_lines_title ^= "price 1"
           $ defaultPlotLines

    price1_area = plot_fillbetween_values ^= [((date d m y), (v * 0.95, v * 1.05)) | (d,m,y,v,_) <- prices]
                $ plot_fillbetween_style  ^= solidFillStyle (withOpacity blue 0.2)
                $ defaultPlotFillBetween

    price2 = plot_lines_style ^= lineStyle (opaque red)
	   $ plot_lines_values ^= [[ ((date d m y), v) | (d,m,y,_,v) <- prices]]
           $ plot_lines_title ^= "price 2"
           $ defaultPlotLines

    price2_area = plot_fillbetween_values ^= [((date d m y), (v * 0.95, v * 1.05)) | (d,m,y,_,v) <- prices]
                $ plot_fillbetween_style  ^= solidFillStyle (withOpacity red 0.2)
                $ defaultPlotFillBetween

    fg = opaque black
    fg1 = opaque $ sRGB 0.0 0.0 0.15

    layout = layout1_title ^="Price History"
           $ layout1_background ^= solidFillStyle (opaque white)
           $ layout1_right_axis ^: laxis_override ^= axisGridHide
 	   $ layout1_plots ^= [ Left (toPlot price1_area), Right (toPlot price2_area)
                              , Left (toPlot price1),      Right (toPlot price2)
                              ]
           $ setLayout1Foreground fg
           $ defaultLayout1

-------------------------------------------------------------------------------
-- A quick test of stacked layouts

test11 :: OutputType -> Renderable ()
test11 otype = renderLayout1sStacked [withAnyOrdinate layout1, withAnyOrdinate layout2]
  where
    vs1 :: [(Int,Int)]
    vs1 = [ (2,2), (3,40), (8,400), (12,60) ]

    vs2 :: [(Int,Double)]
    vs2 = [ (0,0.7), (3,0.35), (4,0.25), (7, 0.6), (10,0.4) ]

    allx = map fst vs1 ++ map fst vs2
    extendRange = PlotHidden allx []

    plot1 = plot_points_style ^= filledCircles 5 (opaque red)
          $ plot_points_values ^= vs1
          $ defaultPlotPoints

    layout1 = layout1_title ^= "Integer Axis"
 	   $ layout1_plots ^= [Left (toPlot plot1), Left (toPlot extendRange)]
           $ defaultLayout1

    plot2 = plot_lines_values ^= [vs2]
          $ defaultPlotLines

    layout2 = layout1_title ^= "Float Axis"
 	   $ layout1_plots ^= [Left (toPlot plot2), Left (toPlot extendRange)]
           $ defaultLayout1

-------------------------------------------------------------------------------
-- More of an example that a test:
-- configuring axes explicitly configured axes

test12 :: OutputType -> Renderable ()
test12 otype = toRenderable layout
  where
    vs1 :: [(Int,Int)]
    vs1 = [ (2,10), (3,40), (8,400), (12,60) ]

    baxis = AxisData {
        axis_viewport_ = vmap (0,15),
        axis_ticks_    = [(v,3) | v <- [0,1..15]],
        axis_grid_     = [0,5..15],
        axis_labels_   = [(v,show v) | v <- [0,5..15]]
    }    

    laxis = AxisData {
        axis_viewport_ = vmap (0,500),
        axis_ticks_    = [(v,3) | v <- [0,25..500]],
        axis_grid_     = [0,100..500],
        axis_labels_   = [(v,show v) | v <- [0,100..500]]
    }    

    plot = plot_lines_values ^= [vs1]
         $ defaultPlotLines

    layout = layout1_plots ^= [Left (toPlot plot)]
           $ layout1_bottom_axis ^: laxis_generate ^= const baxis
           $ layout1_left_axis   ^: laxis_generate ^= const laxis
           $ layout1_title ^= "Explicit Axes"
           $ defaultLayout1


-------------------------------------------------------------------------------
-- Plot annotations test

test13 otype = fillBackground fwhite $ (gridToRenderable t)
  where
    t = weights (1,1) $ aboveN [ besideN [tval (annotated h v) | h <- hs] | v <- vs ]
    hs = [HTA_Left, HTA_Centre, HTA_Right]
    vs = [VTA_Top, VTA_Centre, VTA_Bottom]
    points=[-2..2]
    pointPlot :: PlotPoints Int Int
    pointPlot = plot_points_style^= filledCircles 2 (opaque red)
                $  plot_points_values ^= [(x,x)|x<-points]
                $  defaultPlotPoints
    p = Left (toPlot pointPlot)
    annotated h v = toRenderable ( layout1_plots ^= [Left (toPlot labelPlot), Left (toPlot rotPlot), p] $ defaultLayout1 )
      where labelPlot = plot_annotation_hanchor ^= h
                      $ plot_annotation_vanchor ^= v
                      $ plot_annotation_values  ^= [(x,x,"Hello World (plain)")|x<-points]
                      $ defaultPlotAnnotation
            rotPlot =   plot_annotation_angle ^= -45.0
                      $ plot_annotation_style ^= defaultFontStyle{font_size_=10,font_weight_=C.FontWeightBold, font_color_ =(opaque blue) }
                      $ plot_annotation_values  ^= [(x,x,"Hello World (fancy)")|x<-points]
                      $ labelPlot


-------------------------------------------------------------------------------
-- demonstrate AreaSpots

test14 :: [(Int,Int,Int,Double,Double)] -> OutputType -> Renderable ()
test14 prices otype = toRenderable layout
  where
    layout = layout1_title ^="Price History"
           $ layout1_background ^= solidFillStyle (opaque white)
           $ layout1_left_axis ^: laxis_override ^= axisTicksHide
 	   $ layout1_plots ^= [ Left (toPlot price1), Left (toPlot spots) ]
           $ setLayout1Foreground fg
           $ defaultLayout1

    price1 = plot_lines_style ^= lineStyle (opaque blue)
           $ plot_lines_values ^= [[ ((date d m y), v) | (d,m,y,v,_) <- prices]]
           $ plot_lines_title ^= "price 1"
           $ defaultPlotLines

    spots = area_spots_title ^= "random value"
          $ area_spots_max_radius ^= 20
          $ area_spots_values ^= values
          $ defaultAreaSpots
    
    points = map (\ (d,v,z)-> (d,v) ) values
    values = [ (date d m y, v, z) | ((d,m,y,v,_),z) <- zip prices zs ]
    zs    :: [Int]
    zs     = randoms $ mkStdGen 0

    lineStyle c = line_width ^= 3 * chooseLineWidth otype
                $ line_color ^= c
                $ defaultPlotLines ^. plot_lines_style

    fg = opaque black

----------------------------------------------------------------------
-- a quick test to display labels with all combinations
-- of anchors
misc1 rot otype = fillBackground fwhite $ (gridToRenderable t)
  where
    t = weights (1,1) $ aboveN [ besideN [tval (lb h v) | h <- hs] | v <- vs ]
    lb h v = addMargins (20,20,20,20) $ fillBackground fblue $ crossHairs $ rlabel fs h v rot s
    s = "Labelling"
    hs = [HTA_Left, HTA_Centre, HTA_Right]
    vs = [VTA_Top, VTA_Centre, VTA_Bottom]
    fwhite = solidFillStyle $ opaque white
    fblue = solidFillStyle $ opaque $ sRGB 0.8 0.8 1
    fs = defaultFontStyle{font_size_=20,font_weight_=C.FontWeightBold}
    crossHairs r =Renderable {
      minsize = minsize r,
      render = \sz@(w,h) -> do
          let xa = w / 2
          let ya = h / 2
          strokeLines [Point 0 ya,Point w ya]
          strokeLines [Point xa 0,Point xa h]
          render r sz
    }

----------------------------------------------------------------------
allTests :: [ (String, OutputType -> Renderable ()) ]
allTests =
     [ ("test1",  test1)
     , ("test1a", test1a)
     , ("test2a", test2 prices False)
     , ("test2b", test2 (filterPrices (date 1 1 2005) (date 31 12 2005)) False)
     , ("test2c", test2 (filterPrices (date 1 5 2005) (date 1 7 2005)) False)
     , ("test2d", test2 (filterPrices (date 1 1 2006) (date 10 1 2006)) False)
     , ("test2e", test2 (filterPrices (date 1 8 2005) (date 31 8 2005)) True)
     , ("test3", test3)
     , ("test4a", test4 False False)
     , ("test4b", test4 True False)
     , ("test4c", test4 False True)
     , ("test4d", test4d)
     , ("test5", test5)
     , ("test6", test6)
     , ("test7", test7)
     , ("test8", test8)
     , ("test9c", test9 BarsCentered)
     , ("test9l", test9 BarsLeft)
     , ("test9r", test9 BarsRight)
     , ("test10", test10 (filterPrices (date 1 1 2005) (date 31 12 2005)))
     , ("test11", test11)
     , ("test12", test12)
     , ("test13", test13)
     , ("test14", test14 (filterPrices (date 1 1 2005) (date 31 12 2005)))
     , ("misc1", misc1 0)
     , ("misc1a", misc1 45)
     ]

filterPrices t1 t2 = [ v | v@(d,m,y,_,_) <- prices, let t = date d m y in t >= t1 && t <= t2]

main = do
    args <- getArgs
    main1 args

main1 :: [String] -> IO ()
main1 ("--png":tests) = showTests tests renderToPNG
main1 ("--pdf":tests) = showTests tests renderToPDF
main1 ("--svg":tests) = showTests tests renderToSVG
main1 ("--ps":tests) = showTests tests renderToPS
main1 tests = showTests tests renderToWindow

showTests :: [String] -> ((String,OutputType -> Renderable ()) -> IO()) -> IO ()
showTests tests ofn = mapM_ ofn (filter (match tests) allTests)

match :: [String] -> (String,a) -> Bool
match [] t = True
match ts t = (fst t) `elem` ts

renderToWindow (n,ir) = renderableToWindow (ir Window) 640 480
renderToPNG (n,ir) = renderableToPNGFile (ir PNG) 640 480 (n ++ ".png")
renderToPS (n,ir) = renderableToPSFile (ir PS) 640 480 (n ++ ".ps")
renderToPDF (n,ir) = renderableToPDFFile (ir PDF) 640 480 (n ++ ".pdf")
renderToSVG (n,ir) = renderableToSVGFile (ir SVG) 640 480 (n ++ ".svg")
