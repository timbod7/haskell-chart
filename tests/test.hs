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
import Prices

data OutputType = Window | PNG | PS | PDF | SVG

chooseLineWidth Window = 1.0
chooseLineWidth PNG = 1.0
chooseLineWidth PDF = 0.25
chooseLineWidth PS = 0.25
chooseLineWidth SVG = 0.25

green1 = (Color 0.5 1 0.5)
red1 = (Color 0.5 0.5 1)

----------------------------------------------------------------------
test1 :: OutputType -> Renderable ()
test1 otype = toRenderable layout
  where
    am :: Double -> Double
    am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))

    sinusoid1 = plot_lines_values ^= [[ (x,(am x)) | x <- [0,(0.5)..400]]]
              $ plot_lines_style  ^= solidLine lineWidth blue
              $ defaultPlotLines

    sinusoid2 = plot_points_style ^= filledCircles 2 red
              $ plot_points_values ^= [ (x,(am x)) | x <- [0,7..400]]
              $ defaultPlotPoints

    layout = layout1_title ^= "Amplitude Modulation"
	   $ layout1_plots ^= [("am",Left (toPlot sinusoid1)),
			      ("am points", Left (toPlot sinusoid2))]
           $ defaultLayout1

    lineWidth = chooseLineWidth otype

test1a :: OutputType -> Renderable ()
test1a otype = toRenderable layout
  where
    am :: Double -> Double
    am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))

    sinusoid1 = plot_lines_values ^= [[ (x,(am x)) | x <- [0,(0.5)..400]]]
              $ plot_lines_style ^= solidLine lineWidth blue
              $ defaultPlotLines

    sinusoid2 = plot_points_style ^= filledCircles 2 red
              $ plot_points_values ^= [ (x,(am x)) | x <- [0,7..400]]
              $ defaultPlotPoints

    axis = mAxis $ axisGridAtTicks.autoScaledAxis (
          la_nLabels ^= 2 
        $ la_nTicks ^= 20
        $ defaultLinearAxis
        )

    layout = layout1_title ^= "Amplitude Modulation"
           $ updateXAxesData (const axis)
           $ updateYAxesData (const axis)
	   $ layout1_plots ^= [("am",Left (toPlot sinusoid1)),
			       ("am points", Left (toPlot sinusoid2))]
           $ defaultLayout1

    lineWidth = chooseLineWidth otype

----------------------------------------------------------------------
test2 :: [(Int,Int,Int,Double,Double)] -> OutputType -> Renderable ()
test2 prices otype = toRenderable layout
  where

    lineStyle c = line_width ^= 3 * chooseLineWidth otype
                $ line_color ^= c
                $ defaultPlotLines ^. plot_lines_style

    price1 = plot_lines_style ^= lineStyle blue
           $ plot_lines_values ^= [[ ((date d m y), v) | (d,m,y,v,_) <- prices]]
           $ defaultPlotLines

    price2 = plot_lines_style ^= lineStyle green
	   $ plot_lines_values ^= [[ ((date d m y), v) | (d,m,y,_,v) <- prices]]
           $ defaultPlotLines

    vaxis = mAxis $ axisGridNone.autoAxis
    bg = Color 0 0 0.25
    fg = Color 1 1 1
    fg1 = Color 0.0 0.0 0.15

    layout = layout1_title ^="Price History"
           $ layout1_background ^= solidFillStyle bg
           $ updateAllAxesStyles (axis_grid_style ^= solidLine 1 fg1)
           $ layout1_left_axis ^: laxis_data ^= vaxis
           $ layout1_right_axis ^: laxis_data ^= vaxis
 	   $ layout1_plots ^= [("price 1", Left (toPlot price1)),
                               ("price 2", Right (toPlot price2))]
           $ layout1_grid_last ^= False
           $ setForeground fg
           $ defaultLayout1

date dd mm yyyy = (LocalTime (fromGregorian (fromIntegral yyyy) mm dd) midnight)

----------------------------------------------------------------------
test3 :: OutputType -> Renderable ()
test3 otype = toRenderable layout
  where

    price1 = plot_fillbetween_style ^= solidFillStyle green1
           $ plot_fillbetween_values ^= [ (date d m y,(0,v2)) | (d,m,y,v1,v2) <- prices]
           $ defaultPlotFillBetween 

    price2 = plot_fillbetween_style ^= solidFillStyle red1
           $ plot_fillbetween_values ^= [ (date d m y,(0,v1)) | (d,m,y,v1,v2) <- prices]
           $ defaultPlotFillBetween

    layout = layout1_title ^= "Price History"
 	   $ layout1_plots ^= [("price 1", Left (toPlot price1)),
                               ("price 2", Left (toPlot price2))]
           $ defaultLayout1

----------------------------------------------------------------------        
test4 :: Bool -> Bool -> OutputType -> Renderable ()
test4 xrev yrev otype = toRenderable layout
  where

    points = plot_points_style ^= filledCircles 3 red
           $ plot_points_values ^= [ (x, LogValue (10**x)) | x <- [0.5,1,1.5,2,2.5] ]
           $ defaultPlotPoints

    lines = plot_lines_values ^= [ [(x, LogValue (10**x)) | x <- [0,3]] ]
          $ defaultPlotLines

    layout = layout1_title ^= "Log/Linear Example"
           $ layout1_bottom_axis ^: laxis_title ^= "horizontal"
           $ layout1_bottom_axis ^: laxis_reverse ^= xrev
           $ layout1_left_axis ^: laxis_title ^= "vertical"
           $ layout1_left_axis ^: laxis_reverse ^= yrev
	   $ layout1_plots ^= [("values",Left (toPlot points)),
			       ("values",Left (toPlot lines)) ]
           $ defaultLayout1

----------------------------------------------------------------------
-- Example thanks to Russell O'Connor

test5 :: OutputType -> Renderable ()
test5 otype = toRenderable (layout 1001 (trial bits) :: Layout1 Double LogValue)
  where
    bits = randoms $ mkStdGen 0

    layout n t = layout1_title ^= "Simulation of betting on a biased coin"
               $ layout1_plots ^= [
                      ("f=0.05", Left (toPlot (plot s1 n 0 (t 0.05)))),
                      ("f=0.1", Left (toPlot (plot s2 n 0 (t 0.1))))]
               $ defaultLayout1

    plot s n m t = plot_lines_style ^= s
                 $  plot_lines_values ^=
                       [[(fromIntegral x, LogValue y) | (x,y) <-
                         filter (\(x,_)->x `mod` (m+1)==0) $ take n $ zip [0..] t]]
                 $ defaultPlotLines 

    b = 0.1

    trial bits frac = scanl (*) 1 (map f bits)
      where 
        f True = (1+frac*(1+b))
        f False = (1-frac)

    s1 = solidLine lineWidth green
    s2 = solidLine lineWidth blue

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
         $ defaultPlotErrBars

    points = plot_points_style ^= filledCircles 2 red
	   $ plot_points_values ^= [(x,y) |  (x,y,dx,dy) <- vals]
           $ defaultPlotPoints

    layout = layout1_title ^= "errorbars example"
           $ layout1_plots ^= [("test",Left (toPlot bars)),
                               ("test",Left (toPlot points))]
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
-- a quick test to display labels with all combinations
-- of anchors
misc1 rot otype = fillBackground fwhite $ (gridToRenderable t)
  where
    t = weights (1,1) $ aboveN [ besideN [tval (lb h v) | h <- hs] | v <- vs ]
    lb h v = addMargins (20,20,20,20) $ fillBackground fblue $ crossHairs $ rlabel fs h v rot s
    s = "Labelling"
    hs = [HTA_Left, HTA_Centre, HTA_Right]
    vs = [VTA_Top, VTA_Centre, VTA_Bottom]
    fwhite = solidFillStyle white
    fblue = solidFillStyle (Color 0.8 0.8 1)
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
     , ("test2a", test2 prices)
     , ("test2b", test2 (filterPrices (date 1 1 2005) (date 31 12 2005)))
     , ("test2c", test2 (filterPrices (date 1 5 2005) (date 1 7 2005)))
     , ("test2d", test2 (filterPrices (date 1 1 2006) (date 10 1 2006)))
     , ("test3", test3)
     , ("test4a", test4 False False)
     , ("test4b", test4 True False)
     , ("test4c", test4 False True)
     , ("test5", test5)
     , ("test6", test6)
     , ("test7", test7)
     , ("test8", test8)
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
