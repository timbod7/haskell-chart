import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Simple
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Gtk
import System.Environment(getArgs)
import System.Time
import System.Random
import Data.Time.Calendar
import Data.Time.LocalTime
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
test1 :: OutputType -> Renderable
test1 otype = toRenderable layout
  where
    am :: Double -> Double
    am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))

    sinusoid1 = defaultPlotLines {
	plot_lines_values = [[ (x,(am x)) | x <- [0,(0.5)..400]]],
        plot_lines_style = solidLine lineWidth blue
    }

    sinusoid2 = defaultPlotPoints {
        plot_points_style=filledCircles 2 red,
	plot_points_values = [ (x,(am x)) | x <- [0,7..400]]
    }

    layout = defaultLayout1 {
        layout1_title="Amplitude Modulation",			   
        layout1_horizontal_axes=linkedAxes (autoScaledAxis defaultAxis),
	layout1_vertical_axes=linkedAxes (autoScaledAxis defaultAxis),
	layout1_plots = [("am",Left (toPlot sinusoid1)),
			 ("am points", Left (toPlot sinusoid2))]
    }

    lineWidth = chooseLineWidth otype


test1a :: OutputType -> Renderable
test1a otype = toRenderable layout
  where
    am :: Double -> Double
    am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))

    sinusoid1 = defaultPlotLines {
	plot_lines_values = [[ (x,(am x)) | x <- [0,(0.5)..400]]],
        plot_lines_style = solidLine lineWidth blue
    }

    sinusoid2 = defaultPlotPoints {
        plot_points_style=filledCircles 2 red,
	plot_points_values = [ (x,(am x)) | x <- [0,7..400]]
    }

    lap = defaultLinearAxis{la_nLabels=2,la_nTicks=20,la_gridAtMinor=True}

    layout = defaultLayout1 {
        layout1_title="Amplitude Modulation",			   
        layout1_horizontal_axes=linkedAxes (autoScaledAxis' lap defaultAxis),
	layout1_vertical_axes=linkedAxes (autoScaledAxis' lap defaultAxis),
	layout1_plots = [("am",Left (toPlot sinusoid1)),
			 ("am points", Left (toPlot sinusoid2))]
    }

    lineWidth = chooseLineWidth otype

----------------------------------------------------------------------
test2 :: [(Int,Int,Int,Double,Double)] -> OutputType -> Renderable
test2 prices otype = toRenderable layout
  where

    lineStyle c = (plot_lines_style defaultPlotLines){
                   line_width=3 * chooseLineWidth otype,
                   line_color = c
                  }

    price1 = defaultPlotLines {
        plot_lines_style = lineStyle blue,
	plot_lines_values = [[ ((date d m y), v) | (d,m,y,v,_) <- prices]]
    }

    price2 = defaultPlotLines {
        plot_lines_style = lineStyle green,
	plot_lines_values = [[ ((date d m y), v) | (d,m,y,_,v) <- prices]]
    }

    baseAxis = defaultAxis{
        axis_grid=[],
        axis_line_style=solidLine 1 fg,
        axis_grid_style=solidLine 1 fg1,
        axis_label_style=(axis_label_style defaultAxis){font_color=fg}
    }

    vaxis = autoScaledAxis baseAxis
    bg = Color 0 0 0.25
    fg = Color 1 1 1
    fg1 = Color 0.0 0.0 0.15

    layout = defaultLayout1 {
        layout1_title="Price History",
        layout1_title_style=(layout1_title_style defaultLayout1){font_color=fg},
        layout1_background=solidFillStyle bg,
        layout1_horizontal_axes=linkedAxes' (autoTimeAxis baseAxis),
	layout1_vertical_axes=independentAxes vaxis vaxis,
 	layout1_plots = [("price 1", Left (toPlot price1)),
                         ("price 2", Right (toPlot price2))],
        layout1_legend = Just (defaultLegendStyle{
            legend_label_style=(legend_label_style defaultLegendStyle){font_color=fg}
        } ),
        layout1_grid_last=False
    }

date dd mm yyyy = (LocalTime (fromGregorian (fromIntegral yyyy) mm dd) midnight)

----------------------------------------------------------------------
test3 :: OutputType -> Renderable
test3 otype = toRenderable layout
  where

    price1 = defaultPlotFillBetween {
        plot_fillbetween_style = solidFillStyle green1,
	plot_fillbetween_values = [ (date d m y,(0,v2)) | (d,m,y,v1,v2) <- prices]
    }

    price2 = defaultPlotFillBetween {
        plot_fillbetween_style = solidFillStyle red1,
	plot_fillbetween_values = [ (date d m y,(0,v1)) | (d,m,y,v1,v2) <- prices]
    }

    layout = defaultLayout1 {
        layout1_title="Price History",			   
        layout1_horizontal_axes=linkedAxes' (autoTimeAxis defaultAxis),
	layout1_vertical_axes=linkedAxes' (autoScaledAxis defaultAxis),
 	layout1_plots = [("price 1", Left (toPlot price1)),
                         ("price 2", Left (toPlot price2))]
    }

----------------------------------------------------------------------        
test4 :: OutputType -> Renderable
test4 otype = toRenderable layout
  where

    points = defaultPlotPoints {
        plot_points_style=filledCircles 3 red,
	plot_points_values = [ (x, LogValue (10**x)) | x <- [0.5,1,1.5,2,2.5] ]
    }

    lines = defaultPlotLines {
	plot_lines_values = [ [(x, LogValue (10**x)) | x <- [0,3]] ]
    }

    layout = defaultLayout1 {
        layout1_title="Log/Linear Example",			   
        layout1_horizontal_axes=linkedAxes' (autoScaledAxis defaultAxis{axis_title="horizontal"}),
	layout1_vertical_axes=linkedAxes' (autoScaledLogAxis defaultAxis{axis_title="vertical"}),
	layout1_plots = [("values",Left (toPlot points)),
			 ("values",Left (toPlot lines)) ]
    }


----------------------------------------------------------------------
-- Example thanks to Russell O'Connor

test5 :: OutputType -> Renderable
test5 otype = toRenderable (layout 1001 (trial bits))
  where
    bits = randoms $ mkStdGen 0
    layout n t = defaultLayout1 {
           layout1_title="Simulation of betting on a biased coin",			   
           layout1_horizontal_axes=linkedAxes (autoScaledAxis defaultAxis),
            layout1_vertical_axes=linkedAxes (autoScaledLogAxis defaultAxis),
            layout1_plots = [
             ("f=0.05", Left (toPlot (plot s1 n 0 (t 0.05)))),
             ("f=0.1", Left (toPlot (plot s2 n 0 (t 0.1))))]
        }

    plot s n m t = defaultPlotLines {
            plot_lines_style = s,
            plot_lines_values =
             [[(fromIntegral x, LogValue y) | (x,y) <-
                          filter (\(x,_)->x `mod` (m+1)==0) $ take n $ zip [0..] t]]
        }

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

test6 :: OutputType -> Renderable
test6 otype = toRenderable (plotLayout pp){layout1_title="Graphics.Rendering.Chart.Simple example"}
  where
    pp = plot xs sin "sin"
                 cos "cos" "o"
                 (sin.sin.cos) "sin.sin.cos" "."
                 (/3) "- "
                 (const 0.5)
                 [0.1,0.7,0.5::Double] "+"
    xs = [0,0.3..3] :: [Double]

----------------------------------------------------------------------
test7 :: OutputType -> Renderable
test7 otype = toRenderable layout
  where
    vals = [ (x,sin (exp x),sin x/2,cos x/10) | x <- [1..20]]
    bars = defaultPlotErrBars {
	plot_errbars_values = [symErrPoint x y dx dy | (x,y,dx,dy) <- vals]
    }
    points = defaultPlotPoints {
        plot_points_style=filledCircles 2 red,
	plot_points_values = [(x,y) |  (x,y,dx,dy) <- vals]
    }

    layout = defaultLayout1 {
        layout1_title= "errorbars example",
	layout1_plots = [("test",Left (toPlot bars)),
                         ("test",Left (toPlot points))]
    }

----------------------------------------------------------------------
test8 :: OutputType -> Renderable
test8 otype = toRenderable layout
  where
    values = [ ("eggs",38,e), ("milk",45,e), ("bread",11,e1), ("salmon",8,e) ]
    e = 0
    e1 = 25
    layout = defaultPieLayout {
        pie_title = "Pie Chart Example",
        pie_plot = defaultPieChart {
            pie_data = [ defaultPieItem{pitem_value=v,pitem_label=s,pitem_offset=o} 
                         | (s,v,o) <- values ]
        }
    }

----------------------------------------------------------------------
-- a quick test to display labels with all combinations
-- of anchors
misc1 rot otype = fillBackground fwhite $ grid [1,1,1] [1,1,1] ls
  where
    ls = [ [(0,addMargins (20,20,20,20) $ fillBackground fblue $ crossHairs $ rlabel fs h v rot s) | h <- hs] | v <- vs ]
    s = "Labelling"
    hs = [HTA_Left, HTA_Centre, HTA_Right]
    vs = [VTA_Top, VTA_Centre, VTA_Bottom]
    fwhite = solidFillStyle white
    fblue = solidFillStyle (Color 0.8 0.8 1)
    fs = defaultFontStyle{font_size=20,font_weight=C.FontWeightBold}
    crossHairs r =Renderable {
      minsize = minsize r,
      render = \rect@(Rect (Point x1 y1) (Point x2 y2)) -> do
          let xa = (x1 + x2) / 2
          let ya = (y1 + y2) / 2
          strokeLines [Point x1 ya,Point x2 ya]
          strokeLines [Point xa y1,Point xa y2]
          render r rect
    }
    

----------------------------------------------------------------------        
allTests :: [ (String, OutputType -> Renderable) ]
allTests =
     [ ("test1",  test1)
     , ("test1a", test1a)
     , ("test2a", test2 prices)
     , ("test2b", test2 (filterPrices (date 1 1 2005) (date 31 12 2005)))
     , ("test2c", test2 (filterPrices (date 1 5 2005) (date 1 7 2005)))
     , ("test2d", test2 (filterPrices (date 1 1 2006) (date 10 1 2006)))
     , ("test3", test3)
     , ("test4", test4)
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

showTests :: [String] -> ((String,OutputType -> Renderable) -> IO()) -> IO ()
showTests tests ofn = mapM_ ofn (filter (match tests) allTests)

match :: [String] -> (String,a) -> Bool
match [] t = True
match ts t = (fst t) `elem` ts

renderToWindow (n,ir) = renderableToWindow (ir Window) 640 480
renderToPNG (n,ir) = renderableToPNGFile (ir PNG) 640 480 (n ++ ".png")
renderToPS (n,ir) = renderableToPSFile (ir PS) 640 480 (n ++ ".ps")
renderToPDF (n,ir) = renderableToPDFFile (ir PDF) 640 480 (n ++ ".pdf")
renderToSVG (n,ir) = renderableToSVGFile (ir SVG) 640 480 (n ++ ".svg")
