import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Simple
import Graphics.Rendering.Chart.Gtk
import System.Environment(getArgs)
import System.Time
import System.Random
import Prices

data OutputType = Window | PNG | PS | PDF

chooseLineWidth Window = 1.0
chooseLineWidth PNG = 1.0
chooseLineWidth PDF = 0.25
chooseLineWidth PS = 0.25

blue = (Color 0 0 1)
green = (Color 0 1 0)
green1 = (Color 0.5 1 0.5)
red = (Color 1 0 0)
red1 = (Color 0.5 0.5 1)

----------------------------------------------------------------------
test1 :: OutputType -> IO Renderable
test1 otype = return (toRenderable layout)
  where
    am :: Double -> Double
    am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))

    sinusoid1 = defaultPlotLines {
	plot_lines_values = [[ (Point x (am x)) | x <- [0,(0.5)..400]]],
        plot_lines_style = solidLine lineWidth blue
    }

    sinusoid2 = defaultPlotPoints {
        plot_points_style=filledCircles 2 red,
	plot_points_values = [ (Point x (am x)) | x <- [0,7..400]]
    }

    layout = defaultLayout1 {
        layout1_title="Amplitude Modulation",			   
        layout1_horizontal_axes=linkedAxes (autoScaledAxis defaultAxis),
	layout1_vertical_axes=linkedAxes (autoScaledAxis defaultAxis),
	layout1_plots = [("am",HA_Bottom,VA_Left,(toPlot sinusoid1)),
			 ("am points", HA_Bottom,VA_Left,(toPlot sinusoid2))]
    }

    lineWidth = chooseLineWidth otype


test1a :: OutputType -> IO Renderable
test1a otype = return (toRenderable layout)
  where
    am :: Double -> Double
    am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))

    sinusoid1 = defaultPlotLines {
	plot_lines_values = [[ (Point x (am x)) | x <- [0,(0.5)..400]]],
        plot_lines_style = solidLine lineWidth blue
    }

    sinusoid2 = defaultPlotPoints {
        plot_points_style=filledCircles 2 red,
	plot_points_values = [ (Point x (am x)) | x <- [0,7..400]]
    }

    lap = defaultLinearAxis{la_nLabels=2,la_nTicks=20,la_gridAtMinor=True}

    layout = defaultLayout1 {
        layout1_title="Amplitude Modulation",			   
        layout1_horizontal_axes=linkedAxes (autoScaledAxis' lap defaultAxis),
	layout1_vertical_axes=linkedAxes (autoScaledAxis' lap defaultAxis),
	layout1_plots = [("am",HA_Bottom,VA_Left,(toPlot sinusoid1)),
			 ("am points", HA_Bottom,VA_Left,(toPlot sinusoid2))]
    }

    lineWidth = chooseLineWidth otype

----------------------------------------------------------------------
test2 :: [(Int,Int,Int,Double,Double)] -> OutputType -> IO Renderable
test2 prices otype = return (toRenderable layout)
  where

    price1 = defaultPlotLines {
        plot_lines_style = solidLine lineWidth blue,
	plot_lines_values = [[ Point (date d m y) v | (d,m,y,v,_) <- prices]]
    }

    price2 = defaultPlotLines {
        plot_lines_style = solidLine lineWidth green,
	plot_lines_values = [[ Point (date d m y) v | (d,m,y,_,v) <- prices]]
    }

    gridlessAxis = defaultAxis{axis_grid=[]}
    vaxis = autoScaledAxis gridlessAxis

    layout = defaultLayout1 {
        layout1_title="Price History",			   
        layout1_horizontal_axes=linkedAxes' (autoTimeAxis gridlessAxis),
	layout1_vertical_axes=independentAxes vaxis vaxis,
 	layout1_plots = [("price 1", HA_Bottom,VA_Left,(toPlot price1)),
                         ("price 2", HA_Bottom,VA_Right,(toPlot price2))]
    }

    lineWidth = chooseLineWidth otype

date dd mm yyyy = doubleFromClockTime ct
  where
    ct = toClockTime CalendarTime {
    ctYear=yyyy,
    ctMonth=toEnum (mm-1),
    ctDay=dd,
    ctHour=0,
    ctMin=0,
    ctSec=0,
    ctPicosec=0,
    ctTZ=0,
    ctWDay=Monday,
    ctYDay=0,
    ctTZName="",
    ctIsDST=False
    }

----------------------------------------------------------------------
test3 :: OutputType -> IO Renderable
test3 otype = return (toRenderable layout)
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
 	layout1_plots = [("price 1", HA_Bottom,VA_Left,(toPlot price1)),
                         ("price 2", HA_Bottom,VA_Left,(toPlot price2))]
    }

----------------------------------------------------------------------        
test4 :: OutputType -> IO Renderable
test4 otype = return (toRenderable layout)
  where

    points = defaultPlotPoints {
        plot_points_style=filledCircles 3 red,
	plot_points_values = [ Point x (10**x) | x <- [0.5,1,1.5,2,2.5] ]
    }

    lines = defaultPlotLines {
	plot_lines_values = [ [Point x (10**x) | x <- [0,3]] ]
    }

    layout = defaultLayout1 {
        layout1_title="Log/Linear Example",			   
        layout1_horizontal_axes=linkedAxes' (autoScaledAxis defaultAxis{axis_title="horizontal"}),
	layout1_vertical_axes=linkedAxes' (autoScaledLogAxis defaultAxis{axis_title="vertical"}),
	layout1_plots = [("values",HA_Bottom,VA_Left,(toPlot points)),
			 ("values",HA_Bottom,VA_Left,(toPlot lines)) ]
    }


----------------------------------------------------------------------
-- Example thanks to Russell O'Connor

test5 :: OutputType -> IO Renderable
test5 otype = do
    bits <- fmap randoms getStdGen
    return (toRenderable (layout 1001 (trial bits)))
  where
    layout n t = defaultLayout1 {
           layout1_title="Simulation of betting on a biased coin",			   
           layout1_horizontal_axes=linkedAxes (autoScaledAxis defaultAxis),
            layout1_vertical_axes=linkedAxes (autoScaledLogAxis defaultAxis),
            layout1_plots = [
             ("f=0.05",HA_Bottom,VA_Left,(toPlot (plot s1 n 0 (t 0.05)))),
             ("f=0.1",HA_Bottom,VA_Left,(toPlot (plot s2 n 0 (t 0.1))))]
        }

    plot s n m t = defaultPlotLines {
            plot_lines_style = s,
            plot_lines_values =
             [[Point (fromIntegral x) y | (x,y) <-
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

test6 :: OutputType -> IO Renderable
test6 otype = return (toRenderable pp{layout1_title="Graphics.Rendering.Chart.Simple example"})
  where
    pp = plot xs sin "sin"
                 cos "cos" "o"
                 (sin.sin.cos) "sin.sin.cos" "."
                 (/3) "- "
                 (const 0.5)
                 [0.1,0.7,0.5::Double] "+"
    xs = [0,0.3..3] :: [Double]

----------------------------------------------------------------------
test7 :: OutputType -> IO Renderable
test7 otype = return (toRenderable layout)
  where
    vals = [ (x,sin (exp x),sin x/2,cos x/10) | x <- [1..20]]
    bars = defaultPlotErrBars {
	plot_errbars_values = [ErrPoint x y dx dy | (x,y,dx,dy) <- vals]
    }
    points = defaultPlotPoints {
        plot_points_style=filledCircles 2 red,
	plot_points_values = [Point x y |  (x,y,dx,dy) <- vals]
    }

    layout = defaultLayout1 {
        layout1_title= "errorbars example",
	layout1_plots = [("test",HA_Bottom,VA_Left,(toPlot bars)),
                         ("test",HA_Bottom,VA_Left,(toPlot points))]
    }

----------------------------------------------------------------------
test8 :: OutputType -> IO Renderable
test8 otype = return (toRenderable layout)
  where
    values = [ ("eggs",38,e), ("milk",45,e), ("bread",11,e1), ("salmon",8,e) ]
    e = 0
    e1 = 15
    layout = defaultPieLayout {
        pie_title = "Pie Chart Example",
        pie_plot = defaultPieChart {
            pie_data = [ defaultPieItem{pitem_value=v,pitem_label=s,pitem_offset=o} 
                         | (s,v,o) <- values ]
        }
    }

----------------------------------------------------------------------        
allTests :: [ (String, OutputType -> IO Renderable) ]
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
     ]

filterPrices t1 t2 = [ v | v@(d,m,y,_,_) <- prices, let t = date d m y in t >= t1 && t <= t2]

main = do
    args <- getArgs
    main1 args

main1 :: [String] -> IO ()
main1 ("--png":tests) = showTests tests renderToPNG
main1 ("--pdf":tests) = showTests tests renderToPDF
main1 ("--ps":tests) = showTests tests renderToPS
main1 tests = showTests tests renderToWindow

showTests :: [String] -> ((String,OutputType -> IO Renderable) -> IO()) -> IO ()
showTests tests ofn = mapM_ ofn (filter (match tests) allTests)

match :: [String] -> (String,a) -> Bool
match [] t = True
match ts t = (fst t) `elem` ts

renderToWindow (n,ir) = do { r <- ir Window; renderableToWindow r 640 480}
renderToPNG (n,ir) = do { r <- ir PNG; renderableToPNGFile r 640 480 (n ++ ".png")}
renderToPS (n,ir) = do { r <- ir PS; renderableToPSFile r 640 480 (n ++ ".ps")}
renderToPDF (n,ir) = do { r <- ir PDF; renderableToPDFFile r 640 480 (n ++ ".pdf")}
