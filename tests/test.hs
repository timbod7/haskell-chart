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

----------------------------------------------------------------------
test1 :: OutputType -> IO Layout1
test1 otype = return layout 
  where
    am :: Double -> Double
    am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))

    sinusoid1 = defaultPlotLines {
	plot_lines_values = [[ (Point x (am x)) | x <- [0,(0.5)..400]]],
        plot_lines_style = solidLine lineWidth 0 0 1
    }

    sinusoid2 = defaultPlotPoints {
        plot_points_style=filledCircles 2 1 0 0,
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

----------------------------------------------------------------------
test2 :: OutputType -> [(Int,Int,Int,Double,Double)] -> IO Layout1
test2 otype prices = return layout 
  where

    price1 = defaultPlotLines {
        plot_lines_style = solidLine lineWidth 0 0 1,
	plot_lines_values = [[ Point (date d m y) v | (d,m,y,v,_) <- prices]]
    }

    price2 = defaultPlotLines {
        plot_lines_style = solidLine lineWidth 0 1 0,
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
test3 :: OutputType -> IO Layout1
test3 otype = return layout 
  where

    price1 = defaultPlotFillBetween {
        plot_fillbetween_style = solidFillStyle 0.5 1 0.5,
	plot_fillbetween_values = [ (date d m y,(0,v2)) | (d,m,y,v1,v2) <- prices]
    }

    price2 = defaultPlotFillBetween {
        plot_fillbetween_style = solidFillStyle 0.5 0.5 1,
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
test4 :: OutputType -> IO Layout1
test4 otype = return layout 
  where

    points = defaultPlotPoints {
        plot_points_style=filledCircles 3 1 0 0,
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

test5 :: OutputType -> IO Layout1
test5 otype = do
    bits <- fmap randoms getStdGen
    return (layout 1001 (trial bits))
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

    s1 = solidLine lineWidth 0 1 0
    s2 = solidLine lineWidth 0 0 1

    lineWidth = chooseLineWidth otype


----------------------------------------------------------------------        
-- Test the Simple interface

test6 :: OutputType -> IO Layout1
test6 otype = return pp{layout1_title="Graphics.Rendering.Chart.Simple example"}
  where
    pp = plot xs sin "sin"
                 cos "cos" "o"
                 (sin.sin.cos) "sin.sin.cos" "."
                 (/3) "- "
                 (const 0.5)
                 [0.1,0.7,0.5::Double] "+"
    xs = [0,0.3..3] :: [Double]
----------------------------------------------------------------------        
allTests =
     [ ("test1",test1)
     , ("test2a",\ot -> test2 ot prices)
     , ("test2b",\ot -> test2 ot (filterPrices (date 1 1 2005) (date 31 12 2005)))
     , ("test2c",\ot -> test2 ot (filterPrices (date 1 1 2006) (date 10 1 2006)))
     , ("test3",test3)
     , ("test4",test4)
     , ("test5",test5)
     , ("test6",test6)
     ]

filterPrices t1 t2 = [ v | v@(d,m,y,_,_) <- prices, let t = date d m y in t >= t1 && t <= t2]

main = do
    args <- getArgs
    main1 args

main1 ("--png":tests) = showTests tests renderToPNG
main1 ("--pdf":tests) = showTests tests renderToPDF
main1 ("--ps":tests) = showTests tests renderToPS
main1 tests = showTests tests renderToWindow

showTests tests outputfn = mapM_ outputfn (filter (match tests) allTests)

match [] t = True
match ts t = (fst t) `elem` ts

renderToWindow (n,t) = t Window >>= \l -> renderableToWindow (toRenderable l) 640 480
renderToPNG (n,t) = t PNG >>= \l -> renderableToPNGFile (toRenderable l) 640 480 (n ++ ".png")
renderToPS (n,t) = t PS >>= \l -> renderableToPSFile (toRenderable l) 640 480 (n ++ ".ps")
renderToPDF (n,t) = t PDF >>= \l -> renderableToPDFFile (toRenderable l) 640 480 (n ++ ".pdf")
