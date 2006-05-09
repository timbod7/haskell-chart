import System.Time
import qualified Graphics.Rendering.Cairo as C
import Graphics.Chart
import Graphics.Chart.GtkChart
import Prices


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


chart = layout 
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
        layout1_horizontal_axes=linkedAxes' (monthsAxis defaultAxis),
	layout1_vertical_axes=linkedAxes' (autoScaledAxis defaultAxis),
 	layout1_plots = [("price 1", HA_Bottom,VA_Left,(toPlot price1)),
                         ("price 2", HA_Bottom,VA_Left,(toPlot price2))]
    }

main = do
    renderableToWindow (toRenderable chart) 640 480
