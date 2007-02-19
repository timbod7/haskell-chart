import System.Time
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
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

    price1 = defaultPlotLines {
        plot_lines_style = solidLine 1 0 0 1,
	plot_lines_values = [[ Point (date d m y) v | (d,m,y,v,_) <- prices]]
    }

    price2 = defaultPlotLines {
        plot_lines_style = solidLine 1 0 1 0,
	plot_lines_values = [[ Point (date d m y) v | (d,m,y,_,v) <- prices]]
    }

    layout = defaultLayout1 {
        layout1_title="Price History",			   
        layout1_horizontal_axes=linkedAxes' (monthsAxis defaultAxis),
	layout1_vertical_axes=independentAxes (autoScaledAxis defaultAxis) (autoScaledAxis defaultAxis),
 	layout1_plots = [("price 1", HA_Bottom,VA_Left,(toPlot price1)),
                         ("price 2", HA_Bottom,VA_Right,(toPlot price2))]
    }

main = do
    renderableToWindow (toRenderable chart) 640 480
