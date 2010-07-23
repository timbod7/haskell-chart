module Test8 where 

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Data.Accessor
import System.Environment(getArgs)

chart :: Renderable ()
chart = toRenderable layout
  where
    values = [ ("eggs",38,e), ("milk",45,e), ("bread",11,e1), ("salmon",8,e) ]
    e = 0
    e1 = 25
    layout = pie_title ^= "Pie Chart Example"
           $ pie_plot ^: pie_data ^= [ defaultPieItem{pitem_value_=v,pitem_label_=s,pitem_offset_=o}
                                       | (s,v,o) <- values ]
           $ defaultPieLayout

main1 :: [String] -> IO (PickFn ())
main1 ["small"]  = renderableToPNGFile chart 320 240 "test8_small.png"
main1 ["big"]    = renderableToPNGFile chart 800 600 "test8_big.png"
main1 _          = renderableToWindow  chart 640 480 >> return undefined

main = getArgs >>= main1


