module Test8 where 

import Graphics.Rendering.Chart
import Data.Accessor
import Data.Default

import Utils

chart :: (ChartBackend m) => Renderable m ()
chart = mapPickFn (const ()) $ pieToRenderable layout
  where
    values = [ ("eggs",38,e), ("milk",45,e), ("bread",11,e1), ("salmon",8,e) ]
    e = 0
    e1 = 25
    layout = pie_title ^= "Pie Chart Example"
           $ pie_plot ^: pie_data ^= [ def{pitem_value_=v,pitem_label_=s,pitem_offset_=o}
                                       | (s,v,o) <- values ]
           $ def

main = main' "test8" chart


