module Test8 where 

import Graphics.Rendering.Chart
import Control.Lens
import Data.Default.Class

import Utils

chart :: Renderable ()
chart = mapPickFn (const ()) $ pieToRenderable layout
  where
    values = [ ("eggs",38,e), ("milk",45,e), ("bread",11,e1), ("salmon",8,e) ]
    e = 0
    e1 = 25
    layout = pie_title .~ "Pie Chart Example"
           $ pie_plot . pie_data .~ [ def{_pitem_value=v,_pitem_label=s,_pitem_offset=o}
                                      | (s,v,o) <- values ]
           $ pie_plot . pie_label_style .~ (font_size .~ 20 $ def)
           $ def

-- main = main' "test8" chart


