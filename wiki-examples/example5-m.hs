import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo


values :: [ (String,Double,Bool) ]
values = [ ("Mexico City",19.2,False), ("Mumbai",12.9,False), ("Sydney",4.3,False), ("London",8.3,False), ("New York",8.2,True) ]

pitem (s,v,o) = pitem_value .~ v
              $ pitem_label .~ s
              $ pitem_offset .~ (if o then 25 else 0)
              $ def

main = toFile def "example5_big.png" $ do
  pie_title .= "Relative Population"
  pie_plot . pie_data .= map pitem values

  
