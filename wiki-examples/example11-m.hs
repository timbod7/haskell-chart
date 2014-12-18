import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

titles = ["Cash","Equity"]

values :: [ (String,[Double]) ]
values =
  [ ("Jun", [20,45])
  , ("Jul", [45,30])
  , ("Aug", [30,20])
  , ("Sep", [10,40])
  , ("Oct", [20,50])
  ]

main = toFile def "example11_big.png" $ do
    layout_title .= "Sample Bars"
    layout_title_style . font_size .= 10
    layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
    plot $ fmap plotBars $ bars titles (addIndexes (map snd values))
  
