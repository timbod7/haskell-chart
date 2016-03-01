import Control.Monad(void)
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Backend.Cairo(renderableToFile)

-- haskell black scholes (see http://www.espenhaug.com/black_scholes.html)

blackScholesCall :: Double -> Double -> Double -> Double -> Double -> Double
blackScholesCall s x t r v = s * normcdf d1 - x*exp (-r*t) * normcdf d2
  where
    d1 = ( log(s/x) + (r+v*v/2)*t )/(v*sqrt t)
    d2 = d1 - v*sqrt t

normcdf :: Double -> Double
normcdf x | x < 0 = 1 - w
          | otherwise = w
  where
    w = 1.0 - 1.0 / sqrt (2.0 * pi) * exp(-l*l / 2.0) * (a1 * k + a2 * (k**2) + a3 * (k**3) + a4 * (k**4) + a5 * (k**5))
    k = 1.0 / (1.0 + 0.2316419 * l)
    l = abs x
    a1 = 0.31938153
    a2 = -0.356563782
    a3 = 1.781477937
    a4 = -1.821255978
    a5 = 1.330274429


-- Construct a single chart for the grid
bsChart :: Double -> Double -> Double -> Layout Double Double
bsChart t r v = execEC $ do
    layout_y_axis . laxis_generate .= scaledAxis def (-10,80)
    plot $ line "" [[(s,blackScholesCall s 100 0.001 r v) | s <- ss]]
    plot $ line lbl [[(s,blackScholesCall s 100 t r v) | s <- ss]]
  where    
    ss = [50,51..150]
    lbl = "t = " ++ show t ++ ", r = " ++ show r

-- Construct a grid of charts, with a single title accross the top
grid = title `wideAbove` aboveN [ besideN [ layoutToGrid (bsChart t r v) | t <-ts ] | r <- rs ]
  where
    ts = [1,2,5]
    rs = [0.05,0.10,0.20]
    v = 0.10
    title = setPickFn nullPickFn $ label ls HTA_Centre VTA_Centre "Black Scholes Option Values"
    ls = def { _font_size   = 15 , _font_weight = FontWeightBold }
    
main :: IO ()
main = do
  void $ renderableToFile def "example13_big.png" $ fillBackground def $ gridToRenderable $  grid
