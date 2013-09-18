
import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Backend.Diagrams
import qualified Graphics.Rendering.Chart.Renderable as R

import Diagrams.Core.Types ( renderDia )
import Diagrams.TwoD ( SizeSpec2D(..) )
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal

import Tests (tests)

main :: IO ()
main = (flip mapM_) tests $ \(name, w, h, draw) -> do
  render (name ++ ".svg") w h draw
  

render :: FilePath -> Int -> Int -> ChartBackend a -> IO ()
render f w h m = do
  env <- defaultEnv bitmapAlignmentFns (fromIntegral w) (fromIntegral h)
  renderableToEmbeddedFontSVGFile' ( R.Renderable 
                                   { R.minsize = return (fromIntegral w, fromIntegral h)
                                   , R.render = \(w,h) -> m >> return R.nullPickFn
                                   }) env f
  return ()