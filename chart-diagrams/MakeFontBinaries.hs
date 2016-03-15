-- {-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
module MakeFontBinaries (main) where

import Data.Monoid ((<>))
import qualified Graphics.SVGFonts.ReadFont as ReadFont
import qualified Data.ByteString.Lazy as ByteString.Lazy
-- import qualified Data.Binary as Binary
import qualified Data.Serialize as Serialize

-- import GHC.Generics (Generic)
-- import Diagrams.Prelude (Path, V2)

import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Backend.Serialize

main :: IO ()
main = mapM_ go namesFilepaths

namesFilepaths :: [(String, String)]
namesFilepaths =
  [ ("SourceSansPro_R",   "fonts/SourceSansPro_R.svg")
  , ("SourceSansPro_RB",  "fonts/SourceSansPro_RB.svg")
  , ("SourceSansPro_RBI", "fonts/SourceSansPro_RBI.svg")
  , ("SourceSansPro_RI",  "fonts/SourceSansPro_RI.svg")
  ]

go :: (String, FilePath) -> IO ()
go (name, filepath) = do
  bs <- ByteString.Lazy.toStrict <$> ByteString.Lazy.readFile filepath
  let
    font :: ReadFont.PreparedFont Double
    font = snd (ReadFont.loadFont' name bs)
  -- Serialize.encodeFile ("fonts/" <> name <> ".bin") font
  ByteString.Lazy.writeFile ("fonts/" <> name <> ".cereal.bin") (Serialize.encodeLazy font)
