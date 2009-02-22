-- Quick script to plot darcs patches over time, by user.
-- relies on the following non-standard packages from hackage:
--     xml
--    data-accessor      
--    Chart

import System.Process
import System.IO
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Format
import Data.Accessor
import Data.List(sort,nub)
import qualified Data.Map as Map
import qualified Text.XML.Light as X
import qualified Text.XML.Light.Input as X
import qualified Text.XML.Light.Proc as X
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk

type PatchInfo = (LocalTime, String, String)
getDarcsPatches :: IO [PatchInfo]
getDarcsPatches = do
    (hi,ho,he,ph) <- runInteractiveCommand ( "darcs changes --xml" )
    s <- hGetContents ho
    let (Just xml) = X.parseXMLDoc s
    let patches = X.findChildren (qname "patch") xml
    return (map patchDetails patches)
  where
    patchDetails xml = (date,author,title)
      where
        (Just author) = X.findAttr (qname "author") xml

        (Just date0) = X.findAttr (qname "date") xml
        yyyy = read $ take 4 $ drop 0 $ date0
        mm = read $ take 2 $ drop 4 $ date0
        dd = read $ take 2 $ drop 6 $ date0
        date = LocalTime (fromGregorian yyyy mm dd) midnight

        (Just title0) = X.findChild (qname "name") xml
        title = X.strContent title0            

    qname s = X.blank_name{X.qName=s}

countPatches :: Ord k => (PatchInfo->k) -> [PatchInfo] -> Map.Map k Int
countPatches f = foldr f1 Map.empty
  where
    f1 p m = Map.insertWith (+) (f p) (1::Int) m


main = do
    patches <- getDarcsPatches
    renderableToWindow (toRenderable (plotPatches patches)) 800 600

plotPatches patches = layout
  where
    layout = layout1_title ^= "Patch history by user"
           $ layout1_plots ^= [ Left (plotBars bars) ]
           $ defaultLayout1 :: Layout1 LocalTime Double
        
    bars = plot_bars_titles ^= users
         $ plot_bars_style ^= BarsStacked
         $ plot_bars_values ^= barvs
         $ defaultPlotBars

    allUsers = map snd $ reverse $ sort [ (n,u) | (u,n) <- Map.toList countPerUser]
    topUsers = take 3 $ allUsers
    countPerUser = countPatches (\(date,user,comment)->user) patches
    nTotal = sum (Map.elems countPerUser)

    summary :: Map.Map (LocalTime,String) Int
    summary = countPatches (\(date,user,comment)->(month date,mapUser user)) patches

    users = if length topUsers < length allUsers then topUsers ++ ["other"] else allUsers
    months = nub $ map fst $ Map.keys summary
    barvs = [ (m,[count m u|u <- users]) | m <- months]
    count m u = fromIntegral (Map.findWithDefault 0 (m,u) summary)

    mapUser u = if u `elem` topUsers then u else "other"
    month t = t{localDay=fromGregorian y m 15}
      where
        (y,m,day) = toGregorian (localDay t)



