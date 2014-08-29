{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.Contour
    (
    -- * Contour Plots
    PlotContours (PlotContours)
    , contourPlot
    , plot_contours_line_plots
    , plot_contours_levels
    , plot_contours_styles
    , plot_contours_title
    -- * Util
    , rgbaGradient
    ) where


--- Imports ---


-- Rendering --

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB.Linear

-- General --

import Numeric
import Control.Lens
import Control.Applicative
import Control.Monad
import Data.List
import Data.Default.Class

-- Qualified --

import qualified Data.Map as M


-- Unqualified --

import Data.Ord
import Data.Tuple (swap)


--- Util ---

rgbaGradient :: (Double, Double, Double,Double) -> (Double, Double, Double,Double) -> Int
    -> [AlphaColour Double]
{-| Returns an ordered list of colours useful for plotting. -}
rgbaGradient (rmn,gmn,bmn,amn) (rmx,gmx,bmx,amx) n =
    zipWith (flip withOpacity) [amn,amn + astp .. amx]
    $ zipWith3 rgb [rmn,rmn + rstp .. rmx] [gmn,gmn + gstp .. gmx] [bmn,bmn + bstp .. bmx]
    where rstp = (rmx - rmn) / fromIntegral n
          gstp = (gmx - gmn) / fromIntegral n
          bstp = (bmx - bmn) / fromIntegral n
          astp = (amx - amn) / fromIntegral n


-- Contour Plot --

data PlotContours x y = PlotContours
    { _plot_contours_line_plots :: [PlotLines x y]
    , _plot_contours_levels :: [Double]
    , _plot_contours_styles :: [LineStyle]
    , _plot_contours_title :: String }
$( makeLenses ''PlotContours )

instance ToPlot PlotContours where
    toPlot plt =
        let plts = plt ^. plot_contours_line_plots
            mn = minimum $ plt ^. plot_contours_levels
            mx = maximum $ plt ^. plot_contours_levels
            ttl = plt ^. plot_contours_title
            strs = if ttl == ""
                       then repeat ""
                       else ["Min " ++ ttl ++ ": " ++ showFFloat (Just 2) mn ""]
                            ++ replicate (length plts -2) ""
                            ++ ["Max " ++ ttl ++ ": " ++ showFFloat (Just 2) mx ""]
            stls = plt ^. plot_contours_styles
                       --else [ line_color ^= clr $ (head $ plt ^. plot_contours_styles)
                       --     | clr <- plt ^. plot_contours_colours ]
        in foldl1 joinPlot $ toPlot <$> zipWith3 zipperfun strs stls plts
        where zipperfun str stl lplt =
                  plot_lines_title .~ str
                  $ plot_lines_style .~ stl
                  $ lplt

-- Plotting Functions --

singleContourPlot :: [[Double]] -> (Double,Double) -> (Double,Double)
    -> Double -> PlotLines Double Double
singleContourPlot lsts mxs stps isolvl =
    linePlot (linksToLines mxs stps <$> listsToLinks lsts isolvl) "" black
    where linePlot lns ttl clr =
              plot_lines_values .~ lns
              $ plot_lines_title .~ ttl
              $ plot_lines_style .~ solidLine 2 (opaque clr)
              $ def

contourPlot
    :: (Double,Double) -- ^ The range along the x axis.
    -> (Double,Double) -- ^ The range along the y axis.
    -> Int -- ^ The number of points to sample along the x axis.
    -> Int -- ^ The number of points to sample along the y axis.
    -> Int -- ^ The number of isolevels.
    -> (Double -> Double -> Double) -- ^ The function to contour.
    -> PlotContours Double Double -- ^ The contour plot.
{-| Given various parameters and a function to analyze, contourPlot generates the default
    PlotContours. -}
contourPlot (xmn,xmx) (ymn,ymx) nx ny nlvls f =
    let lsts = functionToLists f (xmn,ymn) (xmx,ymx) stps
        stps = ((xmx - xmn) / fromIntegral nx,(ymx - ymn) / fromIntegral ny)
        (mn,mx) = (minimum $ minimum <$> lsts,maximum $ maximum <$> lsts)
        isostp = (mx - mn) / fromIntegral nlvls
        isolvls = take nlvls [mn + (isostp / 2),mn + (isostp * 1.5)..]
        plts = singleContourPlot lsts (xmx,ymx) stps <$> isolvls
    in PlotContours { _plot_contours_line_plots = plts
                    , _plot_contours_levels = isolvls
                    , _plot_contours_styles = replicate nlvls $ solidLine 2 (opaque black)
                    , _plot_contours_title = "" }


--- Contour Plot Internal ---


-- Types --

type Link = (Int,Int)
type ContourPair = (Link,Link)
data ContourBox =
    {- Styled like the indicies of a 3x3 Matrix. The lines are drawn from
       left to right, and if need be, top to bottom -}
    Empty
    | Line ContourPair
    | DLine ContourPair ContourPair
    deriving (Show,Eq)

data PairMap = PM (M.Map Link Link) (M.Map Link Link) deriving Show

-- Type Functions --

contourBoxesToPairMap :: [ContourBox] -> PairMap
contourBoxesToPairMap clns =
    let cprs = concatMap toPairs clns
    in PM (M.fromList cprs) (M.fromList $ map swap cprs)
    where toPairs (Line prpr) = [prpr]
          toPairs (DLine lprpr rprpr) = [lprpr,rprpr]

deletePair :: ContourPair -> PairMap -> PairMap
{- Deletes a left oriented line segmented -}
deletePair (lpr,rpr) (PM lmp rmp) =
    let lmp' = if M.lookup lpr lmp == Just rpr then M.delete lpr lmp else lmp
        rmp' = if M.lookup rpr rmp == Just lpr then M.delete rpr rmp else rmp
    in PM lmp' rmp'

popLink :: Link -> PairMap -> (Maybe Link,PairMap)
popLink lnk pmp@(PM lmp rmp) =
    let lft = M.lookup lnk lmp
        rgt = M.lookup lnk rmp
    in case (lft,rgt) of
           (Just rlnk,_) -> (Just rlnk,deletePair (lnk,rlnk) pmp)
           (_,Just llnk) -> (Just llnk,deletePair (llnk,lnk) pmp)
           _ -> (Nothing,pmp)

-- Core Algorithm --

functionToLists :: (Double -> Double -> Double) -> (Double,Double) -> (Double,Double)
    -> (Double,Double) -> [[Double]]
functionToLists f (xmn,ymn) (xmx,ymx) (xstp,ystp) =
    map (uncurry f) <$> [ [ (x,y) | x <- [xmx,(xmx - xstp)..xmn] ] | y <- [ymx,(ymx - ystp)..ymn] ]

situationTable :: Double -> (((Bool,Double),(Bool,Double)),((Bool,Double),(Bool,Double))) -> ContourBox
situationTable _ (((True,_),(True,_)),((True,_),(True,_))) = Empty
situationTable _ (((True,_),(True,_)),((False,_),(True,_))) = Line ((1,0),(2,1))
situationTable _ (((True,_),(True,_)),((True,_),(False,_))) = Line ((2,1),(1,2))
situationTable _ (((True,_),(True,_)),((False,_),(False,_))) = Line ((1,0),(1,2))
situationTable _ (((True,_),(False,_)),((True,_),(True,_))) = Line ((0,1),(1,2))
situationTable isolvl (((True,ul),(False,ur)),((False,ll),(True,lr)))
    | sum [ul,ur,ll,lr] / 4 < isolvl = DLine ((1,0),(0,1)) ((2,1),(1,2))
    | otherwise = DLine ((1,0),(2,1)) ((0,1),(1,2))
situationTable _ (((True,_),(False,_)),((True,_),(False,_))) = Line ((0,1),(2,1))
situationTable _ (((True,_),(False,_)),((False,_),(False,_))) = Line ((1,0),(0,1))
situationTable _ (((False,_),(True,_)),((True,_),(True,_))) = Line ((1,0),(0,1))
situationTable _ (((False,_),(True,_)),((False,_),(True,_))) = Line ((0,1),(2,1))
situationTable isolvl (((False,ul),(True,ur)),((True,ll),(False,lr)))
    | sum [ul,ur,ll,lr] / 4 < isolvl = DLine ((1,0),(2,1)) ((0,1),(1,2))
    | otherwise = DLine ((1,0),(0,1)) ((2,1),(1,2))
situationTable _ (((False,_),(True,_)),((False,_),(False,_))) = Line ((0,1),(1,2))
situationTable _ (((False,_),(False,_)),((True,_),(True,_))) = Line ((1,0),(1,2))
situationTable _ (((False,_),(False,_)),((False,_),(True,_))) = Line ((2,1),(1,2))
situationTable _ (((False,_),(False,_)),((True,_),(False,_))) = Line ((1,0),(2,1))
situationTable _ (((False,_),(False,_)),((False,_),(False,_))) = Empty

listsToContourBoxes :: [[Double]] -> Double -> [ContourBox]
listsToContourBoxes lsts isolvl = do
    (stnrw,r) <- zip stnlsts [0..]
    (stn,c) <- zip stnrw [0..]
    let bx = situationTable isolvl stn
    guard (bx /= Empty)
    return $ repositionBox (r,c) bx
    where stnlsts = rowZipper $ elementPairs . map threshold <$> lsts
          threshold x = (x >= isolvl,x)
          elementPairs lst = zip lst $ tail lst
          rowZipper rws = uncurry zip <$> elementPairs rws
          repositionContourPair (r,c) ((lr,lc),(rr,rc)) =
              ((lr + 2 * r, lc + 2 * c),(rr + 2 * r, rc + 2 * c))
          repositionBox rc (Line pr) =
              Line $ repositionContourPair rc pr
          repositionBox rc (DLine pr1 pr2) =
              DLine (repositionContourPair rc pr1) (repositionContourPair rc pr2)

traceLinks :: ContourPair -> PairMap -> ([Link],PairMap)
traceLinks (llnk,rlnk) pmp =
    let (llnks,pmp') = tracer [] (Just llnk,pmp)
        (rlnks,pmp'') = tracer [] (Just rlnk,pmp')
    in (llnks ++ reverse rlnks,pmp'')
    where tracer lnks (Just lnk,pmp') =
              tracer (lnk:lnks) $ popLink lnk pmp'
          tracer lnks (Nothing,pmp') = (lnks,pmp')

popLinks :: PairMap -> Maybe ([Link],PairMap)
popLinks pmp@(PM lmp _)
    | M.null lmp = Nothing
    | otherwise =
        let cpr = M.findMin lmp
        in Just (traceLinks cpr $ deletePair cpr pmp)

listsToLinks :: [[Double]] -> Double -> [[(Int,Int)]]
listsToLinks lsts isolvl =
    unfoldr popLinks .  contourBoxesToPairMap $ listsToContourBoxes lsts isolvl

linksToLines :: (Double,Double) -> (Double,Double) -> [(Int,Int)] -> [(Double,Double)]
linksToLines (xmx,ymx) (xstp,ystp) lnks =
   (\(r,c) -> (xmx - fromIntegral c * xstp / 2,ymx - fromIntegral r * ystp / 2)) <$> lnks


