-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Simple
-- Copyright   :  (c) David Roundy 2007
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- An even simpler framework for creating 2D charts in Haskell.
--
-- The basic idea is to make it as easy to plot as octave, which means that
-- you provide no more information than you wish to provide.  We provide
-- four plotting functions, which differ only in their output.  One
-- produces a "Layout1" that you can customize using other
-- Graphics.Rendering.Chart functions.  The other three produce their
-- output directly.  All three accept the same input (except for the
-- filename required by plotPDF and plotPS), and produce the same plots.
--
-- The plot functions accept a variable number of arguments.  You must
-- provide a [Double] which defines the points on the x axis, which must
-- precede any of the "y" values.  The y values may either be [Double] or
-- functions.  After any given y value, you can give either Strings or
-- PlotKinds describing how you'd like that y printed.
--
-- Examples:
--
-- @renderableToWindow (toRenderable $ plotLayout $ plot [0,0.1..10] sin "sin(x)") 640 480@
--
-- @plotWindow [0,1,3,4,8]] [12,15,1,5,8] "o" "points"@
--
-- @plotPDF "foo.pdf" [0,0.1..10] sin "- " cos ". " cos "o"@
--
-- @plotPS "foo.ps" [0,0.1..10] (sin.exp) "- " (sin.exp) "o-"@
-----------------------------------------------------------------------------
module Graphics.Rendering.Chart.Simple( plot, PlotKind(..), xcoords,
                                        plotWindow, plotPDF, plotPS,
                                        plotLayout
                                      ) where

import Data.Maybe ( catMaybes )

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Types (isValidNumber)
import Graphics.Rendering.Chart.Gtk

styleColor :: Int -> Color
styleColor ind = colorSequence !! ind
    where colorSequence = cycle [Color 0 0 1,Color 1 0 0,Color 0 1 0,
                                 Color 1 1 0,Color 0 1 1,Color 1 0 1,
                                 Color 0 0 0]

styleSymbol :: Int -> PlotKind
styleSymbol ind = symbolSequence !! ind
    where symbolSequence = cycle [ Ex, HollowCircle, Square, Diamond,
                                   Triangle, DownTriangle, Plus, Star, FilledCircle ]

-- When defaultLayout1 has been generalized, change this signature to 
-- [InternalPlot x y] -> Layout1 x y z
iplot :: [InternalPlot Double Double] -> Layout1 Double Double
iplot foobar = defaultLayout1 {
        layout1_plots_ = concat $ zipWith toplot (ip foobar) [0..]
    }
    where ip (xs@(IPX _ _):xyss) = map (\ys -> (xs,ys)) yss ++ ip rest
              where yss = takeWhile isIPY xyss
                    rest = dropWhile isIPY xyss
          ip (_:xyss) = ip xyss
          ip [] = []
          isIPY (IPY _ _) = True
          isIPY _ = False
          toplot (IPX xs _, IPY ys yks) ind = map (\z -> (name yks, Left z)) plots
              where vs = zip xs ys
                    plots = case catMaybes $ map plotas yks of
                            [] -> [toPlot $ defaultPlotLines
                                   { plot_lines_values_ = [vs],
                                     plot_lines_style_ = solidLine 1 (styleColor ind) }]
                            xs -> xs
                    plotas Solid = Just $ toPlot $ defaultPlotLines
                                   { plot_lines_values_ = [vs],
                                     plot_lines_style_ = solidLine 1 (styleColor ind) }
                    plotas Dashed = Just $ toPlot $ defaultPlotLines
                                   { plot_lines_values_ = [vs],
                                     plot_lines_style_ = dashedLine 1 [10,10] (styleColor ind) }
                    plotas Dotted = Just $ toPlot $ defaultPlotLines
                                   { plot_lines_values_ = [vs],
                                     plot_lines_style_ = dashedLine 1 [1,11] (styleColor ind) }
                    plotas FilledCircle = Just $ toPlot $ defaultPlotPoints
                                          { plot_points_values_ = vs,
                                            plot_points_style_=filledCircles 4 (styleColor ind) }
                    plotas HollowCircle = Just $ toPlot $ defaultPlotPoints
                                          { plot_points_values_ = vs,
                                            plot_points_style_=hollowCircles 5 1 (styleColor ind) }
                    plotas Triangle = Just $ toPlot $ defaultPlotPoints
                                          { plot_points_values_ = vs,
                                            plot_points_style_=hollowPolygon 7 1 3 False (styleColor ind) }
                    plotas DownTriangle = Just $ toPlot $ defaultPlotPoints
                                          { plot_points_values_ = vs,
                                            plot_points_style_=hollowPolygon 7 1 3 True (styleColor ind) }
                    plotas Square = Just $ toPlot $ defaultPlotPoints
                                          { plot_points_values_ = vs,
                                            plot_points_style_=hollowPolygon 7 1 4 False (styleColor ind) }
                    plotas Diamond = Just $ toPlot $ defaultPlotPoints
                                          { plot_points_values_ = vs,
                                            plot_points_style_=hollowPolygon 7 1 4 True (styleColor ind) }
                    plotas Plus = Just $ toPlot $ defaultPlotPoints
                                          { plot_points_values_ = vs,
                                            plot_points_style_=plusses 7 1 (styleColor ind) }
                    plotas Ex = Just $ toPlot $ defaultPlotPoints
                                          { plot_points_values_ = vs,
                                            plot_points_style_=exes 7 1 (styleColor ind) }
                    plotas Star = Just $ toPlot $ defaultPlotPoints
                                          { plot_points_values_ = vs,
                                            plot_points_style_=stars 7 1 (styleColor ind) }
                    plotas Symbols = plotas (styleSymbol ind)
                    plotas _ = Nothing

name :: [PlotKind] -> String
name (Name s:_) = s
name (_:ks) = name ks
name [] = ""

str2k :: String -> [PlotKind]
str2k "" = []
str2k ". " = [Dotted]
str2k s@('?':_) = str2khelper s Symbols
str2k s@('@':_) = str2khelper s FilledCircle
str2k s@('#':_) = str2khelper s Square
str2k s@('v':_) = str2khelper s DownTriangle
str2k s@('^':_) = str2khelper s Triangle
str2k s@('o':_) = str2khelper s HollowCircle
str2k s@('+':_) = str2khelper s Plus
str2k s@('x':_) = str2khelper s Ex
str2k s@('*':_) = str2khelper s Star
str2k s@('.':_) = str2khelper s LittleDot
str2k "- " = [Dashed]
str2k "-" = [Solid]
str2k n = [Name n]

str2khelper :: String -> PlotKind -> [PlotKind]
str2khelper s@(_:r) x = case str2k r of
                        [] -> [x]
                        [Name _] -> [Name s]
                        xs -> x:xs

-- | Type to define a few simple properties of each plot.
data PlotKind = Name String | FilledCircle | HollowCircle
              | Triangle | DownTriangle | Square | Diamond | Plus | Ex | Star | Symbols
              | LittleDot | Dashed | Dotted | Solid
              deriving ( Eq, Show, Ord )
data InternalPlot x y = IPY [y] [PlotKind] | IPX [x] [PlotKind]

newtype Layout1DDD = Layout1DDD { plotLayout :: Layout1 Double Double }
instance ToRenderable Layout1DDD where
 toRenderable = toRenderable . plotLayout

uplot :: [UPlot] -> Layout1DDD
uplot us = Layout1DDD $ iplot $ nameDoubles $ evalfuncs us
    where nameDoubles :: [UPlot] -> [InternalPlot Double Double]
          nameDoubles (X xs:uus) = case grabName uus of
                                     (ks,uus') -> IPX (filter isValidNumber xs) ks : nameDoubles uus'
          nameDoubles (UDoubles xs:uus) = case grabName uus of
                                          (ks,uus') -> IPY (filter isValidNumber xs) ks : nameDoubles uus'
          nameDoubles (_:uus) = nameDoubles uus
          nameDoubles [] = []
          evalfuncs :: [UPlot] -> [UPlot]
          evalfuncs (UDoubles xs:uus) = X xs : map ef (takeWhile (not.isX) uus)
                                        ++ evalfuncs (dropWhile (not.isX) uus)
              where ef (UFunction f) = UDoubles (map f xs)
                    ef u = u
          evalfuncs (X xs:uus) = X xs : map ef (takeWhile (not.isX) uus)
                                 ++ evalfuncs (dropWhile (not.isX) uus)
              where ef (UFunction f) = UDoubles (map f xs)
                    ef u = u
          evalfuncs (u:uus) = u : evalfuncs uus
          evalfuncs [] = []
          grabName :: [UPlot] -> ([PlotKind],[UPlot])
          grabName (UString n:uus) = case grabName uus of
                                     (ks,uus') -> (str2k n++ks,uus')
          grabName (UKind ks:uus) = case grabName uus of
                                     (ks',uus') -> (ks++ks',uus')
          grabName uus = ([],uus)
          isX (X _) = True
          isX _ = False

-- | The main plotting function.  The idea behind PlotType is shamelessly
-- copied from Text.Printf (and is not exported).  All you need to know is
-- that your arguments need to be in class PlotArg.  And PlotArg consists
-- of functions and [Double] and String and PlotKind or [PlotKind].

plot :: PlotType a => a
plot = pl []
class PlotType t where
    pl :: [UPlot] -> t
instance (PlotArg a, PlotType r) => PlotType (a -> r) where
    pl args = \ a -> pl (toUPlot a ++ args)
instance PlotType Layout1DDD where
    pl args = uplot (reverse args)

-- | Display a plot on the screen.

plotWindow :: PlotWindowType a => a
plotWindow = plw []
class PlotWindowType t where
    plw :: [UPlot] -> t
instance (PlotArg a, PlotWindowType r) => PlotWindowType (a -> r) where
    plw args = \ a -> plw (toUPlot a ++ args)
instance PlotWindowType (IO a) where
    plw args = do renderableToWindow (toRenderable $ uplot (reverse args)) 640 480
                  return undefined

-- | Save a plot as a PDF file.

plotPDF :: PlotPDFType a => String -> a
plotPDF fn = pld fn []
class PlotPDFType t where
    pld :: FilePath -> [UPlot] -> t
instance (PlotArg a, PlotPDFType r) => PlotPDFType (a -> r) where
    pld fn args = \ a -> pld fn (toUPlot a ++ args)
instance PlotPDFType (IO a) where
    pld fn args = do renderableToPDFFile (toRenderable $ uplot (reverse args)) 640 480 fn
                     return undefined

-- | Save a plot as a postscript file.

plotPS :: PlotPSType a => String -> a
plotPS fn = pls fn []
class PlotPSType t where
    pls :: FilePath -> [UPlot] -> t
instance (PlotArg a, PlotPSType r) => PlotPSType (a -> r) where
    pls fn args = \ a -> pls fn (toUPlot a ++ args)
instance PlotPSType (IO a) where
    pls fn args = do renderableToPSFile (toRenderable $ uplot (reverse args)) 640 480 fn
                     return undefined

-- | Save a plot as a png file
plotPNG :: PlotPNGType a => String -> a
plotPNG fn = plp fn []

class PlotPNGType t where
    plp :: FilePath -> [UPlot] -> t
instance (PlotArg a, PlotPNGType r) => PlotPNGType (a -> r) where
    plp fn args = \ a -> plp fn (toUPlot a ++ args)
instance PlotPNGType (IO a) where
    plp fn args = do renderableToPNGFile (toRenderable $ uplot (reverse args)) 640 480 fn
                     return undefined



data UPlot = UString String | UDoubles [Double] | UFunction (Double -> Double)
           | UKind [PlotKind] | X [Double]

xcoords :: [Double] -> UPlot
xcoords = X

class PlotArg a where
    toUPlot :: a -> [UPlot]

instance IsPlot p => PlotArg [p] where
    toUPlot = toUPlot'

instance (Real a, Real b, Fractional a, Fractional b) => PlotArg (a -> b) where
    toUPlot x = [UFunction (realToFrac . x . realToFrac)]

instance (Real a, Real b, Fractional a, Fractional b) => IsPlot (a -> b) where
    toUPlot' = reverse . concatMap f
        where f x = [UFunction (realToFrac . x . realToFrac)]

instance PlotArg UPlot where
    toUPlot = (:[])

instance PlotArg PlotKind where
    toUPlot = (:[]) . UKind . (:[])

class IsPlot c where
    toUPlot' :: [c] -> [UPlot]

instance IsPlot PlotKind where
    toUPlot' = (:[]) . UKind

instance IsPlot Double where
    toUPlot' = (:[]) . UDoubles

instance IsPlot Char where
    toUPlot' = (:[]) . UString

instance IsPlot p => IsPlot [p] where
    toUPlot' = reverse . concatMap toUPlot'

instance (IsPlot p, IsPlot q, IsPlot r) => IsPlot (p,q,r) where
    toUPlot' = reverse . concatMap f
        where f (p,q,r) = toUPlot' [p] ++ toUPlot' [q] ++ toUPlot' [r]

instance (IsPlot p, IsPlot q) => IsPlot (p,q) where
    toUPlot' = reverse . concatMap f
        where f (p,q) = toUPlot' [p] ++ toUPlot' [q]
