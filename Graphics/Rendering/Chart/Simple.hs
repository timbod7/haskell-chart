-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Simple
-- Copyright   :  (c) David Roundy 2007
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- An even simpler framework for creating 2D charts in Haskell.
--
--
-- The basic idea is to make it as easy to plot as octave, which means that
-- you provide no more information than you wish to provide.  There is one
-- function 'plot', which does all the work.  It accepts a variable number
-- of arguments.  You must provide a [Double] which defines the points on
-- the x axis, which must precede any of the "y" values.  The y values may
-- either be [Double] or functions.  After any given y value, you can give
-- either Strings or PlotKinds describing how you'd like that y printed.
--
-- Examples:
--
-- renderableToWindow (toRenderable $ plot [0,0.1..10] sin "sin(x)") 640 480
--
-- renderableToWindow (toRenderable $ plot [0,1,3,4,8]] [12,15,1,5,8] "o" "points") 640 480
--
-- renderableToWindow (toRenderable $ plot [0,0.1..10] sin "- " cos ". " cos "o") 640 480
-----------------------------------------------------------------------------
module Graphics.Rendering.Chart.Simple( plot, PlotKind(..) ) where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk

styleColor :: (Double -> Double -> Double -> a) -> Int -> a
styleColor f ind = case colorSequence !! ind of (r,g,b) -> f r g b
    where colorSequence = cycle [(0,0,1),(1,0,0),(0,1,0),(1,1,0),(0,1,1),(1,0,1),(0,0,0)]

iplot :: [InternalPlot] -> Layout1
iplot (IP xs kxs:yss) = defaultLayout1 {
        layout1_plots = zipWith toplot yss [1..]
    }
    where toplot (IP ys yks) ind = (name yks, HA_Bottom, VA_Left, p)
              where vs = zipWith (\x y -> Point x y) xs ys
                    p | Solid `elem` yks = toPlot $ defaultPlotLines {
                          plot_lines_values = [vs],
                          plot_lines_style = solidLine 1 `styleColor` ind
                        }
                      | Dashed `elem` yks = toPlot $ defaultPlotLines {
                          plot_lines_values = [vs],
                          plot_lines_style = dashedLine 1 [10, 10] `styleColor` ind
                        }
                      | Dotted `elem` yks = toPlot $ defaultPlotLines {
                          plot_lines_values = [vs],
                          plot_lines_style = dashedLine 1 [1, 11] `styleColor` ind
                        }
                      | FilledCircle `elem` yks = toPlot $ defaultPlotPoints {
                          plot_points_values = vs,
                          plot_points_style=filledCircles 7 `styleColor` ind
                        }
                      | FilledCircle `elem` yks = toPlot $ defaultPlotPoints {
                          plot_points_values = vs,
                          plot_points_style=filledCircles 2 `styleColor` ind
                        }
                      | otherwise = toPlot $ defaultPlotLines {
                          plot_lines_values = [vs],
                          plot_lines_style = solidLine 1 `styleColor` ind
                        }

name :: [PlotKind] -> String
name (Name s:_) = s
name (_:ks) = name ks
name [] = ""

str2k :: String -> PlotKind
str2k "o" = FilledCircle
str2k "." = LittleDot
str2k "- " = Dashed
str2k ". " = Dotted
str2k n = Name n

-- | Type to define a few simple properties of each plot.
data PlotKind = Name String | FilledCircle | LittleDot | Dashed | Dotted | Solid
              deriving ( Eq, Show, Ord )
data InternalPlot = IP [Double] [PlotKind]

uplot :: [UPlot] -> Layout1
uplot us = iplot $ nameDoubles $ evalfuncs us
    where nameDoubles :: [UPlot] -> [InternalPlot]
          nameDoubles (UDoubles xs:uus) = case grabName uus of
                                          (ks,uus') -> IP xs ks : nameDoubles uus'
          nameDoubles (_:uus) = nameDoubles uus
          nameDoubles [] = []
          evalfuncs :: [UPlot] -> [UPlot]
          evalfuncs (UDoubles xs:uus) = UDoubles xs : map ef uus
              where ef (UFunction f) = UDoubles (map f xs)
                    ef u = u
          evalfuncs (u:uus) = u : evalfuncs uus
          evalfuncs [] = []
          grabName :: [UPlot] -> ([PlotKind],[UPlot])
          grabName (UString n:uus) = case grabName uus of
                                     (ks,uus') -> (str2k n:ks,uus')
          grabName (UKind ks:uus) = case grabName uus of
                                     (ks',uus') -> (ks++ks',uus')
          grabName uus = ([],uus)

-- | The main function.  The idea behind PlotType is shamelessly copied
-- from Text.Printf (and is not exported).  All you need to know is that
-- your arguments need to be in class PlotArg.  And PlotArg consists of
-- functions and [Double] and String and PlotKind or [PlotKind].

plot :: PlotType a => a
plot = pl []

class PlotType t where
    pl :: [UPlot] -> t

instance (PlotArg a, PlotType r) => PlotType (a -> r) where
    pl args = \ a -> pl (toUPlot a : args)

instance PlotType Layout1 where
    pl args = uplot (reverse args)

data UPlot = UString String | UDoubles [Double] | UFunction (Double -> Double)
           | UKind [PlotKind]

class PlotArg a where
    toUPlot :: a -> UPlot

instance IsPlot p => PlotArg [p] where
    toUPlot = toUPlot'

instance (Real a, Real b, Fractional a, Fractional b) => PlotArg (a -> b) where
    toUPlot f = UFunction (realToFrac . f . realToFrac)

instance PlotArg PlotKind where
    toUPlot = UKind . (:[])

class IsPlot c where
    toUPlot' :: [c] -> UPlot

instance IsPlot PlotKind where
    toUPlot' = UKind

instance IsPlot Double where
    toUPlot' = UDoubles

instance IsPlot Char where
    toUPlot' = UString
