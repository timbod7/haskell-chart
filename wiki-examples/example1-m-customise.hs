import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour.SRGB

signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]

peach :: AlphaColour Double
peach = opaque $ sRGB 1 0.95 0.9

main = toFile def "example1_custom.png" $ do

    -- Set the background colours.
    layout_background .= FillStyleSolid peach
    layout_plot_background .= Just (FillStyleSolid $ opaque white)

    -- Set the margin.
    layout_margin .= 25

    -- Set titles.
    layout_title .= "Amplitude Modulation"
    layout_x_axis . laxis_title .= "time (seconds)"
    layout_y_axis . laxis_title .= "amplitude"

    -- Extend the range of the y-axis. Note that the range we specify
    -- here is may be extended to the nearest tick-mark. For full
    -- control, over the range and scaling, supply an AxisFn.
    layout_y_axis . laxis_generate .= scaledAxis def (-1.2,1.2)

    -- Format the main title
    layout_title_style . font_size .= 18
    layout_title_style . font_weight .= FontWeightNormal
    -- see also font_name, font_slant, font_color

    -- Format other titles
    layout_x_axis . laxis_title_style . font_size .= 16
    layout_x_axis . laxis_style . axis_label_style . font_size .= 14
    layout_y_axis . laxis_title_style . font_size .= 16
    layout_y_axis . laxis_style . axis_label_style . font_size .= 14
    -- see also font_name, font_weight, font_slant, font_color

    -- Show the top and right axes
    layout_top_axis_visibility . axis_show_line .= True
    layout_top_axis_visibility . axis_show_ticks .= True
    layout_top_axis_visibility . axis_show_labels .= True
    layout_right_axis_visibility . axis_show_line .= True
    layout_right_axis_visibility . axis_show_ticks .= True
    layout_right_axis_visibility . axis_show_labels .= True
    -- also see layout_bottom_axis_visibility, layout_left_axis_visibility
    
    -- Turn off the legend.
    layout_legend .= Nothing

    plot (line "am" [signal [0,(0.5)..400]])
    plot (points "am points" (signal [0,7..400]))
