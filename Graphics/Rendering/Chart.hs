-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- A framework for creating 2D charts in Haskell.
--
-- The basic model is that you define a value of type 'Renderable',
-- typically by applying 'toRenderable' to some other value. This
-- 'Renderable' is then actually displayed or output by calling either
-- 'renderableToPNGFile', or 'Graphics.Rendering.Chart.Gtk.renderableToWindow'.
--
-- Currently, the only useful 'Renderable' for displaying charts
-- is created by applying 'toRenderable' to a value of type
-- 'Graphics.Rendering.Chart.Layout.Layout1'
--
-- For a simpler though less flexible API, see "Graphics.Rendering.Chart.Simple".
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Chart(
    Renderable(..),
    ToRenderable(..),
    Layout1(..),
    MAxisFn,
    Axis(..),
    LinearAxisParams(..),
    Plot(..),
    ToPlot(..),
    PlotPoints(..),
    PlotErrBars(..),
    PlotLines(..),
    PlotFillBetween(..),
    LegendStyle(..),
    Rect(..),
    Point(..),
    Color(..),
    LogValue(..),
    ErrPoint(..),
    symErrPoint,
    PieChart(..),
    PieLayout(..),
    PieItem(..),
    defaultAxisLineStyle, 
    defaultPlotLineStyle,
    defaultPlotPoints,
    defaultPlotErrBars,
    defaultPlotLines,
    defaultPlotFillBetween,
    defaultLayout1,
    defaultLinearAxis,
    defaultPieLayout,
    defaultPieChart,
    defaultPieItem,
    defaultLegendStyle,
    filledCircles,
    hollowCircles,
    exes, plusses, stars,
    filledPolygon,
    hollowPolygon,
    solidLine,
    dashedLine,
    solidFillStyle,
    AxisStyle(..),
    defaultAxisStyle,
    autoScaledAxis,
    autoScaledAxis',
    autoScaledLogAxis,
    autoScaledLogAxis',
    timeAxis,
    autoTimeAxis,
    days, months, years,
    renderableToPNGFile,
    renderableToPDFFile,
    renderableToPSFile,
    renderableToSVGFile,
    CairoLineStyle(..),
    CairoFillStyle(..),
    CairoFontStyle(..),
    PlotValue(..),
    mAxis,
    noAxis,

    line_width,
    line_color,
    line_dashes,
    line_cap,
    line_join,

    font_name,
    font_size,
    font_slant,
    font_weight,
    font_color,

    layout1_background,
    layout1_title,
    layout1_title_style,
    layout1_left_axis,
    layout1_right_axis,
    layout1_top_axis,
    layout1_bottom_axis,
    layout1_link_vertical_axes,
    layout1_margin,
    layout1_plots,
    layout1_legend,
    layout1_grid_last,

    laxis_title_style,
    laxis_title,
    laxis_style,
    laxis_data,

    rect_minsize,
    rect_fillStyle,
    rect_lineStyle,
    rect_cornerStyle,

    axisGridNone,
    axisGridAtTicks,
    axisGridAtLabels,

    axis_viewport,
    axis_ticks,
    axis_labels,
    axis_grid,

    axis_line_style,
    axis_label_style,
    axis_grid_style,
    axis_label_gap,

    axis_style,
    axis_data,

    la_labelf,
    la_nLabels,
    la_nTicks,

    plot_render,
    plot_render_legend,
    plot_all_points,

    plot_lines_style,
    plot_lines_values,

    plot_points_style,
    plot_points_values,

    plot_fillbetween_style,
    plot_fillbetween_values,

    plot_errbars_line_style,
    plot_errbars_tick_length,
    plot_errbars_overhang,
    plot_errbars_values,

    legend_label_style,
    legend_margin,
    legend_plot_size,

    pie_title,
    pie_title_style,
    pie_plot,
    pie_background,
    pie_margin,
    pie_data,
    pie_colors,
    pie_label_style,
    pie_label_line_style,
    pie_start_angle,
    pitem_label,
    pitem_offset,
    pitem_value,

) where

import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Layout
import Graphics.Rendering.Chart.Axis
import Graphics.Rendering.Chart.Plot
import Graphics.Rendering.Chart.Legend
import Graphics.Rendering.Chart.Pie
