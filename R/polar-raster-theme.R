# =============================================================================
#  Theme
# =============================================================================

#' Theme for the polar display
#'
#' Empty background, no x-axis, visible y-axis (= radial axis) on the left.
#'
#' @param base_size,base_family As in the ggplot2 themes.
#' @param axis_colour Colour of the axis line, ticks, and label.
#' @param grid_colour,grid_linewidth,grid_linetype The `polar.grid` element,
#'   i.e. the rings and spokes drawn by [polar_grid()]. They live in the theme
#'   rather than in the layers so that `theme(polar.grid = element_line(...))`
#'   restyles them like any other grid.
#' @return A ggplot2 theme.
#' @export
theme_polar <- function(base_size      = 11,
                        base_family    = "",
                        axis_colour    = "grey35",
                        grid_colour    = axis_colour,
                        grid_linewidth = 0.25,
                        grid_linetype  = 2) {
  ggplot2::theme_void(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Not panel.grid: on a Cartesian panel that would draw straight lines
      # through the rose. polar.grid is this package's own element (R/zzz.R),
      # read by polar_grid() and rendered as layers.
      polar.grid = ggplot2::element_line(
        colour = grid_colour, linewidth = grid_linewidth,
        linetype = grid_linetype),
      axis.line.y        = ggplot2::element_line(colour = axis_colour, linewidth = 0.3),
      axis.ticks.y       = ggplot2::element_line(colour = axis_colour, linewidth = 0.3),
      axis.ticks.length.y = grid::unit(2.5, "pt"),
      # The right margin keeps the axis label clear of the compass label
      # (W), which sits at the same height.
      axis.text.y        = ggplot2::element_text(
        size = ggplot2::rel(0.8), colour = axis_colour,
        margin = ggplot2::margin(r = 0.4 * base_size), hjust = 1),
      # theme_void() sets axis.title to element_blank(); the radial axis
      # title must therefore be set explicitly, angle included.
      axis.title.y       = ggplot2::element_text(
        size = ggplot2::rel(0.9), colour = axis_colour, angle = 90,
        margin = ggplot2::margin(r = 0.3 * base_size)),
      legend.ticks       = ggplot2::element_blank(),
      strip.text         = ggplot2::element_text(
        size = ggplot2::rel(0.9), margin = ggplot2::margin(b = 0.4 * base_size)),
      plot.caption       = ggplot2::element_text(
        size = ggplot2::rel(0.75), hjust = 0.5, colour = axis_colour)
    )
}
