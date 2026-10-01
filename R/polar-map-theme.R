# =============================================================================
#  Theme for the map display
# =============================================================================

#' Theme for [polar_map()]
#'
#' Empty background, no axes by default. On a map the axes would carry LV95
#' coordinates, which are noise for most readers — the scale bar carries the
#' distance information instead. Turn them on with `axes = TRUE` when the
#' figure is meant to be read against other coordinate material.
#'
#' @param base_size,base_family As in the ggplot2 themes.
#' @param axes Show the LV95 coordinate axes.
#' @param axis_colour Colour of axis line, ticks, and text.
#' @param grid_colour,grid_linewidth,grid_linetype The `polar.grid` element —
#'   rings and spokes, see [polar_grid()]. Darker and slightly heavier than in
#'   [theme_polar()]: over a basemap the plain-background grey disappears.
#' @return A ggplot2 theme.
#' @examples
#' library(ggplot2)
#' ggplot(data.frame(x = 1, y = 1), aes(x, y)) + geom_point() + theme_polar_map()
#' @export
theme_polar_map <- function(base_size      = 11,
                            base_family    = "",
                            axes           = FALSE,
                            axis_colour    = "grey35",
                            grid_colour    = "grey20",
                            grid_linewidth = 0.3,
                            grid_linetype  = 2) {

  th <- ggplot2::theme_void(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      polar.grid   = ggplot2::element_line(
        colour = grid_colour, linewidth = grid_linewidth,
        linetype = grid_linetype),
      legend.ticks = ggplot2::element_blank(),
      strip.text   = ggplot2::element_text(
        size = ggplot2::rel(0.9),
        margin = ggplot2::margin(b = 0.4 * base_size)),
      # The swisstopo attribution lands here and must stay readable.
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.75), hjust = 1, colour = axis_colour,
        margin = ggplot2::margin(t = 0.3 * base_size))
    )

  if (!isTRUE(axes)) return(th)

  # theme_void() blanks the titles, so like theme_polar() they have to be
  # restored explicitly, angle included.
  th + ggplot2::theme(
    axis.line    = ggplot2::element_line(colour = axis_colour, linewidth = 0.3),
    axis.ticks   = ggplot2::element_line(colour = axis_colour, linewidth = 0.3),
    axis.ticks.length = grid::unit(2.5, "pt"),
    axis.text.x  = ggplot2::element_text(
      size = ggplot2::rel(0.75), colour = axis_colour,
      margin = ggplot2::margin(t = 0.2 * base_size)),
    axis.text.y  = ggplot2::element_text(
      size = ggplot2::rel(0.75), colour = axis_colour, hjust = 1,
      margin = ggplot2::margin(r = 0.2 * base_size)),
    axis.title.x = ggplot2::element_text(
      size = ggplot2::rel(0.85), colour = axis_colour,
      margin = ggplot2::margin(t = 0.3 * base_size)),
    axis.title.y = ggplot2::element_text(
      size = ggplot2::rel(0.85), colour = axis_colour, angle = 90,
      margin = ggplot2::margin(r = 0.3 * base_size))
  )
}
