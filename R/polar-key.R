# =============================================================================
#  The key rose, standing on its own
# -----------------------------------------------------------------------------
#  polar_map() has always carried a key rose: one rose off to the side that
#  names the directions and the rings, so that the roses on the map itself can
#  go without compass letters and radial labels and stay readable at 240 m.
#  A *facetted* figure has exactly the same problem -- twenty-four panels, and
#  compass letters on every one of them would be twenty-four times the same
#  four letters -- but no key, because polar_map()'s one is drawn into map
#  coordinates and cannot leave them.
#
#  polar_key() is that key as a plot of its own, in the u/v space the roses
#  already live in, ready to be placed beside a facetted polar_raster() with
#  patchwork. It shares .polar_rings() and .polar_grid_layers() with
#  polar_raster() and .polar_axis_layers() with the radial axis, so the key
#  cannot describe rings the roses do not have -- the same reason polar_map()'s
#  key is built from the map's own `k` rather than from a second computation
#  of it.
# =============================================================================

#' A key rose for a facetted polar figure
#'
#' A single rose carrying no data: the rings of [polar_raster()], its spokes,
#' its outer circle, the compass directions and one label per ring. Placed
#' beside a facetted figure it says once what every panel would otherwise have
#' to repeat.
#'
#' @param x What the key describes: either the same data frame the roses are
#'   drawn from (any object [polar_raster()] accepts — the outer radius is
#'   taken from its `u`/`v`), or a single number, the outer radius in data
#'   units.
#' @param u,v Coordinate columns, used when `x` is a data frame.
#' @param grid Grid settings, as in [polar_raster()]. Pass the *same*
#'   [polar_grid()] the figure uses; `compass` is forced on here, because
#'   naming the directions is what the key is for.
#' @param compass Compass labelling of the key, overriding `grid$compass`.
#'   `TRUE` (default) = N/E/S/W, `"8"` = also NE/SE/SW/NW, `FALSE` = none.
#' @param axis_unit Unit appended to every ring label. `NULL` = bare numbers.
#' @param n_labels How many rings may carry a label (default 3, the
#'   outermost always among them). Every ring is still drawn. Same rule and
#'   same drawing as [polar_map()]'s key rose: a tick on the north spoke
#'   with the number beside it, so the two keys of one report are read the
#'   same way.
#' @param size,colour Text size and colour of the labels.
#' @param title Heading above the key, or `NULL` for none.
#' @param theme Plot theme; supplies `polar.grid` and `axis.text.y`, exactly
#'   as it does for the roses.
#'
#' @return A ggplot.
#' @seealso [polar_raster()], [polar_map()], whose `decor` draws the same key
#'   into map coordinates.
#' @examples
#' polar_key(6)
#' polar_key(6, compass = "8", title = "Wind")
#' @export
polar_key <- function(x,
                      u         = "u",
                      v         = "v",
                      grid      = polar_grid(),
                      compass   = TRUE,
                      axis_unit = "m/s",
                      n_labels  = 3,
                      size      = NULL,
                      colour    = NULL,
                      title     = NULL,
                      theme     = theme_polar()) {

  r_max <- if (is.numeric(x) && length(x) == 1L) {
    x
  } else {
    dat  <- .polar_data(x)
    miss <- setdiff(c(u, v), names(dat))
    if (length(miss)) {
      cli::cli_abort(c(
        "Column(s) not found: {.val {miss}}.",
        i = "Available: {.val {names(dat)}}."
      ))
    }
    max(sqrt(dat[[u]]^2 + dat[[v]]^2), na.rm = TRUE)
  }
  if (!is.finite(r_max) || r_max <= 0) {
    cli::cli_abort("The outer radius of the key must be a positive number.")
  }

  grid <- .polar_grid_style(grid, theme)
  grid$compass <- compass

  rings <- .polar_rings(r_max, grid)
  r_out <- max(r_max, rings, na.rm = TRUE)
  lim   <- r_out * (1 + grid$expand)

  p <- ggplot2::ggplot() +
    .polar_grid_layers(rings, r_out, grid) +
    # The map key\x27s own builder, so the two keys of a report cannot be read
    # differently; the text takes its size and colour from `axis.text.y`,
    # which is what styles the radial axis of a rose.
    .polar_key_ring_labels(
      rings, r_out, unit = axis_unit, n_max = n_labels,
      size = size %||% ((.polar_el(theme, "axis.text.y")$size %||% 8.8) /
                        ggplot2::.pt),
      colour = colour %||% (.polar_el(theme, "axis.text.y")$colour %||% "grey35"),
      fill = .polar_opaque((.polar_el(theme, "panel.background") %||% list())$fill) %||%
             .polar_opaque((.polar_el(theme, "plot.background") %||% list())$fill) %||%
             "white",
      alpha = 0.65) +
    ggplot2::coord_fixed(xlim = c(-lim, lim), ylim = c(-lim, lim),
                         expand = FALSE) +
    theme +
    # The key is all furniture and no data, so the plot's own axes have
    # nothing to say: theme_polar() draws the radial axis on the left, which
    # here would be a second, straight-line copy of the rings the key is
    # already showing.
    # Blanked child by child, not with the parents `axis.text` and
    # `axis.title`: theme_polar() sets the .y children explicitly, and an
    # explicit child is not overridden by a blank parent added afterwards.
    ggplot2::theme(axis.line.x = ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank())

  # theme_void() draws no axes, but it does draw a title, and the key is the
  # one place in the figure where a heading has room.
  if (!is.null(title)) p <- p + ggplot2::labs(title = title)
  p
}
