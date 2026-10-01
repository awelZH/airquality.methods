# =============================================================================
#  polar_raster() — polar display of a u/v grid
# -----------------------------------------------------------------------------
#  Draws the data output of openair::polarPlot() (or any other regular grid
#  in Cartesian wind-vector coordinates) as a ggplot:
#
#   * the surface as geom_raster() over u/v — a bitmap instead of tens of
#     thousands of vector objects (roughly a factor 25 smaller in vector
#     output),
#   * the polar grid (rings, spokes, outer circle, compass directions) as its
#     own, configurable layers,
#   * the radial axis on the far left as a *real* y-axis — so it's
#     formattable via `theme(axis.text.y = ...)`, `axis.ticks.y`,
#     `axis.line.y`, and trimmed to 0..r_max with `guide_axis(cap = "both")`,
#   * facets automatically from polarPlot()'s grouping columns,
#   * the colour scale via an argument — default scale_fill_viridis_c().
#
#  Nothing is smoothed here: the statistics stay with openair, this only
#  draws.
#
#  Entry points:
#    polar_raster(pp, ...)                     draws an existing result
#    polar_plot(data, ..., args = list(...))   computes and draws in one go
#    polar_bin(data, "nox", res = 0.5)         square cells, no smoothing,
#                                              straight from the raw data
#    polar_sector(data, "nox", wd_res = 10)    the same in ring segments
#
#  A capped colour scale isn't built in, but composes from outside:
#    polar_raster(pp, scale = scale_fill_capped(limits = c(50, 250)))
#    polar_raster(pp) + scale_fill_capped(limits = c(50, 250))
# =============================================================================

#' Polar display of a u/v grid
#'
#' @param x A result from [openair::polarPlot()] (then `x$data` is used) or
#'   directly a data frame with the columns `u`, `v`, `z`.
#' @param u,v,z Column names of the Cartesian coordinates and the value.
#' @param facet Faceting. `NULL` (default) = automatic from the grouping
#'   columns, `NA` = none, a formula (e.g. `~season`), or a vector of column
#'   names. All facets share one colour scale.
#' @param grid Polar grid settings, see [polar_grid()].
#' @param scale Colour scale. `NULL` (default) builds
#'   [ggplot2::scale_fill_viridis_c()] from `...`; alternatively a ready-made
#'   scale, in which case `...` is ignored. A capped scale plugs in like this:
#'   `scale = scale_fill_capped(limits = c(50, 250))`.
#' @param interpolate Passed on to [ggplot2::geom_raster()]: `TRUE`
#'   interpolates bilinearly between cell centres (a smooth surface, as in
#'   openair), `FALSE` draws sharp cells. Default `NULL` = `FALSE` unless
#'   `TRUE` is requested explicitly — sharp cells are the honest default,
#'   including for a smoothed [openair::polarPlot()] surface; aggregated
#'   cells from [polar_bin()] or [polar_sector()] also record `FALSE`, since
#'   interpolation there would be invented detail.
#' @param axis_labels Where the radial axis goes: `TRUE`/`"left"` (default)
#'   the plot's real y-axis on the far left, `"centre"` an axis drawn inside
#'   the panel from the centre outwards, `FALSE`/`"none"` no axis at all.
#'   See Details for what "centre" costs.
#' @param axis_bearing Direction along which the centred labels sit, in
#'   compass degrees (0 = north, clockwise).
#' @param axis_title Radial axis title. Default `NULL` = hidden (the unit
#'   already sits on every break via `axis_unit`).
#' @param axis_unit Unit appended to every radial axis break (e.g. `"2 m/s"`).
#'   Default `"m/s"`; `NULL` leaves the bare numbers.
#' @param theme Plot theme. Default [theme_polar()].
#' @param ... Arguments for the default scale [ggplot2::scale_fill_viridis_c()]
#'   — e.g. `limits`, `name`, `option`, `direction`, `trans`. The default is
#'   `option = "A"` (magma), not viridis.
#'
#' @details
#' The radial axis is the plot's ordinary y-axis and therefore formattable
#' via `theme(axis.text.y = , axis.ticks.y = , axis.line.y = )`. The axis
#' line is trimmed to the range 0..largest ring with `guide_axis(cap = "both")`.
#'
#' `axis_labels = "centre"` moves it into the panel. ggplot2 draws axes
#' outside the panel and nowhere else, so this one is no longer an axis in
#' ggplot's sense but a single layer: one label per ring, sitting on its ring
#' along `axis_bearing`. No axis line and no ticks, because the rings already
#' are the ticks. The same `rings` as the grid, the same break labels as
#' [polar_map()]'s key rose, and `axis.text.y` for the styling. What is lost
#' is the guide machinery: `guide_axis()` settings, `axis_title`, and the
#' axis's claim on plot margin space.
#'
#' The surface is drawn as [ggplot2::geom_raster()], which requires an evenly
#' spaced grid — true for `polarPlot()` even when cells are missing and
#' facets have differently sized grids. If the grid is uneven (e.g. after
#' filtering), this falls back to [ggplot2::geom_tile()] with a note; without
#' the fallback ggplot2 would silently shift the pixels.
#'
#' @return A ggplot object.
#'
#' @examples
#' library(ggplot2)
#'
#' # A minimal u/v/z grid works without openair::polarPlot() at all.
#' grid <- expand.grid(u = seq(-10, 10, 1), v = seq(-10, 10, 1))
#' grid$z <- with(grid, 100 - sqrt(u^2 + v^2) * 8 + u)
#' polar_raster(grid)
#'
#' \dontrun{
#' library(openair)
#' pp <- polarPlot(mydata, pollutant = "nox", plot = FALSE)
#' polar_raster(pp, name = "NOx")
#' polar_raster(pp, limits = c(50, 250), option = "A")
#'
#' # capped scale with "<="/">=" and whiskers
#' polar_raster(pp, scale = scale_fill_capped(limits = c(50, 250), name = "NOx"))
#'
#' pp2 <- polarPlot(mydata, pollutant = "nox", type = "season", plot = FALSE)
#' polar_raster(pp2, limits = c(50, 250))
#' }
#' @export
polar_raster <- function(x,
                         u           = "u",
                         v           = "v",
                         z           = "z",
                         facet       = NULL,
                         grid        = polar_grid(),
                         scale       = NULL,
                         interpolate = NULL,
                         axis_labels = TRUE,
                         axis_bearing = 0,
                         axis_title  = NULL,
                         axis_unit   = "m/s",
                         theme       = theme_polar(),
                         ...) {

  axis_pos <- .polar_axis_position(axis_labels)
  # Rings and spokes take their look from the theme unless polar_grid() was
  # given an explicit one -- see polar.grid in R/zzz.R.
  grid <- .polar_grid_style(grid, theme)

  dat  <- .polar_data(x)
  miss <- setdiff(c(u, v, z), names(dat))
  if (length(miss)) {
    cli::cli_abort(c(
      "Column(s) not found: {.val {miss}}.",
      i = "Available: {.val {names(dat)}}."
    ))
  }

  # --- Radius, rings, drawing area --------------------------------------
  rad   <- sqrt(dat[[u]]^2 + dat[[v]]^2)
  r_max <- max(rad, na.rm = TRUE)
  rings <- .polar_rings(r_max, grid)
  r_out <- max(r_max, rings, na.rm = TRUE)
  lim   <- r_out * (1 + grid$expand)

  # --- Grid layers (without facet columns -> drawn in every panel) ------
  # Shared with polar_key(), so the key rose is the same object as these.
  gl <- .polar_grid_layers(rings, r_out, grid)

  # --- Surface ------------------------------------------------------------
  fc <- .polar_facets(dat, facet, c(u, v, z))

  # Sharp cells by default; polar_bin()/polar_sector() reaffirm that via
  # the polar_interpolate attribute, but even a smoothed polarPlot() surface
  # only blurs when interpolate = TRUE is requested explicitly.
  interpolate <- interpolate %||% attr(dat, "polar_interpolate") %||% FALSE

  if (identical(attr(dat, "polar_geom"), "polygon")) {
    # Sectors: ready-made vertices, no grid
    if (is.null(dat$.cell)) {
      cli::cli_abort(c(
        "Polygon data without a {.field .cell} column.",
        i = "There's no way to tell which points belong to which cell."
      ))
    }
    # Adjacent polygons leave fine light seams when rendered.
    # after_scale(fill) traces the same colour value as a hairline contour —
    # without a second scale and without an extra legend.
    surface <- ggplot2::geom_polygon(
      ggplot2::aes(fill = .data[[z]], group = .data$.cell,
                   colour = ggplot2::after_scale(.data$fill)),
      linewidth = 0.2)
  } else {
    grp  <- if (length(fc)) interaction(dat[fc], drop = TRUE) else factor(rep(1L, nrow(dat)))
    kind <- .polar_kind(dat, u, v, grp)
    if (kind == "irregular") {
      cli::cli_inform(c(
        "Grid is irregularly spaced \u2014 using {.fn geom_tile} instead of {.fn geom_raster}.",
        i = "Cells may overlap or leave gaps as a result."
      ))
    }
    surface <- if (kind == "regular") {
      ggplot2::geom_raster(ggplot2::aes(fill = .data[[z]]), interpolate = interpolate)
    } else {
      ggplot2::geom_tile(ggplot2::aes(fill = .data[[z]]))
    }
  }

  # --- Assemble -------------------------------------------------------------
  p <- ggplot2::ggplot(dat, ggplot2::aes(.data[[u]], .data[[v]]))
  p <- p + if (isTRUE(grid$below)) c(gl, list(surface)) else c(list(surface), gl)

  p <- p +
    (scale %||% .polar_scale(...)) +
    ggplot2::scale_y_continuous(
      breaks = c(0, rings),
      labels = function(b) .polar_axis_labels(b, axis_unit),
      # The edge axis is suppressed through the guide, not through
      # `theme(axis.text.y = element_blank())`: the centred axis is styled
      # from those very elements, so blanking them would mean a user who
      # restyles the centred axis switches the edge axis back on.
      guide  = if (axis_pos == "left") ggplot2::guide_axis(cap = "both") else "none") +
    ggplot2::coord_fixed(xlim = c(-lim, lim), ylim = c(-lim, lim), expand = FALSE) +
    ggplot2::labs(x = NULL, y = if (axis_pos == "left") axis_title else NULL) +
    theme

  if (length(fc)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", paste(fc, collapse = " + "))))
  }
  if (axis_pos == "centre") {
    # Same `rings` and the same break labels as the edge axis would have had;
    # only the rendering differs. The styling is read from `theme` here and
    # now, so a `+ theme(...)` added afterwards cannot reach these layers --
    # pass the theme to `theme =` instead.
    p <- p + .polar_axis_layers(rings, r_out, axis_bearing, axis_unit, theme)
  }
  p
}

#' Default colour scale
#'
#' Magma (`option = "A"`) rather than viridis's default green-yellow: the
#' surfaces here sit on a topographic basemap whose forest green and meadow
#' tones are exactly viridis's middle, and a dark-to-light ramp survives the
#' greyscale print that a report gets photocopied into. Passing `option`
#' explicitly still wins.
#' @keywords internal
.polar_scale <- function(..., option = "A") {
  ggplot2::scale_fill_viridis_c(..., option = option)
}
