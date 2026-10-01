# =============================================================================
#  The polar grid
# -----------------------------------------------------------------------------
#  The rings and spokes are layers, not coord furniture. That is forced by the
#  design: the surface is a `geom_raster()` on a Cartesian u/v panel, so the
#  only coord that could draw a *round* grid -- `coord_radial()` -- is out
#  (a regular u/v grid is irregular in theta/r, which costs the raster and with
#  it polar_map(), where the panel has to be LV95 metres).
#
#  Being layers does not have to mean being unthemable, though. `colour`,
#  `linewidth`, and `linetype` default to NULL = "ask the theme", and the
#  theme element they ask is `polar.grid`, registered in R/zzz.R and set by
#  theme_polar() and theme_polar_map(). So the grid is styled the ggplot way,
#  theme(polar.grid = element_line(...)), while an explicit value here still
#  wins for a one-off plot.
# =============================================================================

#' Configure the look of the polar grid
#'
#' Bundles everything that isn't the surface itself, so [polar_raster()]'s
#' signature stays lean.
#'
#' @param rings Radii of the rings (in units of the radial axis, i.e. wind
#'   speed for `polarPlot()`). `NULL` = automatic via `pretty(c(0, r_max), n)`.
#' @param n Target number of rings for automatic selection.
#' @param spokes Angles of the spokes in degrees (0 = north, clockwise).
#'   `NULL` or `numeric(0)` = no spokes.
#' @param compass Labelling of compass directions. `TRUE` = N/E/S/W, `"8"` =
#'   also NE/SE/SW/NW, `FALSE` = none, or a named numeric vector
#'   `c(N = 0, E = 90, ...)` for custom marks.
#' @param outer Draw the outer circle.
#' @param below Draw the grid below the surface instead of above it.
#' @param colour,linewidth,linetype Appearance of rings and spokes. `NULL`
#'   (default) takes them from the `polar.grid` theme element — see Details.
#' @param outer_linetype Line type of the outer circle (usually solid).
#' @param casing Colour of a wider line drawn *underneath* every ring and
#'   spoke, or `NULL` (default) for none. On a plain panel a grid needs no
#'   casing; over a topographic basemap it is the difference between a grid
#'   one can follow and one that disappears wherever the map is dark. This
#'   is the same trick the site labels use with their filled box, and the
#'   same one cartographers use for a road over a hillshade.
#' @param casing_width,casing_alpha Width of the casing as a multiple of
#'   `linewidth`, and its opacity. The defaults sit the casing just outside
#'   the line and let the map show through, so the grid reads as laid over
#'   the map rather than as cut out of it.
#' @param compass_size,compass_fontface Appearance of the compass labels.
#' @param expand Fraction of the radius kept as margin for the compass
#'   labels.
#'
#' @details
#' The grid is drawn as ordinary layers, because the surface is a
#' [ggplot2::geom_raster()] on a Cartesian panel and `panel.grid` would put
#' *straight* lines across it. It is still styled like a theme element: the
#' three appearance arguments default to `NULL` and are then read from
#' `polar.grid`, which this package registers with ggplot2 and which
#' [theme_polar()] and [theme_polar_map()] set. So both of these work, and the
#' second one keeps working across every plot:
#'
#' ```
#' polar_raster(pp, grid = polar_grid(colour = "grey20"))
#' polar_raster(pp) + theme(polar.grid = element_line(colour = "grey20"))
#' ```
#'
#' Field by field, an explicit argument wins over the theme, and the theme
#' wins over the built-in fallback (`grey35`, `0.25`, dashed).
#'
#' @return A list with the settings.
#' @export
polar_grid <- function(rings            = NULL,
                       n                = 4,
                       spokes           = seq(0, 315, by = 45),
                       compass          = TRUE,
                       outer            = TRUE,
                       below            = FALSE,
                       colour           = NULL,
                       linewidth        = NULL,
                       linetype         = NULL,
                       outer_linetype   = 1,
                       casing           = NULL,
                       casing_width     = 3,
                       casing_alpha     = 0.55,
                       compass_size     = 3.2,
                       compass_fontface = "plain",
                       expand           = 0.17) {
  list(rings = rings, n = n, spokes = spokes, compass = compass, outer = outer,
       below = below, colour = colour, linewidth = linewidth, linetype = linetype,
       casing = casing, casing_width = casing_width, casing_alpha = casing_alpha,
       outer_linetype = outer_linetype, compass_size = compass_size,
       compass_fontface = compass_fontface, expand = expand)
}

#' Complete a possibly partial theme
#'
#' [ggplot2::calc_element()] needs a complete theme. A user may pass a bare
#' `theme(...)` to `polar_raster(theme = )`, which ggplot2 itself would add on
#' top of the session default -- so that is what we resolve against too,
#' rather than guessing.
#' @keywords internal
.polar_theme <- function(theme) {
  if (is.null(theme)) return(ggplot2::theme_get())
  if (isTRUE(attr(theme, "complete"))) theme else ggplot2::theme_get() + theme
}

#' Fill the grid's appearance in from the theme
#'
#' Explicit argument > `polar.grid` theme element > built-in fallback, one
#' field at a time. The fallback matters because `polar.grid` inherits from
#' `line`, and in a `theme_void()`-based theme that parent is blank.
#' @keywords internal
.polar_grid_style <- function(grid, theme) {

  el <- tryCatch(ggplot2::calc_element("polar.grid", .polar_theme(theme)),
                 error = function(e) NULL)
  if (inherits(el, "element_blank")) el <- NULL

  grid$colour    <- grid$colour    %||% el$colour    %||% "grey35"
  grid$linewidth <- grid$linewidth %||% el$linewidth %||% 0.25
  grid$linetype  <- grid$linetype  %||% el$linetype  %||% 2
  grid
}

#' Resolve compass directions into a named angle vector
#' @keywords internal
.polar_compass <- function(compass) {
  if (is.numeric(compass)) return(compass)
  if (isFALSE(compass) || is.null(compass)) return(numeric(0))
  if (identical(compass, "8")) {
    return(stats::setNames(seq(0, 315, by = 45),
                           c("N", "NE", "E", "SE", "S", "SW", "W", "NW")))
  }
  stats::setNames(c(0, 90, 180, 270), c("N", "E", "S", "W"))
}

#' Circle as a path
#' @keywords internal
.polar_circle <- function(r, n = 181L) {
  a <- seq(0, 2 * pi, length.out = n)
  data.frame(.r = r, .u = r * sin(a), .v = r * cos(a))
}

#' Ring radii for a given outer radius
#'
#' Split out of [polar_raster()] on 2026-08-27 so that [polar_key()] cannot
#' come to a different answer: a key rose whose rings are not the data roses'
#' rings is worse than no key at all. Explicit `rings` win; otherwise
#' `pretty()` picks them and anything at or below zero, or beyond the data, is
#' dropped.
#' @keywords internal
.polar_rings <- function(r_max, grid) {
  grid$rings %||% {
    b <- pretty(c(0, r_max), grid$n)
    b[b > 0 & b <= r_max]
  }
}

#' Is a casing wanted, and what does it look like?
#'
#' One place, so the panel grid and the map grid cannot case their lines
#' differently. Returns `NULL` when no casing was asked for, which every
#' caller can pass straight to `c()`.
#' @keywords internal
.polar_casing <- function(grid) {
  cas <- grid$casing
  if (is.null(cas) || isTRUE(is.na(cas))) return(NULL)
  list(colour = cas,
       linewidth = grid$linewidth * (grid$casing_width %||% 3),
       alpha = grid$casing_alpha %||% 0.55)
}


#' The grid itself, as layers
#'
#' Rings, spokes, outer circle and compass letters, in the u/v space of a rose.
#' [polar_raster()] draws them into every panel, [polar_key()] into a panel of
#' its own; one function, so the key is the same object as the roses it
#' explains and not a drawing of one.
#'
#' `grid` must already have been through [.polar_grid_style()].
#' @keywords internal
.polar_grid_layers <- function(rings, r_out, grid) {

  gl  <- list()
  cas <- .polar_casing(grid)

  if (length(rings)) {
    ring_df <- dplyr::bind_rows(lapply(rings, .polar_circle))
    ring_aes <- ggplot2::aes(.data$.u, .data$.v, group = .data$.r)
    # The casing first, so the line itself is drawn on top of it.
    if (!is.null(cas)) gl <- c(gl, list(ggplot2::geom_path(
      data = ring_df, ring_aes, inherit.aes = FALSE,
      colour = cas$colour, linewidth = cas$linewidth, alpha = cas$alpha)))
    gl <- c(gl, list(ggplot2::geom_path(
      data = ring_df, ring_aes,
      inherit.aes = FALSE,
      colour = grid$colour, linewidth = grid$linewidth,
      linetype = grid$linetype)))
  }

  if (length(grid$spokes)) {
    a  <- grid$spokes * pi / 180
    sp <- data.frame(.u0 = 0, .v0 = 0,
                     .u1 = r_out * sin(a), .v1 = r_out * cos(a))
    sp_aes <- ggplot2::aes(x = .data$.u0, y = .data$.v0,
                           xend = .data$.u1, yend = .data$.v1)
    if (!is.null(cas)) gl <- c(gl, list(ggplot2::geom_segment(
      data = sp, sp_aes, inherit.aes = FALSE,
      colour = cas$colour, linewidth = cas$linewidth, alpha = cas$alpha)))
    gl <- c(gl, list(ggplot2::geom_segment(
      data = sp, sp_aes,
      inherit.aes = FALSE,
      colour = grid$colour, linewidth = grid$linewidth,
      linetype = grid$linetype)))
  }

  if (isTRUE(grid$outer)) {
    out_df <- .polar_circle(r_out)
    if (!is.null(cas)) gl <- c(gl, list(ggplot2::geom_path(
      data = out_df, ggplot2::aes(.data$.u, .data$.v), inherit.aes = FALSE,
      colour = cas$colour, linewidth = cas$linewidth, alpha = cas$alpha)))
    gl <- c(gl, list(ggplot2::geom_path(
      data = out_df, ggplot2::aes(.data$.u, .data$.v),
      inherit.aes = FALSE,
      colour = grid$colour, linewidth = grid$linewidth * 1.4,
      linetype = grid$outer_linetype)))
  }

  cmp <- .polar_compass(grid$compass)
  if (length(cmp)) {
    a  <- cmp * pi / 180
    cd <- data.frame(.u = r_out * (1 + grid$expand / 2) * sin(a),
                     .v = r_out * (1 + grid$expand / 2) * cos(a),
                     .lab = names(cmp) %||% as.character(cmp))
    gl <- c(gl, list(ggplot2::geom_text(
      data = cd, ggplot2::aes(.data$.u, .data$.v, label = .data$.lab),
      inherit.aes = FALSE,
      size = grid$compass_size, fontface = grid$compass_fontface)))
  }

  gl
}
