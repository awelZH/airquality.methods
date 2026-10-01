# =============================================================================
#  The radial axis
# -----------------------------------------------------------------------------
#  By default the wind-speed axis IS the plot's y-axis: a real axis, on the
#  far left, formattable through `theme(axis.text.y = )` and friends. That is
#  the strongest form available, and it is the default for a reason.
#
#  It cannot, however, be moved into the panel. ggplot2 draws axes in the
#  gtable *around* the panel, and `guide_axis(position = )` only knows the
#  four edges; nothing inside the panel is an axis in ggplot's sense.
#
#  What can be preserved is everything that made it an axis in practice:
#  the same `rings` as the grid, the same `.polar_axis_labels()` as the key
#  rose, and `axis.text.y` doing the styling. Only the rendering changes --
#  from gtable furniture to a layer on the panel. So
#  `theme(axis.text.y = element_text(...))` keeps working in either position,
#  which is the property that matters.
#
#  Inside the panel there is no axis line and no tick, and that is not a
#  simplification but the point: the rings already *are* the ticks. A label
#  sitting on its own ring is read off that ring directly, so a line drawn
#  through all of them would only add ink and one more thing to align.
#  (polar_map()'s key rose does keep its line -- it is a legend standing on
#  blank plaque, not a label on a ring it can lean against.)
# =============================================================================

#' Radial axis break labels
#'
#' `abs()` because the lower half of a polar plot's y-axis runs negative but
#' reads as a positive radius. Shared by [polar_raster()]'s y-axis and
#' [polar_map()]'s key rose, so the two can never drift apart.
#' @keywords internal
.polar_axis_labels <- function(breaks, unit = NULL) {
  lab <- format(abs(breaks), trim = TRUE)
  if (length(unit)) paste(lab, unit) else lab
}

#' Normalise the `axis_labels` argument
#'
#' `TRUE`/`FALSE` are the historical values and keep their meaning; the two
#' strings name the position explicitly. Both spellings of "centre" are
#' accepted -- the package writes British, but nobody should have to remember
#' that at the call site.
#' @keywords internal
.polar_axis_position <- function(axis_labels) {

  if (isTRUE(axis_labels))  return("left")
  if (isFALSE(axis_labels)) return("none")

  if (is.character(axis_labels) && length(axis_labels) == 1L) {
    pos <- switch(axis_labels,
                  left = "left", centre = "centre", center = "centre",
                  none = "none", NULL)
    if (!is.null(pos)) return(pos)
  }

  cli::cli_abort(c(
    "{.arg axis_labels} must be {.code TRUE}, {.code FALSE}, {.val left},
     {.val centre}, or {.val none}.",
    x = "You supplied {.val {axis_labels}}."
  ))
}

#' Look up a theme element, tolerating incomplete themes
#' @keywords internal
.polar_el <- function(theme, name) {
  el <- tryCatch(ggplot2::calc_element(name, .polar_theme(theme)),
                 error = function(e) NULL)
  if (inherits(el, "element_blank")) NULL else el
}

#' A fill only if it would actually be visible
#'
#' `theme_void()` carries `plot.background` as `"#00000000"` -- transparent
#' black. Taken at face value and then given an alpha, that paints the label
#' backings black, so anything see-through is treated as "no colour given".
#' @keywords internal
.polar_opaque <- function(fill) {
  if (is.null(fill) || length(fill) != 1L || is.na(fill)) return(NULL)
  if (identical(fill, "transparent")) return(NULL)
  a <- tryCatch(grDevices::col2rgb(fill, alpha = TRUE)["alpha", 1L],
                error = function(e) 255)
  if (a == 0) NULL else fill
}

#' The radial axis as layers inside the panel
#'
#' @param rings Ring radii, as resolved by [polar_raster()].
#' @param r_out Outer radius, used to size the label backing padding.
#' @param bearing Direction along which the labels sit, in compass degrees.
#' @param unit Passed to `.polar_axis_labels()`.
#' @param theme The plot's theme; supplies `axis.text.y`.
#' @keywords internal
.polar_axis_layers <- function(rings, r_out, bearing = 0, unit = "m/s",
                               theme = theme_polar()) {

  br <- sort(rings[rings > 0])
  if (!length(br)) return(list())

  text <- .polar_el(theme, "axis.text.y") %||% list(size = 8.8, colour = "grey35")

  # The label backing keeps the numbers readable where the surface is dark.
  # It is the panel's own background colour, so it reads as part of the plot
  # rather than as an annotation someone stuck on.
  bg <- .polar_opaque((.polar_el(theme, "panel.background") %||% list())$fill) %||%
        .polar_opaque((.polar_el(theme, "plot.background")  %||% list())$fill) %||%
        "white"

  a  <- bearing * pi / 180

  # One label per ring, centred on the ring it belongs to. The backing then
  # interrupts the ring exactly where the number sits, which is what makes
  # the pairing unambiguous without a leader line.
  list(ggplot2::geom_label(
    data = data.frame(.u = br * sin(a), .v = br * cos(a),
                      .lab = .polar_axis_labels(br, unit)),
    ggplot2::aes(.data$.u, .data$.v, label = .data$.lab),
    inherit.aes = FALSE, hjust = 0.5, vjust = 0.5,
    size = (text$size %||% 8.8) / ggplot2::.pt, colour = text$colour %||% "grey35",
    family = text$family %||% "", fontface = text$face %||% "plain",
    fill = bg, alpha = 0.65, linewidth = 0,
    label.padding = grid::unit(1.5, "pt")))
}
