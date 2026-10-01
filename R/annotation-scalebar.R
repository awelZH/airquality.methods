# =============================================================================
#  The distance scale bar, as a layer any map can carry
# -----------------------------------------------------------------------------
#  polar_map() draws one as part of its furniture, but a map does not have to
#  be a polar_map() to need one: a plain ggplot over a pinned basemap needs
#  the same bar. So the bar lives here, in one place, and
#  .polar_map_scalebar() in R/polar-map-decor.R is a thin wrapper that hands
#  polar_map_decor()'s settings on. Two implementations of the same bar would
#  be two chances to disagree about what "1 km" is.
#
#  Positioning is in data coordinates, i.e. LV95 metres, which is what makes it
#  work with coord_fixed(expand = FALSE): the bbox the coord is limited to is
#  the bbox the bar is placed in.
# =============================================================================

#' Distance scale bar for a map in LV95 metres
#'
#' A drop-in layer list for any ggplot whose axes are LV95 metres. The bar is
#' placed in map coordinates, so it needs to know the extent it is placed in --
#' pass the same [bbox_lv95()] the panel is limited to and the corner cannot
#' drift out of the panel.
#'
#' @param bbox A [bbox_lv95()] result, or any named vector carrying `xmin`,
#'   `xmax`, `ymin` and `ymax`.
#' @param position `"bottomright"` (default), `"bottomleft"`, `"topleft"`,
#'   `"topright"`, or an explicit `c(E, N)` in map coordinates naming the
#'   centre of the bar.
#' @param length_m Bar length in metres. `NULL` (default) picks a round fraction
#'   of the map width.
#' @param colour Colour of bar, ticks and label.
#' @param bg,bg_alpha Plaque behind the bar, for legibility over a basemap.
#'   `NA` or `NULL` for none.
#' @param size Label text size.
#' @param pad Distance from the panel edge, as a fraction of the map width.
#'
#' @return A list of layers, to be added to a ggplot.
#' @examples
#' s  <- data.frame(site = c("a", "b"), x_lv95 = c(2683000, 2685000),
#'                  y_lv95 = c(1250000, 1251000))
#' bb <- bbox_lv95(s, radius = 300)
#' ggplot2::ggplot() +
#'   ggplot2::coord_fixed(xlim = bb[c("xmin", "xmax")],
#'                        ylim = bb[c("ymin", "ymax")], expand = FALSE) +
#'   annotation_scalebar(bb)
#' @export
annotation_scalebar <- function(bbox,
                                position = "bottomright",
                                length_m = NULL,
                                colour   = "grey20",
                                bg       = "white",
                                bg_alpha = 0.85,
                                size     = 2.7,
                                pad      = 0.02) {

  need <- c("xmin", "xmax", "ymin", "ymax")
  if (!is.numeric(bbox) || !all(need %in% names(bbox))) {
    cli::cli_abort(c(
      "{.arg bbox} must carry {.field {need}}.",
      i = "{.fn bbox_lv95} builds one from your site table."
    ))
  }
  # As in polar_map_decor(): NULL is the readable way to say "no plaque", and
  # it must not reach the `if (is.na(bg))` below as a zero-length value.
  if (is.null(bg)) bg <- NA
  if (length(bg) != 1L) {
    cli::cli_abort(c(
      "{.arg bg} must be a single colour.",
      i = "{.code NA} or {.code NULL} draws no plaque."
    ))
  }

  .scalebar_layers(xlim = bbox[c("xmin", "xmax")],
                   ylim = bbox[c("ymin", "ymax")],
                   position = position, length_m = length_m, colour = colour,
                   bg = bg, bg_alpha = bg_alpha, size = size, pad = pad)
}

#' Length of the bar
#'
#' Its own function because [polar_map()] must know the length before the bar
#' is drawn, in order to stack the key above it. Two places deciding what "a
#' round distance" means would be two places to disagree.
#' @keywords internal
.scalebar_length <- function(length_m, xlim) {
  length_m %||% .nice_down(0.25 * diff(xlim), steps = c(1, 2, 2.5, 5))
}

#' The bar itself
#'
#' Split from [annotation_scalebar()] so that [polar_map()], which has already
#' validated its extent and its decor, does not validate them a second time.
#' @keywords internal
.scalebar_layers <- function(xlim, ylim, position, length_m, colour,
                             bg, bg_alpha, size, pad) {

  # A scale bar is read by its number, so the number has to be a round one;
  # the length follows from it, not the other way round.
  L <- .scalebar_length(length_m, xlim)
  lab <- if (L >= 1000) paste0(format(L / 1000, trim = TRUE), " km") else
    paste0(format(L, trim = TRUE), " m")

  h  <- 0.012 * diff(ylim)                       # cap height
  xy <- .polar_map_corner(position, xlim, ylim, L / 2, 2.2 * h, pad)
  cx <- xy[1L]; cy <- xy[2L]
  x0 <- cx - L / 2; x1 <- cx + L / 2

  out <- list()
  if (!isTRUE(is.na(bg))) {
    m <- 0.35 * h
    out <- c(out, list(ggplot2::annotate(
      "rect", xmin = x0 - 3 * m, xmax = x1 + 3 * m,
      ymin = cy - h - m, ymax = cy + 3.4 * h,
      fill = bg, alpha = bg_alpha, colour = NA)))
  }
  c(out, list(
    ggplot2::annotate("segment", x = x0, xend = x1, y = cy, yend = cy,
                      colour = colour, linewidth = 0.7),
    ggplot2::annotate("segment", x = c(x0, x1), xend = c(x0, x1),
                      y = cy - h, yend = cy + h,
                      colour = colour, linewidth = 0.7),
    ggplot2::annotate("text", x = cx, y = cy + 1.3 * h, label = lab,
                      size = size, vjust = 0, colour = colour)))
}
