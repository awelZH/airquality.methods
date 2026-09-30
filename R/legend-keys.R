# Keys that stand beside a figure as plots of their own. A distribution panel draws four things at
# once: a median line, a mean line, an inner band and an outer band. ggplot2 will not build a legend
# for that, because the four come from four columns of the same rows rather than from one
# aesthetic -- and mapping them onto aesthetics in the *figure* only to get a legend would mean
# drawing the figure the wrong way round for the sake of its key. So the key is its own little plot:
# one made-up hump carrying the same four elements, with the aesthetics mapped *there*, where they
# cost nothing, so that ggplot2 lays the legend out itself and nothing can collide. Moved from
# ufp25 (0.6.0).

#' A key for a median-and-band distribution figure
#'
#' Draws a schematic hump with a median line, a mean line and two percentile
#' bands, and lets ggplot2 build the legend for it. Meant to be placed beside
#' the figure it explains, e.g. with `patchwork::wrap_plots()`.
#'
#' @param labels Names of the four elements, in this order: median, mean,
#'   inner band, outer band. All four are required -- a key that leaves one of
#'   them unnamed does not describe the figure.
#' @param colour Colour of the lines and of the bands.
#' @param alpha Opacity of the inner and the outer band. Give the *same* two
#'   numbers the figure uses, or the key stops describing it.
#' @param linetype Line types of the median and the mean, again as in the
#'   figure.
#' @param title Heading above the key, or `NULL`.
#' @param theme Plot theme; give the theme of the figure the key explains, so
#'   the two look like one. The key switches off its axes and grid on top of it.
#'
#' @return A ggplot.
#' @examples
#' band_key()
#' band_key(labels = c("Median", "Mittel", "P25-P75", "P10-P90"))
#' band_key(theme = ggplot2::theme_minimal(base_size = 9))
#' @export
band_key <- function(labels = c("Median", "Mittelwert", "P25\u2013P75",
                                "P10\u2013P90"),
                     colour = "grey25", alpha = c(0.30, 0.18),
                     linetype = c("solid", "42"), title = NULL,
                     theme = ggplot2::theme_minimal(base_size = 9)) {

  if (length(labels) != 4L || anyNA(labels) || !all(nzchar(labels))) {
    cli::cli_abort("{.arg labels} must name all four elements: median, mean,
                    inner band, outer band.")
  }
  if (length(alpha) != 2L || length(linetype) != 2L) {
    cli::cli_abort("{.arg alpha} and {.arg linetype} each take two values:
                    inner and outer band, median and mean.")
  }

  # A hump, not a straight line: a band around a line reads differently when
  # the line bends, and the key should look like the thing it explains.
  x   <- seq(0, 1, length.out = 200)
  mid <- exp(-((x - 0.45) / 0.27)^2)
  bands <- rbind(
    data.frame(x = x, lo = mid * 0.55, hi = mid * 1.72, band = labels[4L]),
    data.frame(x = x, lo = mid * 0.78, hi = mid * 1.30, band = labels[3L]))
  bands$band <- factor(bands$band, levels = labels[4:3])
  lines <- rbind(
    data.frame(x = x, y = mid,               line = labels[1L]),
    data.frame(x = x, y = mid * 1.18 + 0.02, line = labels[2L]))
  lines$line <- factor(lines$line, levels = labels[1:2])

  # The alpha is baked into the fill rather than mapped, so that the legend key
  # shows the shade the figure actually draws.
  fills <- stats::setNames(
    c(scales::alpha(colour, alpha[2L]), scales::alpha(colour, alpha[1L])),
    labels[4:3])

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = bands,
      ggplot2::aes(.data$x, ymin = .data$lo, ymax = .data$hi,
                   fill = .data$band)) +
    ggplot2::geom_line(
      data = lines,
      ggplot2::aes(.data$x, .data$y, linetype = .data$line),
      colour = colour, linewidth = 0.55) +
    ggplot2::scale_fill_manual(values = fills, name = NULL,
                               breaks = labels[3:4]) +
    ggplot2::scale_linetype_manual(values = stats::setNames(linetype,
                                                            labels[1:2]),
                                   name = NULL) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0.08, 0.10))) +
    ggplot2::labs(x = NULL, y = NULL, title = title) +
    ggplot2::guides(linetype = ggplot2::guide_legend(order = 1),
                    fill = ggplot2::guide_legend(order = 2)) +
    theme +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.key.width = grid::unit(14, "pt"),
      legend.spacing.y = grid::unit(1, "pt"),
      plot.title = ggplot2::element_text(size = ggplot2::rel(0.9)))
}
