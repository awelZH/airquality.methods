# =============================================================================
#  Convenient entry point: compute and draw in one call
# =============================================================================

#' Compute polarPlot() and draw the result directly
#'
#' A thin wrapper around [openair::polarPlot()] and [polar_raster()].
#'
#' @param data Dataset for [openair::polarPlot()].
#' @param ... Arguments for [openair::polarPlot()] — `pollutant`, `type`,
#'   `statistic`, `k`, `min.bin`, and so on. `plot` is ignored.
#' @param args Named list with arguments for [polar_raster()], e.g.
#'   `list(limits = c(0, 100), option = "A")`. For a different colour scale:
#'   `list(scale = scale_fill_capped(limits = c(0, 100)))`.
#'
#' @details
#' Why the display arguments live in a list rather than also going through
#' `...`: [openair::polarPlot()] already claims the names `limits`, `type`,
#' and `x`, each with a different meaning than ours. A shared `...` would
#' silently route them to the wrong place. The names in `args` are checked
#' against the arguments of [polar_raster()] and
#' [ggplot2::scale_fill_viridis_c()], so typos are reported instead of
#' silently swallowed.
#'
#' The smoothing in `polarPlot()` is the expensive part (on the order of
#' seconds, a multiple of that with `type =`); drawing is cheap. So you don't
#' have to recompute just to recolour, the result carries the grid data as an
#' attribute; [polar_data()] retrieves it again:
#'
#' ```
#' p <- polar_plot(mydata, pollutant = "nox")
#' polar_raster(polar_data(p), scale = scale_fill_capped(palette = "Zissou 1"))
#' ```
#'
#' @return A ggplot object with the grid data in the `polar_data` attribute.
#'
#' @examples
#' \dontrun{
#' library(openair)
#' mydata |> polar_plot(pollutant = "so2", type = "year")
#' mydata |> polar_plot(pollutant = "nox", args = list(limits = c(50, 250)))
#' mydata |> polar_plot(pollutant = "nox",
#'                      args = list(scale = scale_fill_capped(probs = 0.02)))
#' }
#' @export
polar_plot <- function(data, ..., args = list()) {

  if (!requireNamespace("openair", quietly = TRUE)) {
    cli::cli_abort("{.fn polar_plot} requires the {.pkg openair} package.")
  }
  if (!is.list(args)) {
    cli::cli_abort(
      "{.arg args} must be a named list, e.g. {.code args = list(limits = c(0, 100))}."
    )
  }
  if (length(args) && (is.null(names(args)) || any(!nzchar(names(args))))) {
    cli::cli_abort("Every entry in {.arg args} must be named.")
  }

  # Allowed: everything polar_raster() knows, plus the arguments of the
  # default scale together with what it passes on to continuous_scale()
  # (name, breaks, labels, transform ...).
  known <- names(formals(polar_raster))
  for (f in c("scale_fill_viridis_c", "continuous_scale")) {
    known <- c(known, names(formals(getExportedValue("ggplot2", f))))
  }
  unknown <- setdiff(names(args), setdiff(unique(known), "..."))
  if (length(unknown)) {
    cli::cli_abort(c(
      "Unknown argument(s) in {.arg args}: {.val {unknown}}.",
      i = paste(
        "Allowed are the arguments of polar_raster() and",
        "scale_fill_viridis_c(). For a different colour scale, use",
        "args = list(scale = ...)."
      )
    ))
  }

  dots <- list(...)
  dots$plot <- NULL                      # we draw ourselves
  pp <- do.call(openair::polarPlot, c(list(data), dots, list(plot = FALSE)))

  p <- do.call(polar_raster, c(list(pp), args))
  attr(p, "polar_data") <- pp$data
  p
}

#' Get the grid data out of a plot made by polar_plot()
#'
#' @param p Result of [polar_plot()].
#' @return The `u`/`v`/`z` data frame, or `NULL`.
#' @export
polar_data <- function(p) attr(p, "polar_data")
