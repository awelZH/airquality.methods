# =============================================================================
#  Without smoothing: aggregating raw data into cells
# =============================================================================

#' The statistic a polar cell is condensed with
#'
#' Turns the `statistic` argument of [polar_bin()] and [polar_sector()] into a
#' function. Exported so that an analysis condensing values per wind direction
#' by itself means by `"median"` exactly what the binning means by it.
#'
#' @param statistic `"mean"`, `"median"`, `"min"`, `"max"`, `"sd"`, `"n"`
#'   (alias `"frequency"`), or a function, which is returned as it is. All
#'   named statistics drop `NA`; `"n"` counts every value it is given.
#' @return A function of one numeric vector.
#' @examples
#' polar_statfun("median")(c(1, NA, 3, 10))
#' @export
polar_statfun <- function(statistic) {
  if (is.function(statistic)) return(statistic)
  switch(
    statistic,
    mean      = function(x) mean(x, na.rm = TRUE),
    median    = function(x) stats::median(x, na.rm = TRUE),
    min       = function(x) min(x, na.rm = TRUE),
    max       = function(x) max(x, na.rm = TRUE),
    sd        = function(x) stats::sd(x, na.rm = TRUE),
    n         = ,
    frequency = function(x) length(x),
    cli::cli_abort(c(
      "Unknown {.arg statistic}: {.val {statistic}}.",
      i = "Allowed are \"mean\", \"median\", \"min\", \"max\", \"sd\", \"n\", or a function."
    ))
  )
}

#' @keywords internal
.polar_prepare <- function(data, pollutant, ws, wd, type, ws_max) {

  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")

  # NULL is the other natural spelling of "no limit"; anything else would
  # reach `is.finite()` below and fail there with a base error about a
  # zero-length condition, which says nothing about the argument at fault.
  if (is.null(ws_max)) ws_max <- NA
  if (length(ws_max) != 1L || !(is.numeric(ws_max) || is.logical(ws_max))) {
    cli::cli_abort(c(
      "{.arg ws_max} must be a single number.",
      i = "{.code NA} or {.code NULL} means no limit."
    ))
  }

  # Grouping columns, via openair::cutData() if necessary
  if (length(type)) {
    missing_cols <- setdiff(type, names(data))
    if (length(missing_cols)) {
      if (!requireNamespace("openair", quietly = TRUE)) {
        cli::cli_abort(c(
          "Column(s) not found: {.val {missing_cols}}.",
          i = "Derived types like \"season\" require the {.pkg openair} package."
        ))
      }
      data <- openair::cutData(data, type = missing_cols)
      missing_cols <- setdiff(type, names(data))
      if (length(missing_cols)) {
        cli::cli_abort(
          "{.fn openair::cutData} did not return column(s): {.val {missing_cols}}."
        )
      }
    }
  }

  missing_cols <- setdiff(c(ws, wd, pollutant, type), names(data))
  if (length(missing_cols)) {
    cli::cli_abort(c(
      "Column(s) not found: {.val {missing_cols}}.",
      i = "Available: {.val {names(data)}}."
    ))
  }

  keep <- stats::complete.cases(data[c(ws, wd, pollutant)])
  if (length(type)) keep <- keep & stats::complete.cases(data[type])
  if (isTRUE(is.finite(ws_max))) keep <- keep & data[[ws]] <= ws_max
  d <- data[keep, , drop = FALSE]
  if (!nrow(d)) {
    cli::cli_abort("Nothing left after removing incomplete rows.")
  }
  d
}

#' @keywords internal
.polar_aggregate <- function(values, by, fun, min_n) {
  d <- by
  d$.value <- values
  out <- as.data.frame(d) |>
    dplyr::summarise(z = fun(.data$.value), n = dplyr::n(), .by = dplyr::all_of(names(by))) |>
    dplyr::filter(.data$n >= min_n)
  if (!nrow(out)) cli::cli_abort("No cell reaches {.arg min_n} = {min_n}.")
  as.data.frame(out)
}

#' Aggregate raw data into a u/v grid — without smoothing
#'
#' The alternative to [openair::polarPlot()]: instead of smoothing a surface,
#' measurements are sorted into square cells of the wind-vector plane and
#' summarised per cell. Nothing is interpolated — empty cells stay empty. The
#' result has the same shape as `polarPlot(...)$data` and goes straight into
#' [polar_raster()].
#'
#' @param data Dataset with wind speed, wind direction, and the measured
#'   quantity.
#' @param pollutant Name of the column with the measured quantity.
#' @param ws,wd Column names for wind speed and direction (degrees,
#'   meteorological convention: the direction the wind blows from).
#' @param res Cell edge length, in units of `ws`.
#' @param statistic `"mean"` (default), `"median"`, `"min"`, `"max"`, `"sd"`,
#'   `"n"` (number of measurements per cell), or a custom function that
#'   condenses a vector to a number, e.g. `function(x) quantile(x, 0.95)`.
#' @param min_n Cells with fewer measurements are dropped. Without smoothing,
#'   the edges are thinly populated; `min_n` keeps out single values that
#'   would otherwise show up as strong colour blotches.
#' @param ws_max Limit the radius; `NA` (or `NULL`) = no limit.
#' @param type Column names for grouping (later facets). Names that aren't a
#'   column are passed to `openair::cutData()` — so `"season"`, `"year"`,
#'   `"weekday"`, and related also work.
#'
#' @return A data frame with `u`, `v`, `z`, `n`, and the grouping columns. The
#'   grouping columns are recorded in the `polar_facets` attribute, so
#'   [polar_raster()] doesn't have to guess.
#'
#' @section Cell shape and choosing `res`:
#' Binning happens in the u/v plane, so cells are squares of equal area.
#' Outward, a cell therefore covers an ever smaller angular range — which is
#' exactly where the lower bound for `res` comes from: wind directions are
#' usually recorded in fixed steps (often 10 degrees). Two adjacent
#' directions are about `dwd * pi/180 * r` apart at radius `r`. If `res` is
#' smaller than that, empty cells remain between directions, and the image
#' frays into rays toward the edge.
#'
#' As a rule of thumb, choose `res >= r_max * dwd * pi/180` for a closed
#' image out to the edge — for 10-degree steps and `r_max = 14` roughly 2.4.
#' Smaller values show more structure near the centre and gaps toward the
#' edge; that isn't a bug, it's the honest resolution of the raw data.
#'
#' Binning into sectors (wind direction x speed class) instead — a better fit
#' for the structure of such data, and gap-free — needs a different cell
#' geometry: ring segments instead of squares. This function doesn't do that.
#'
#' @examples
#' \dontrun{
#' library(openair)
#' mydata |> polar_bin("nox", res = 0.5) |> polar_raster()
#' mydata |> polar_bin("nox", res = 1, min_n = 10, type = "season") |> polar_raster()
#' mydata |> polar_bin("nox", statistic = "n") |> polar_raster(name = "Count")
#' }
#' @export
polar_bin <- function(data,
                      pollutant,
                      ws        = "ws",
                      wd        = "wd",
                      res       = 0.5,
                      statistic = "mean",
                      min_n     = 1,
                      ws_max    = NA,
                      type      = NULL) {

  if (!is.numeric(res) || length(res) != 1L || res <= 0) {
    cli::cli_abort("{.arg res} must be a single positive number.")
  }
  fun <- polar_statfun(statistic)
  d   <- .polar_prepare(data, pollutant, ws, wd, type, ws_max)

  a  <- d[[wd]] * pi / 180
  by <- c(list(u = res * round_off(d[[ws]] * sin(a) / res),
               v = res * round_off(d[[ws]] * cos(a) / res)),
          stats::setNames(lapply(type, function(k) d[[k]]), type))

  out <- .polar_aggregate(d[[pollutant]], by, fun, min_n)
  out <- out[order(out$v, out$u), c("u", "v", "z", "n", type), drop = FALSE]
  rownames(out) <- NULL

  # Careful: `attr(x, "a") <- NULL` deletes the attribute. Without the empty
  # vector, polar_raster() falls back to guessing and may facet by a helper
  # column in doubt.
  attr(out, "polar_facets") <- if (length(type)) type else character(0)
  # Without smoothing, bilinear interpolation between cells would be
  # invented detail — polar_raster() reads this off here.
  attr(out, "polar_interpolate") <- FALSE
  out
}


#' Aggregate raw data into sectors — without smoothing, without gaps
#'
#' Like [polar_bin()], but the cells follow the structure of the data:
#' binning is by wind direction and speed class, and each cell is a ring
#' segment. Since wind directions already come in fixed steps, this leaves no
#' gaps — unlike the square grid, which frays into rays toward the edge.
#'
#' [polar_raster()] draws this as a polygon path; the return value therefore
#' contains the segment vertices, not their midpoints.
#'
#' @inheritParams polar_bin
#' @param wd_res Width of the direction sectors in degrees. Must divide 360.
#'   The raw data's step size (often 10) or a multiple of it is sensible.
#' @param ws_res Width of the speed classes, in units of `ws`.
#' @param n_arc Vertices per arc. `NULL` = automatic from `wd_res`.
#'
#' @return A data frame with the polygon vertices (`u`, `v`), the cell id
#'   `.cell`, `z`, `n`, and the grouping columns. Goes straight into
#'   [polar_raster()].
#'
#' @examples
#' \dontrun{
#' library(openair)
#' mydata |> polar_sector("nox", wd_res = 10, ws_res = 1) |> polar_raster()
#' mydata |> polar_sector("nox", wd_res = 30, ws_res = 2, min_n = 20) |>
#'   polar_raster(name = "NOx")
#' }
#' @export
polar_sector <- function(data,
                         pollutant,
                         ws        = "ws",
                         wd        = "wd",
                         wd_res    = 10,
                         ws_res    = 1,
                         statistic = "mean",
                         min_n     = 1,
                         ws_max    = NA,
                         type      = NULL,
                         n_arc     = NULL) {

  if (!is.numeric(wd_res) || length(wd_res) != 1L || wd_res <= 0 || wd_res > 180) {
    cli::cli_abort("{.arg wd_res} must be a single number between 0 and 180.")
  }
  if (abs(360 / wd_res - round(360 / wd_res)) > 1e-8) {
    cli::cli_abort("{.arg wd_res} must divide 360 (e.g. 5, 10, 15, 22.5, 30, 45).")
  }
  if (!is.numeric(ws_res) || length(ws_res) != 1L || ws_res <= 0) {
    cli::cli_abort("{.arg ws_res} must be a single positive number.")
  }

  fun <- polar_statfun(statistic)
  d   <- .polar_prepare(data, pollutant, ws, wd, type, ws_max)

  nsek <- round(360 / wd_res)
  by <- c(list(wd_c = (round_off((d[[wd]] %% 360) / wd_res) %% nsek) * wd_res,
               ws_lo = floor(d[[ws]] / ws_res) * ws_res),
          stats::setNames(lapply(type, function(k) d[[k]]), type))

  cell <- .polar_aggregate(d[[pollutant]], by, fun, min_n)

  # --- Ring segment vertices --------------------------------------------
  na <- n_arc %||% max(2L, ceiling(wd_res / 5) + 1L)
  ang <- (outer(cell$wd_c, seq(-0.5, 0.5, length.out = na) * wd_res, "+")) * pi / 180
  r_o <- cell$ws_lo + ws_res
  r_i <- cell$ws_lo

  # outer left to right, inner back -> closed ring. For the innermost band,
  # the inner arc collapses to the origin, which geom_polygon() draws as a
  # pie slice without complaint.
  uu <- cbind(r_o * sin(ang), r_i * sin(ang[, na:1, drop = FALSE]))
  vv <- cbind(r_o * cos(ang), r_i * cos(ang[, na:1, drop = FALSE]))

  nv  <- 2L * na
  idx <- rep(seq_len(nrow(cell)), each = nv)
  out <- data.frame(.cell = idx,
                    u     = as.vector(t(uu)),
                    v     = as.vector(t(vv)),
                    z     = cell$z[idx],
                    n     = cell$n[idx])
  for (k in type) out[[k]] <- cell[[k]][idx]
  rownames(out) <- NULL

  # Same trap as in polar_bin(): a bare `type` is NULL without `type =`, and
  # assigning NULL deletes the attribute instead of recording "no grouping".
  attr(out, "polar_facets")     <- if (length(type)) type else character(0)
  attr(out, "polar_geom")       <- "polygon"
  attr(out, "polar_interpolate") <- FALSE
  out
}
