# =============================================================================
#  Preparation: data, columns, facets, grid check
# =============================================================================

#' Get the data out of an openair object or a data frame
#' @keywords internal
.polar_data <- function(x) {
  dat <- if (inherits(x, "openair")) x$data else x
  if (is.list(dat) && !is.data.frame(dat)) {
    if (!length(dat)) cli::cli_abort("{.arg x} contains no data.")
    dat <- dat[[1L]]
  }
  if (!is.data.frame(dat)) {
    cli::cli_abort(c(
      "{.arg x} must be an openair object or a data frame.",
      x = "You supplied {.cls {class(x)}}."
    ))
  }
  as.data.frame(dat)
}

#' Determine facet columns
#'
#' `polarPlot()` names the grouping column differently depending on the call:
#' `default` (without `type`), the name of `type` (e.g. `season`), `variable`
#' (multiple pollutants) — combined, even two columns. So: anything that
#' isn't a coordinate or value counts as grouping; columns with only one
#' level (e.g. `default` = "all data") drop out.
#' @keywords internal
.polar_facets <- function(dat, facet, used) {
  if (inherits(facet, "formula")) return(all.vars(facet))
  if (length(facet) == 1L && is.na(facet)) return(character(0))
  if (is.character(facet)) return(facet)
  # polar_bin() records the grouping columns explicitly — then there's
  # nothing to guess (and the count column `n` stays out of it). An empty
  # recording (no `type`) is not treated as "no facets, ever": bind_rows()
  # carries the attribute of the first input along even after adding new
  # columns (e.g. a hand-added label to compare statistics), so an empty
  # `noted` here means "nothing recorded", not "recorded as nothing" — fall
  # through to guessing instead of silently dropping such columns.
  noted <- attr(dat, "polar_facets")
  if (length(noted)) return(intersect(noted, names(dat)))
  cand <- setdiff(names(dat), c(used, "x", "wd", "r", "n", ".cell"))
  cand[vapply(cand, function(k) length(unique(dat[[k]])) > 1L, logical(1))]
}

#' Determine the kind of grid
#'
#' `geom_raster()` requires consistently even spacing. Gaps in the grid — the
#' norm for thinly populated sectors when binning — trigger "Raster pixels
#' are placed at uneven intervals", and only while drawing, where the warning
#' can no longer be caught. Hence the distinction:
#'
#'   "regular"   consistently even spacing        -> geom_raster()
#'   "lattice"   gaps, but everything on a grid    -> geom_tile(), silent
#'   "irregular" uneven spacing                    -> geom_tile() with a note
#' @keywords internal
.polar_grid_kind <- function(a, tol = 1e-6) {
  a <- sort(unique(a[is.finite(a)]))
  if (length(a) < 3L) return("regular")
  d <- diff(a)
  m <- min(d)
  if (m <= 0) return("irregular")
  if (max(abs(d - m)) <= tol * m * length(d)) return("regular")
  k <- d / m
  if (max(abs(k - round(k))) <= tol * length(d)) "lattice" else "irregular"
}

#' Worst grid kind across all groups and both axes
#' @keywords internal
.polar_kind <- function(dat, u, v, grp) {
  rang <- c(regular = 1L, lattice = 2L, irregular = 3L)
  k <- unlist(lapply(split(seq_len(nrow(dat)), grp), function(i)
    c(.polar_grid_kind(dat[[u]][i]), .polar_grid_kind(dat[[v]][i]))))
  names(rang)[max(rang[k])]
}
