# Grid descriptions, stacking years into cubes and the way back from a long
# table onto a cube.

# ---- Grid helpers ---------------------------------------------------------

#' Describe the grid of a stars raster
#'
#' Rasters sharing an identical `grid` string can be stacked without resampling.
#'
#' @param x A `stars` object.
#'
#' @return A one-row tibble with `res_x`, `res_y` and `grid` (resolution, size,
#'   origin).
#'
#' @keywords internal
raster_grid <- function(x) {
  x <- sf::st_normalize(x)
  dims <- stars::st_dimensions(x)
  xy <- attr(dims, "raster")$dimensions
  dx <- dims[[xy[[1]]]]
  dy <- dims[[xy[[2]]]]
  if (is.na(dx$delta) || is.na(dy$delta)) {
    cli::cli_abort("Only regular rasters are supported.")
  }

  tibble::tibble(
    res_x = abs(dx$delta),
    res_y = abs(dy$delta),
    grid = sprintf(
      "%g x %g m | %d x %d cells | origin %.1f / %.1f",
      abs(dx$delta), abs(dy$delta), dx$to, dy$to, dx$offset, dy$offset
    )
  )
}

#' Reassemble the attributes of a stars object onto given dimensions
#'
#' @param x A `stars` object.
#' @param dimensions Dimensions to attach, from [stars::st_dimensions()].
#'
#' @return A `stars` object.
#'
#' @keywords internal
with_dimensions <- function(x, dimensions) {
  names(x) |>
    rlang::set_names() |>
    purrr::map(\(attribute) unname_dim(x[[attribute]])) |>
    stars::st_as_stars(dimensions = dimensions)
}


# ---- Stacking years -------------------------------------------------------

#' Stack years into cubes (x, y, year) without resampling
#'
#' A `stars` cube has exactly one resolution per dimension, so one cube is
#' produced per group of identical grids: for raw data that is one per collection
#' and grid (several, if the resolution changes between years), for the result of
#' [align_to_reference()] or [align_to_grid()] usually exactly one.
#'
#' @param x Tibble from [read_geo_admin()], [align_to_reference()] or
#'   [align_to_grid()].
#' @param by Grouping columns in addition to the grid.
#'
#' @return A tibble with the `by` columns, `grid`, `years` and `cube`.
#'
#' @export
stack_years <- function(x, by = intersect(c("collection", "label"), names(x))) {
  check_names(names(x), c("year", "stars", by), "x")

  x |>
    dplyr::mutate(.grid = purrr::map_chr(.data$stars, \(s) raster_grid(s)$grid)) |>
    dplyr::arrange(.data$year) |>
    dplyr::summarise(
      years = list(.data$year),
      cube = list(stack_group(.data$stars, .data$year)),
      .by = dplyr::all_of(c(by, ".grid"))
    ) |>
    dplyr::rename(grid = ".grid") |>
    dplyr::arrange(dplyr::pick(dplyr::all_of(by)), purrr::map_int(.data$years, min))
}

#' Stack one group of rasters sharing a grid
#'
#' @param rasters List of `stars` objects.
#' @param years Their years.
#'
#' @return A `stars` cube with a `year` dimension.
#'
#' @keywords internal
stack_group <- function(rasters, years) {
  if (anyDuplicated(years) > 0) {
    cli::cli_abort("Several rasters share a year on the same grid: {.val {years[duplicated(years)]}}.")
  }
  attribute_names <- purrr::map(rasters, names)
  if (!all(purrr::map_lgl(attribute_names, \(n) identical(n, attribute_names[[1]])))) {
    cli::cli_abort("Attribute names differ between years, stacking is not possible.")
  }

  dimensions <- stars::st_dimensions(sf::st_normalize(rasters[[1]]))
  rasters <- purrr::map(rasters, \(r) with_dimensions(sf::st_normalize(r), dimensions))
  cube <- if (length(rasters) == 1) {
    # c() adds no dimension for a single object: duplicate, then keep one slice
    c(rasters[[1]], rasters[[1]], along = list(year = c(1L, 2L)))[, , , 1, drop = FALSE]
  } else {
    do.call(c, c(rasters, list(along = list(year = as.integer(years)))))
  }

  # years as instants, not intervals; that matters for gaps such as 2020, 2023, 2024
  stars::st_set_dimensions(cube, "year", values = as.integer(years), point = TRUE)
}


# ---- Table back to cube ---------------------------------------------------

#' Put a long table back onto the grid of a cube
#'
#' The counterpart to `as_tibble(cube)`. Cells are matched by the coordinates of
#' the template, so rows may be missing (after a [dplyr::filter()], say) and
#' become `NA`. The crs, the grid and years-as-instants survive, which they would
#' not with `stars::st_as_stars(dims = ...)`. Without a `year` column the result
#' is two-dimensional, for instance after summarising per cell.
#'
#' @param data Table with `x`, `y` (cell centres), optionally `year`, and the
#'   attribute columns.
#' @param template The cube whose grid is used.
#'
#' @return A `stars` object with every column except `x`, `y`, `year` as an
#'   attribute.
#'
#' @export
tibble_to_cube <- function(data, template) {
  check_names(names(data), c("x", "y"), "data")
  template <- sf::st_normalize(template)
  has_year <- "year" %in% names(data)
  template_has_year <- "year" %in% names(stars::st_dimensions(template))

  if (has_year && !template_has_year) {
    cli::cli_abort("{.arg data} has a {.field year} column, but {.arg template} has no year dimension.")
  }
  if (!has_year && template_has_year) {
    template <- dplyr::slice(template, "year", 1)
  }

  match_value <- function(values, grid_values) match(round(values, 6), round(grid_values, 6))
  idx <- cbind(
    match_value(data$x, stars::st_get_dimension_values(template, "x", center = TRUE)),
    match_value(data$y, stars::st_get_dimension_values(template, "y", center = TRUE))
  )
  if (has_year) {
    idx <- cbind(idx, match(data$year, stars::st_get_dimension_values(template, "year")))
  }

  if (anyNA(idx)) {
    cli::cli_abort(
      "{sum(!stats::complete.cases(idx))} row{?s} {?does/do} not lie on the grid of {.arg template}."
    )
  }
  if (anyDuplicated(idx) > 0) {
    cli::cli_abort("Several rows per cell{if (has_year) ' and year'}: summarise first.")
  }

  attributes <- setdiff(names(data), c("x", "y", "year"))
  if (length(attributes) == 0) {
    cli::cli_abort("{.arg data} holds no attribute columns besides x, y, year.")
  }
  size <- unname(dim(template))

  attributes |>
    rlang::set_names() |>
    purrr::map(\(attribute) {
      values <- data[[attribute]]
      layer <- array(values[NA_integer_], dim = size) # NA of the column's own type
      layer[idx] <- values
      layer
    }) |>
    stars::st_as_stars(dimensions = stars::st_dimensions(template))
}
