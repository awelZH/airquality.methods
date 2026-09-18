# Normalising rasters onto a common grid: GDAL resampling, temporal matching,
# and the two entry points align_to_reference() and align_to_grid().

# ---- Resampling -----------------------------------------------------------

#' Resample a raster onto the grid of a template
#'
#' With `method = "average"` values are area-weighted: a coarser template yields
#' the mean of the overlapping cells, a finer one gives each target cell the
#' value of the source cell it falls into. With `method = "sum"` values are
#' distributed by area, so the total is preserved both when coarsening and when
#' refining (GDAL >= 3.1). `NA` cells are ignored.
#'
#' GDAL's `average` writes one row of cells beyond the edge of the source when
#' refining. Target cells without real area overlap are therefore reset to `NA`
#' afterwards (when both share a crs).
#'
#' @param x A `stars` object, two-dimensional, any number of attributes.
#' @param template A `stars` object whose grid is adopted.
#' @param method GDAL resampling method (`"average"`, `"sum"`, `"near"`,
#'   `"bilinear"`, `"mode"`, ...).
#' @param no_data_value Internal NoData value handed to GDAL. Must not occur in
#'   the data; this is checked.
#'
#' @return A `stars` object on the template grid.
#'
#' @keywords internal
resample_to_grid <- function(x, template, method = "average", no_data_value = -9999) {
  template <- sf::st_normalize(template)
  dimensions <- stars::st_dimensions(template)
  target_dim <- unname(dim(template[[1]]))
  x <- sf::st_normalize(x)
  overlap <- if (sf::st_crs(x) == sf::st_crs(template)) source_overlap_mask(x, template) else NULL

  collides <- purrr::map_lgl(names(x), \(attribute) {
    values <- x[[attribute]]
    is.numeric(values) && any(values == no_data_value, na.rm = TRUE)
  })
  if (any(collides)) {
    cli::cli_abort(c(
      "x" = "{.arg no_data_value} ({no_data_value}) occurs in attribute{?s} {.val {names(x)[collides]}}.",
      "i" = "Those cells would become NA. Choose a {.arg no_data_value} outside the data range."
    ))
  }

  names(x) |>
    rlang::set_names() |>
    purrr::map(\(attribute) {
      warped <- stars::st_warp(
        x[attribute],
        dest = template[1],
        method = method,
        use_gdal = TRUE,
        no_data_value = no_data_value
      )
      values <- unname_dim(warped[[1]])
      if (!identical(dim(values), target_dim)) {
        cli::cli_abort("Resampling {.val {attribute}} produced a raster of a different size.")
      }
      if (!is.null(overlap)) values[!overlap] <- NA
      values
    }) |>
    stars::st_as_stars(dimensions = dimensions)
}

#' Mark target cells that overlap the extent of the source by area
#'
#' @param x Source `stars` object.
#' @param template Target `stars` object.
#' @param tolerance Relative tolerance, as a fraction of the cell size.
#'
#' @return A logical matrix the size of the template grid (x, y).
#'
#' @keywords internal
source_overlap_mask <- function(x, template, tolerance = 1e-6) {
  source_bbox <- sf::st_bbox(x)
  dims <- stars::st_dimensions(sf::st_normalize(template))
  xy <- attr(dims, "raster")$dimensions
  dx <- dims[[xy[[1]]]]
  dy <- dims[[xy[[2]]]]

  x_start <- dx$offset + (seq_len(dx$to) - 1) * dx$delta
  x_end <- x_start + dx$delta
  y_start <- dy$offset + (seq_len(dy$to) - 1) * dy$delta
  y_end <- y_start + dy$delta

  overlap_x <- pmin(pmax(x_start, x_end), source_bbox[["xmax"]]) -
    pmax(pmin(x_start, x_end), source_bbox[["xmin"]]) > tolerance * abs(dx$delta)
  overlap_y <- pmin(pmax(y_start, y_end), source_bbox[["ymax"]]) -
    pmax(pmin(y_start, y_end), source_bbox[["ymin"]]) > tolerance * abs(dy$delta)

  outer(overlap_x, overlap_y)
}

#' Resampling method per collection, from the specs
#'
#' @param specs Collection specs, see [geo_admin_specs()].
#'
#' @return A named character vector, collection id to GDAL method.
#'
#' @keywords internal
resampling_methods <- function(specs = geo_admin_specs()) {
  purrr::map_chr(specs, \(spec) spec$resampling %||% "average")
}

#' Look a value up for one collection
#'
#' A single unnamed value applies to every collection; a named vector is looked
#' up by collection id, falling back to `default_method`.
#'
#' @param collection Collection id.
#' @param methods A single value or a named vector.
#' @param default_method Fallback for collections not named in `methods`.
#'
#' @return A single value.
#'
#' @keywords internal
method_for <- function(collection, methods, default_method) {
  if (length(methods) == 1 && is.null(names(methods))) {
    return(methods[[1]])
  }

  if (collection %in% names(methods)) methods[[collection]] else default_method
}


# ---- Temporal matching ----------------------------------------------------

year_match_choices <- c("exact", "nearest", "linear")

#' Validate the year_match argument
#'
#' @param year_match One value or a named vector of values.
#' @param call Environment used for the error call.
#'
#' @return `TRUE`, invisibly.
#'
#' @keywords internal
check_year_match <- function(year_match, call = rlang::caller_env()) {
  invalid <- setdiff(year_match, year_match_choices)

  if (!is.character(year_match) || length(year_match) == 0 || length(invalid) > 0) {
    cli::cli_abort(
      "{.arg year_match} allows only {.val {year_match_choices}}, got {.val {invalid}}.",
      call = call
    )
  }

  invisible(TRUE)
}

#' Choose the source year(s) for one target year
#'
#' * `"exact"`: the target year itself, nothing else (the default)
#' * `"nearest"`: closest year within `max_year_diff`, ties go to the earlier one
#' * `"linear"`: linear interpolation between the last year before and the first
#'   after (both within `max_year_diff`). Outside the series the nearest year is
#'   taken, as with `"nearest"`; it never extrapolates.
#'
#' @param rows Rows of one collection from [read_geo_admin()].
#' @param target_year The year to produce.
#' @param year_match One of `"exact"`, `"nearest"`, `"linear"`.
#' @param max_year_diff Largest accepted distance in years.
#'
#' @return `NULL`, or a list with `rows` (1 or 2 rows), `weights` and `status`.
#'
#' @keywords internal
select_year_sources <- function(rows, target_year, year_match, max_year_diff) {
  rows <- dplyr::arrange(rows, .data$year)

  exact <- dplyr::filter(rows, .data$year == target_year)
  if (nrow(exact) > 0) {
    return(list(rows = dplyr::slice_head(exact, n = 1), weights = 1, status = "same_year"))
  }
  if (year_match == "exact") {
    return(NULL)
  }

  before <- rows |>
    dplyr::filter(.data$year < target_year, target_year - .data$year <= max_year_diff) |>
    dplyr::slice_tail(n = 1)
  after <- rows |>
    dplyr::filter(.data$year > target_year, .data$year - target_year <= max_year_diff) |>
    dplyr::slice_head(n = 1)

  if (year_match == "linear" && nrow(before) == 1 && nrow(after) == 1) {
    weight_after <- (target_year - before$year) / (after$year - before$year)

    return(list(
      rows = dplyr::bind_rows(before, after),
      weights = c(1 - weight_after, weight_after),
      status = "interpolated"
    ))
  }

  candidates <- dplyr::bind_rows(before, after)
  if (nrow(candidates) == 0) {
    return(NULL)
  }
  nearest <- candidates |>
    dplyr::mutate(.year_diff = abs(.data$year - target_year)) |>
    dplyr::arrange(.data$.year_diff, .data$year) |>
    dplyr::slice_head(n = 1) |>
    dplyr::select(-".year_diff")

  list(rows = nearest, weights = 1, status = "nearest_year")
}


# ---- Reference grids ------------------------------------------------------

#' Build a reference grid from an extent and a cell size
#'
#' The extent is rounded outwards to multiples of the cell size, so for cell
#' sizes such as 100, 200, 500 or 1000 m the grid aligns with the STATPOP grid.
#'
#' @param bbox Extent (LV95 vector or `sf`/`bbox`/`stars` object).
#' @param cellsize Cell size in metres.
#' @param crs Coordinate reference system of the grid.
#'
#' @return An empty `stars` object carrying only the geometry.
#'
#' @examples
#' grid <- make_reference_grid(bbox_zh_lv95, cellsize = 250)
#' dim(grid)
#'
#' @export
make_reference_grid <- function(bbox = bbox_zh_lv95, cellsize = 100, crs = 2056) {
  if (!is.numeric(cellsize) || length(cellsize) != 1 || cellsize <= 0) {
    cli::cli_abort("{.arg cellsize} must be a single positive number.")
  }
  bb <- as_bbox(bbox, crs)
  extent <- c(
    xmin = floor(bb[["xmin"]] / cellsize) * cellsize,
    ymin = floor(bb[["ymin"]] / cellsize) * cellsize,
    xmax = ceiling(bb[["xmax"]] / cellsize) * cellsize,
    ymax = ceiling(bb[["ymax"]] / cellsize) * cellsize
  )

  stars::st_as_stars(
    sf::st_bbox(extent, crs = sf::st_crs(crs)),
    nx = as.integer(round((extent[["xmax"]] - extent[["xmin"]]) / cellsize)),
    ny = as.integer(round((extent[["ymax"]] - extent[["ymin"]]) / cellsize)),
    values = NA_real_
  )
}

#' Derive an empty template from a grid
#'
#' @param grid A `stars` object, `stars_proxy`, or the path/url of a raster file.
#'   Only the geometry is used; a file is never modified.
#' @param crs Coordinate reference system to assume when the grid carries none.
#' @param call Environment used for the error call.
#'
#' @return An empty `stars` object carrying only the geometry.
#'
#' @keywords internal
grid_template <- function(grid, crs = 2056, call = rlang::caller_env()) {
  if (rlang::is_string(grid)) {
    source <- if (grepl("^https?://", grid)) paste0("/vsicurl/", grid) else grid
    grid <- stars::read_stars(source, proxy = TRUE)
  }
  if (!inherits(grid, "stars")) {
    cli::cli_abort(
      "{.arg grid} must be a stars object or a path to a raster file.",
      call = call
    )
  }
  if (!identical(stars::st_raster_type(grid), "regular")) {
    cli::cli_abort("{.arg grid} must be a regular, unrotated raster.", call = call)
  }

  bb <- sf::st_bbox(grid)
  if (is.na(sf::st_crs(bb))) {
    cli::cli_warn("{.arg grid} carries no crs, assuming {sf::st_crs(crs)$input}.")
    attr(bb, "crs") <- sf::st_crs(crs)
  }
  xy <- attr(stars::st_dimensions(grid), "raster")$dimensions
  size <- dim(grid)[xy]

  stars::st_as_stars(bb, nx = size[[1]], ny = size[[2]], values = NA_real_)
}


# ---- Aligning -------------------------------------------------------------

#' Bring every collection onto one template for one target year
#'
#' @inheritParams align_to_grid
#' @param target_year The year to produce.
#' @param template The `stars` template to align onto.
#' @param candidates Rows of [read_geo_admin()] to draw from.
#'
#' @return A list with `layers` (named arrays), `log` (one row per attribute) and
#'   `rows` (the rows of `candidates` actually used).
#'
#' @keywords internal
align_layers <- function(target_year, template, candidates, methods, default_method,
                         year_match, max_year_diff, no_data_value) {
  per_collection <- candidates |>
    dplyr::distinct(.data$collection, .data$label) |>
    vctrs::vec_chop() |>
    purrr::map(\(target) {
      method <- method_for(target$collection, methods, default_method)
      matching <- method_for(target$collection, year_match, "exact")
      collection_rows <- dplyr::filter(candidates, .data$collection == target$collection)
      selection <- select_year_sources(collection_rows, target_year, matching, max_year_diff)

      if (is.null(selection)) {
        # keep the structure: attributes of the most recent year, filled with NA
        attribute_names <- collection_rows |>
          dplyr::slice_max(.data$year, n = 1, with_ties = FALSE) |>
          dplyr::pull("stars") |>
          purrr::pluck(1) |>
          names()
        layers <- purrr::map(
          rlang::set_names(attribute_names),
          \(n) array(NA_real_, dim = unname(dim(template[[1]])))
        )
        log <- tibble::tibble(
          collection = target$collection, label = target$label, attribute = attribute_names,
          source_year = NA_integer_, source_year_after = NA_integer_, weight_after = NA_real_,
          source_res_x = NA_real_, source_res_y = NA_real_,
          method = method, year_match = matching, status = "missing",
          total_source = NA_real_, total_aligned = NA_real_
        )

        return(list(layers = layers, log = log, rows = NULL))
      }

      sources <- selection$rows$stars
      resampled <- purrr::map(sources, \(source) resample_to_grid(source, template, method, no_data_value))
      attributes <- names(resampled[[1]])
      if (!all(purrr::map_lgl(resampled, \(r) identical(names(r), attributes)))) {
        cli::cli_abort(
          "{target$label}: attributes of years {selection$rows$year} differ, interpolation is not possible."
        )
      }

      # weighted sum: one source year has weight 1, interpolation has two weights
      layers <- purrr::map(rlang::set_names(attributes), \(n) {
        purrr::map2(resampled, selection$weights, \(r, w) unname_dim(r[[n]]) * w) |>
          purrr::reduce(`+`)
      })

      # log the totals, so mass conservation under "sum" stays verifiable
      totals <- method == "sum"
      total_source <- purrr::map_dbl(attributes, \(n) {
        sum(purrr::map2_dbl(sources, selection$weights, \(s, w) w * sum(s[[n]], na.rm = TRUE)))
      })
      source_grid <- raster_grid(sources[[1]])
      interpolated <- selection$status == "interpolated"

      log <- tibble::tibble(
        collection = target$collection, label = target$label, attribute = attributes,
        source_year = as.integer(selection$rows$year[[1]]),
        source_year_after = if (interpolated) as.integer(selection$rows$year[[2]]) else NA_integer_,
        weight_after = if (interpolated) selection$weights[[2]] else NA_real_,
        source_res_x = source_grid$res_x, source_res_y = source_grid$res_y,
        method = method, year_match = matching, status = selection$status,
        total_source = if (totals) total_source else NA_real_,
        total_aligned = if (totals) purrr::map_dbl(attributes, \(n) sum(layers[[n]], na.rm = TRUE)) else NA_real_
      )

      list(layers = layers, log = log, rows = selection$rows)
    })

  list(
    layers = purrr::list_flatten(purrr::map(per_collection, "layers")),
    log = purrr::list_rbind(purrr::map(per_collection, "log")),
    rows = purrr::list_rbind(purrr::map(per_collection, "rows"))
  )
}

#' Assemble attribute arrays into a stars object on the template
#'
#' @param layers Named list of arrays.
#' @param template The `stars` template.
#' @param call Environment used for the error call.
#'
#' @return A `stars` object.
#'
#' @keywords internal
combine_layers <- function(layers, template, call = rlang::caller_env()) {
  duplicated_names <- unique(names(layers)[duplicated(names(layers))])
  if (length(duplicated_names) > 0) {
    cli::cli_abort(c(
      "x" = "Duplicate attribute names: {.val {duplicated_names}}.",
      "i" = "Set unique {.arg name}/{.arg variables} in the collection specs."
    ), call = call)
  }

  stars::st_as_stars(layers, dimensions = stars::st_dimensions(template))
}

#' Carry the noloc columns of the row actually used
#'
#' @param rows Rows used for this year.
#' @param has_noloc Whether the input carries noloc columns at all.
#'
#' @return A one-row tibble.
#'
#' @keywords internal
noloc_columns <- function(rows, has_noloc) {
  if (!has_noloc) {
    return(tibble::tibble(.rows = 1))
  }
  empty <- tibble::tibble(noloc_subtracted = NA_real_, noloc_unmatched = NA_real_, noloc = list(NULL))
  if (is.null(rows) || !"noloc_subtracted" %in% names(rows)) {
    return(empty)
  }
  rows <- dplyr::filter(rows, !is.na(.data$noloc_subtracted))
  if (nrow(rows) == 0) {
    return(empty)
  }

  dplyr::select(dplyr::slice_head(rows, n = 1), dplyr::any_of(names(empty)))
}

#' Align collections onto the grid of a reference collection, year by year
#'
#' For every year of the reference collection its own raster is used as the
#' template, so the reference resolution may change between years. The reference
#' itself stays untouched; every other collection is brought onto that grid with
#' GDAL, averaged or summed according to `methods`.
#'
#' @param x Tibble from [read_geo_admin()] holding at least two collections.
#' @param reference Collection id of the reference (STATPOP by default).
#' @param methods Named vector, collection id to GDAL method (by default from
#'   [geo_admin_specs()]: STATPOP `"sum"`, everything else `"average"`).
#' @param default_method Method for collections not named in `methods`.
#' @param year_match Temporal matching. `"exact"` (the default) uses data of the
#'   same year only. Optionally one value for every collection (`"nearest"`,
#'   `"linear"`) or a named vector; collections not named stay `"exact"`. See
#'   [select_year_sources()].
#' @param max_year_diff Largest accepted distance in years for `"nearest"` and
#'   `"linear"`.
#' @param no_data_value See [resample_to_grid()].
#'
#' @return A tibble with one row per reference year: `year`, `reference`,
#'   `res_x`, `res_y`, `grid`, `stars`, `sources` (the per-attribute log) and the
#'   noloc columns.
#'
#' @export
align_to_reference <- function(x,
                               reference = "ch.bfs.statistik-bevoelkerung_haushalte",
                               methods = resampling_methods(),
                               default_method = "average",
                               year_match = "exact",
                               max_year_diff = Inf,
                               no_data_value = -9999) {
  check_year_match(year_match)
  check_names(names(x), c("collection", "label", "year", "stars"), "x")

  if (!reference %in% x$collection) {
    cli::cli_abort(c(
      "x" = "Reference collection {.val {reference}} is not part of {.arg x}.",
      "i" = "Present: {.val {unique(x$collection)}}"
    ))
  }
  if (dplyr::n_distinct(x$collection) < 2) {
    cli::cli_abort(c(
      "x" = "{.arg x} holds only one collection, there is nothing to align.",
      "i" = "Use {.fn align_to_grid} for a freely chosen grid."
    ))
  }

  references <- dplyr::filter(x, .data$collection == reference)
  targets <- dplyr::filter(x, .data$collection != reference)
  if (anyDuplicated(references$year) > 0) {
    cli::cli_abort("The reference collection has several rasters for one year.")
  }
  has_noloc <- "noloc_subtracted" %in% names(x)

  references |>
    dplyr::arrange(.data$year) |>
    vctrs::vec_chop() |>
    purrr::map(\(ref) {
      template <- sf::st_normalize(ref$stars[[1]])
      info <- raster_grid(template)
      cli::cli_inform("Reference year {ref$year} ({info$grid}).")

      aligned <- align_layers(
        ref$year, template, targets, methods, default_method,
        year_match, max_year_diff, no_data_value
      )
      reference_layers <- purrr::map(rlang::set_names(names(template)), \(n) unname_dim(template[[n]]))

      tibble::tibble(year = as.integer(ref$year), reference = ref$collection) |>
        dplyr::bind_cols(info) |>
        dplyr::mutate(
          stars = list(combine_layers(c(reference_layers, aligned$layers), template)),
          sources = list(aligned$log)
        ) |>
        dplyr::bind_cols(noloc_columns(dplyr::bind_rows(ref, aligned$rows), has_noloc))
    }) |>
    purrr::list_rbind()
}

#' Normalise every collection onto a freely chosen grid
#'
#' Each collection is brought onto `grid` per year with GDAL, finer or coarser:
#' summed (STATPOP, the total is preserved) or averaged (everything else),
#' according to `methods`. Since all years share one grid, [stack_years()] on the
#' result yields exactly one cube.
#'
#' The `total_source` and `total_aligned` columns of `sources` show the totals
#' before and after resampling under `"sum"`. They differ only when the grid is
#' smaller than the data that was read in. The noloc columns refer to the extent
#' used when reading.
#'
#' @param x Tibble from [read_geo_admin()].
#' @param grid Target raster: a `stars` object, the path/url of a raster file, or
#'   [make_reference_grid()]. Only the geometry is used.
#' @param years Target years (`NULL` = every year present in `x`). For a gapless
#'   annual series with interpolation, name the years explicitly.
#' @inheritParams align_to_reference
#'
#' @return A tibble with one row per year: `year`, `res_x`, `res_y`, `grid`,
#'   `stars`, `sources` and the noloc columns.
#'
#' @export
align_to_grid <- function(x,
                          grid,
                          years = NULL,
                          methods = resampling_methods(),
                          default_method = "average",
                          year_match = "exact",
                          max_year_diff = Inf,
                          no_data_value = -9999) {
  check_year_match(year_match)
  check_names(names(x), c("collection", "label", "year", "stars"), "x")

  template <- grid_template(grid)
  info <- raster_grid(template)
  years <- sort(unique(years %||% x$year))
  has_noloc <- "noloc_subtracted" %in% names(x)
  cli::cli_inform("Normalising {dplyr::n_distinct(x$collection)} collection{?s} onto {info$grid}.")

  years |>
    purrr::map(\(target_year) {
      cli::cli_inform("Year {target_year}.")
      aligned <- align_layers(
        target_year, template, x, methods, default_method,
        year_match, max_year_diff, no_data_value
      )

      tibble::tibble(year = as.integer(target_year)) |>
        dplyr::bind_cols(info) |>
        dplyr::mutate(
          stars = list(combine_layers(aligned$layers, template)),
          sources = list(aligned$log)
        ) |>
        dplyr::bind_cols(noloc_columns(aligned$rows, has_noloc))
    }) |>
    purrr::list_rbind()
}
