# How each known collection is read, and the entry point that reads several
# collections for the wanted years.

# ---- Collection specs -----------------------------------------------------

#' Describe how a collection is read
#'
#' @param reader A function with the interface
#'   `function(assets, years, bbox, cache_dir, ...)` returning a tibble with at
#'   least `year` and `stars`, for example [read_collection_rasters()] or
#'   [read_statpop_ha()].
#' @param label Short name of the collection in the results.
#' @param resampling GDAL method used when normalising onto a common grid:
#'   `"average"` for concentrations and rates, `"sum"` for counts such as
#'   inhabitants.
#' @param ... Fixed arguments for `reader` (`formats`, `pattern`, `name`, ...).
#'
#' @return A collection spec, a list used by [read_geo_admin()].
#'
#' @examples
#' spec <- collection_spec(read_collection_rasters, label = "no2", name = "no2")
#' spec$label
#'
#' @export
collection_spec <- function(reader, label, resampling = "average", ...) {
  if (!is.function(reader)) cli::cli_abort("{.arg reader} must be a function.")
  if (!rlang::is_string(label)) cli::cli_abort("{.arg label} must be a single string.")
  if (!rlang::is_string(resampling)) cli::cli_abort("{.arg resampling} must be a single string.")

  list(reader = reader, label = label, resampling = resampling, args = list(...))
}

#' Known collections
#'
#' The collections routinely used for air quality analyses in the Canton of
#' Zurich. Extend with, for example:
#'
#' ```r
#' specs <- c(
#'   geo_admin_specs(),
#'   list("ch.x.y" = collection_spec(read_collection_rasters, "y", name = "y"))
#' )
#' ```
#'
#' This is a function rather than a stored list so that the reader functions are
#' resolved when it is called, not when the package is built.
#'
#' @return A named list of collection specs.
#'
#' @examples
#' names(geo_admin_specs())
#'
#' @export
geo_admin_specs <- function() {
  list(
    # Inhabitants per hectare. A count, so "sum": the population total has to
    # survive resampling onto a coarser or finer grid.
    "ch.bfs.statistik-bevoelkerung_haushalte" = collection_spec(
      read_statpop_ha,
      label = "statpop",
      resampling = "sum",
      variables = "BBTOT",
      formats = c("parquet", "csv", "tif"),
      ha_pattern = "_ha_"
    ),
    # Annual mean concentrations modelled by BAFU, one COG per year.
    "ch.bafu.luftreinhaltung-stickstoffdioxid" = collection_spec(
      read_collection_rasters,
      label = "no2",
      name = "no2",
      formats = "tif"
    ),
    "ch.bafu.luftreinhaltung-feinstaub_pm10" = collection_spec(
      read_collection_rasters,
      label = "pm10",
      name = "pm10",
      formats = "tif"
    ),
    "ch.bafu.luftreinhaltung-feinstaub_pm2_5" = collection_spec(
      read_collection_rasters,
      label = "pm2_5",
      name = "pm2_5",
      formats = "tif"
    ),
    # Highest monthly 98th percentile of the half-hourly means.
    "ch.bafu.luftreinhaltung-ozon" = collection_spec(
      read_collection_rasters,
      label = "o3_max_98p_m1",
      name = "o3_max_98p_m1",
      formats = "tif"
    ),
    # Modelled total nitrogen deposition (wet, dry, gaseous), 500 m, for the
    # model years 1990, 2000, 2005, 2010, 2015, 2020 (one COG each). The
    # collection also holds an "Additional files" item (a zip without a year),
    # which `pattern` excludes. An area-related rate, hence "average". For years
    # between the model years use `year_match = "linear"` when aligning.
    "ch.bafu.luftreinhaltung-stickstoffdeposition" = collection_spec(
      read_collection_rasters,
      label = "n_deposition",
      resampling = "average",
      name = "n_deposition",
      formats = "tif",
      pattern = "_(19|20)\\d{2}_2056\\.tif$"
    ),
    # Exceedance of the critical loads for nitrogen in sensitive ecosystems;
    # already an exposure quantity, available from 1990 on.
    "ch.bafu.luftreinhaltung-stickstoff_kritischer_eintrag" = collection_spec(
      read_collection_rasters,
      label = "n_deposition_exceedance",
      resampling = "average",
      name = "n_deposition_exceedance",
      formats = "tif"
    )
  )
}

#' Look up the spec for a collection
#'
#' Unknown collections fall back to the generic raster reader.
#'
#' @param collection Collection id.
#' @param specs Collection specs, see [geo_admin_specs()].
#'
#' @return A collection spec.
#'
#' @keywords internal
get_collection_spec <- function(collection, specs = geo_admin_specs()) {
  if (collection %in% names(specs)) {
    return(specs[[collection]])
  }
  label <- collection_label(collection)
  cli::cli_inform(c("i" = "No spec for {.val {collection}}, using {.fn read_collection_rasters}."))

  collection_spec(read_collection_rasters, label = label, name = label)
}


# ---- Reading one collection -----------------------------------------------

#' Read the raster data of one collection, year by year
#'
#' Resolves the file hierarchy with [resolve_assets()] and reads every year as a
#' `stars` object. Single band rasters get the attribute name `name`, so all
#' years are named alike and can be stacked.
#'
#' @param assets Tibble from [get_geo_admin_assets()].
#' @param years Wanted years (`NULL` = all available).
#' @param bbox Extent, by default the Canton of Zurich.
#' @param pattern,formats,epsg See [resolve_assets()].
#' @param variables Bands or table columns (`NULL` = all).
#' @param name Attribute name for single band rasters (by default derived from
#'   the collection id).
#' @param cache_dir Cache directory.
#' @param ... Further arguments passed to [read_asset_stars()].
#'
#' @return A tibble with one row per year: `item`, `year`, `format`, `href`,
#'   `stars`.
#'
#' @examplesIf interactive()
#' assets <- get_geo_admin_assets("ch.bafu.luftreinhaltung-feinstaub_pm2_5")
#' pm25 <- read_collection_rasters(assets, years = 2020:2024, formats = "tif", name = "pm2_5")
#'
#' @export
read_collection_rasters <- function(assets,
                                    years = NULL,
                                    bbox = bbox_zh_lv95,
                                    pattern = NULL,
                                    formats = c("tif", "parquet", "csv"),
                                    variables = NULL,
                                    name = NULL,
                                    epsg = 2056,
                                    cache_dir = geo_admin_cache_dir(),
                                    ...) {
  resolved <- resolve_assets(assets, pattern, formats, epsg, years)
  name <- name %||% collection_label(dplyr::first(assets[["collection"]], default = "value"))

  rows <- resolved |>
    vctrs::vec_chop() |>
    purrr::map(\(asset) {
      cli::cli_inform("Reading {.val {asset$item}} ({asset$format}).")
      x <- read_asset_stars(
        asset,
        variables = variables, bbox = bbox, crs = epsg, cache_dir = cache_dir, ...
      )
      if (length(x) == 1 && is.null(variables)) names(x) <- name

      tibble::tibble(
        item = asset$item,
        year = asset$year,
        format = asset$format,
        href = asset$href,
        stars = list(x)
      )
    })

  if (length(rows) == 0) {
    return(tibble::tibble(
      item = character(), year = integer(), format = character(),
      href = character(), stars = list()
    ))
  }

  purrr::list_rbind(rows)
}


# ---- Reading several collections ------------------------------------------

#' Read several collections for the wanted years
#'
#' For each collection the assets are queried, the file hierarchy is resolved and
#' only the available years are read; missing ones are reported. Every raster
#' keeps its original resolution — use [align_to_reference()] or
#' [align_to_grid()] to bring them onto a common grid.
#'
#' @param collections Collection ids.
#' @param years Wanted years (`NULL` = all available).
#' @param bbox Extent, by default the Canton of Zurich (LV95 or an `sf` object).
#' @param specs Collection specs, see [geo_admin_specs()].
#' @param on_error `"stop"` aborts, `"warn"` skips collections that fail.
#' @param cache_dir Cache directory.
#'
#' @return A tibble with one row per collection and year: `collection`, `label`,
#'   `year`, `format`, `res_x`, `res_y`, `grid`, `stars`, `item`, `href`, plus
#'   the noloc columns for STATPOP.
#'
#' @examplesIf interactive()
#' data <- read_geo_admin(
#'   c("ch.bfs.statistik-bevoelkerung_haushalte", "ch.bafu.luftreinhaltung-feinstaub_pm2_5"),
#'   years = 2020:2024
#' )
#' dplyr::select(data, label, year, format, grid)
#'
#' @export
read_geo_admin <- function(collections,
                           years = NULL,
                           bbox = bbox_zh_lv95,
                           specs = geo_admin_specs(),
                           on_error = c("stop", "warn"),
                           cache_dir = geo_admin_cache_dir()) {
  on_error <- rlang::arg_match(on_error)
  if (!is.character(collections) || length(collections) == 0) {
    cli::cli_abort("{.arg collections} must be a character vector of at least one collection id.")
  }

  result <- unique(collections) |>
    purrr::map(\(collection) read_one_collection(collection, years, bbox, specs, on_error, cache_dir)) |>
    purrr::list_rbind()

  if (nrow(result) == 0) {
    cli::cli_warn("No data found for the requested collections and years.")

    # A bare list_rbind() of nothing has no columns at all, so a caller that
    # filters or selects on the result hits "column not found" instead of an
    # empty result. Hand back the same shape a successful read would have.
    return(tibble::tibble(
      collection = character(), label = character(), year = integer(),
      format = character(), res_x = double(), res_y = double(),
      grid = character(), stars = list(), item = character(), href = character()
    ))
  }

  result |>
    dplyr::relocate(
      dplyr::any_of(c("collection", "label", "year", "format", "res_x", "res_y", "grid", "stars")),
      .before = 1
    ) |>
    dplyr::arrange(.data$collection, .data$year)
}

#' Read one collection, honouring its spec
#'
#' @inheritParams read_geo_admin
#' @param collection A single collection id.
#'
#' @return A tibble, or `NULL` when nothing could be read.
#'
#' @keywords internal
read_one_collection <- function(collection, years, bbox, specs, on_error, cache_dir) {
  spec <- get_collection_spec(collection, specs)
  cli::cli_inform(c("*" = "Collection {.val {collection}} ({spec$label})"))

  data <- tryCatch(
    {
      assets <- get_geo_admin_assets(collection, bbox = bbox)
      rlang::exec(spec$reader, assets, years = years, bbox = bbox, cache_dir = cache_dir, !!!spec$args)
    },
    error = function(err) {
      msg <- "Reading {.val {collection}} failed."
      if (on_error == "stop") cli::cli_abort(msg, parent = err)
      cli::cli_warn(c(msg, "i" = "Skipping this collection."), parent = err)
      NULL
    }
  )

  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }

  grids <- purrr::map(data$stars, raster_grid) |> purrr::list_rbind()

  data |>
    tibble::add_column(collection = collection, label = spec$label, .before = 1) |>
    dplyr::bind_cols(grids)
}
