# Superseded readers, kept so the analysis repository keeps running while it
# migrates to the new pipeline. Each wrapper reproduces the old return shape on
# top of the new implementation; none of them is maintained beyond that.
#
# Migration:
#   read_bafu_raster_data()     -> read_geo_admin() / read_collection_rasters()
#   read_statpop_raster_data()  -> read_statpop_ha()
#   get_geo_admin_metadata()    -> get_geo_admin_assets()
#   get_bfs_metadata()          -> get_geo_admin_assets()
#   get_bfs_statpop_metadata()  -> get_geo_admin_assets()
#   download_file()             -> download_geo_admin_asset()
#   download_zip()              -> download_geo_admin_asset()
#   download_statpop_data()     -> download_geo_admin_asset()
#   read_statpop_csv()          -> read_asset_table() + table_to_stars()
#   get_assets(), check_for_more() -> internals of get_geo_admin_assets()

#' Pollutant code of a BAFU collection
#'
#' The short codes the analysis repository keys its nested lists by.
#'
#' @param id Collection id or asset url.
#'
#' @return Character vector of pollutant codes, `NA` where unknown.
#'
#' @keywords internal
legacy_pollutant <- function(id) {
  dplyr::case_when(
    stringr::str_detect(id, "feinstaub_pm2_5") ~ "pm25",
    stringr::str_detect(id, "feinstaub_pm10") ~ "pm10",
    stringr::str_detect(id, "ozon") ~ "mp98",
    stringr::str_detect(id, "schwefeldioxid") ~ "so2",
    stringr::str_detect(id, "stickstoffdioxid") ~ "no2",
    stringr::str_detect(id, "stickstoff_kritischer_eintrag") ~ "ndep_exmax",
    .default = NA_character_
  )
}

#' Collection id from an id or a browser url
#'
#' The analysis repository stores the browser url of a collection in
#' `ressources.csv`, not the bare id.
#'
#' @param collection Collection id, or the browser url of the collection.
#'
#' @return The collection id.
#'
#' @keywords internal
legacy_collection_id <- function(collection) {
  id <- stringr::str_extract(collection, "ch\\.[a-z]+\\.[A-Za-z0-9_-]+")

  dplyr::coalesce(id, collection)
}

#' Read BAFU raster data (deprecated)
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Superseded by [read_geo_admin()] and [read_collection_rasters()], which return
#' one tidy row per collection and year instead of a nested list, stream the data
#' rather than downloading it whole, and record their provenance.
#'
#' @param collection Collection id, or the browser url of the collection.
#' @param years_filter Years to read.
#' @param boundary Extent to crop to.
#' @param crs Coordinate reference system.
#'
#' @return A named list, year to a named list of pollutant to `stars`, the shape
#'   the previous implementation returned.
#'
#' @keywords internal
#' @export
read_bafu_raster_data <- function(collection, years_filter, boundary, crs = 2056) {
  lifecycle::deprecate_warn(
    "0.4.0", "read_bafu_raster_data()",
    details = "Use read_geo_admin() or read_collection_rasters(); see vignette('geodata')."
  )

  collection_id <- legacy_collection_id(collection)
  pollutant <- legacy_pollutant(collection_id)

  rasters <- get_geo_admin_assets(collection_id, bbox = boundary) |>
    read_collection_rasters(
      years = years_filter,
      bbox = boundary,
      formats = "tif",
      name = pollutant,
      epsg = crs
    )

  rasters$stars |>
    purrr::map(\(x) stats::setNames(list(x), pollutant)) |>
    stats::setNames(as.character(rasters$year))
}

#' Read BFS STATPOP raster data (deprecated)
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Superseded by [read_statpop_ha()], which reads the STAC assets instead of the
#' retired zip download and subtracts the collector pixels.
#'
#' @param year Year to read.
#' @param destination_path Ignored; downloads go to [geo_admin_cache_dir()].
#' @param boundary Extent to crop to.
#' @param crs Coordinate reference system.
#'
#' @return A `stars` object with the attributes `RELI` and `population`, the
#'   shape the previous implementation returned. Note that unlike before, the
#'   inhabitants that cannot be located are removed from the collector pixels.
#'
#' @keywords internal
#' @export
read_statpop_raster_data <- function(year, destination_path, boundary, crs = 2056) {
  lifecycle::deprecate_warn(
    "0.4.0", "read_statpop_raster_data()",
    details = paste(
      "Use read_statpop_ha(). Note that collector pixels are now subtracted,",
      "so population totals are lower than before by the inhabitants that",
      "cannot be located."
    )
  )
  if (!missing(destination_path)) {
    cli::cli_inform(c("i" = "{.arg destination_path} is ignored; caching to {.file {geo_admin_cache_dir()}}."))
  }

  statpop <- get_geo_admin_assets("ch.bfs.statistik-bevoelkerung_haushalte", bbox = boundary) |>
    read_statpop_ha(
      years = as.integer(year),
      variables = c("RELI", "BBTOT"),
      bbox = boundary,
      crs = crs
    )

  if (nrow(statpop) == 0) {
    cli::cli_abort("No STATPOP data available for {year}.")
  }

  stats::setNames(statpop$stars[[1]], c("RELI", "population"))
}

#' Get asset urls from the geo.admin STAC API (deprecated)
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Superseded by [get_geo_admin_assets()], which returns a table with year,
#' format and checksum rather than bare urls, and uses the current v1 API.
#'
#' @param collection Collection id.
#' @param filter Substring the asset name must contain.
#' @param stac_version Ignored; the v1 API is always used.
#' @param max_check_more Ignored; all pages are followed.
#'
#' @return Character vector of asset urls.
#'
#' @keywords internal
#' @export
get_geo_admin_metadata <- function(collection, filter = ".tif", stac_version = "0.9", max_check_more = 3) {
  lifecycle::deprecate_warn(
    "0.4.0", "get_geo_admin_metadata()", "get_geo_admin_assets()",
    details = "The v0.9 API is deprecated; the v1 API is used instead."
  )

  assets <- get_geo_admin_assets(collection)

  assets$href[stringr::str_detect(assets$asset, stringr::fixed(filter))]
}

#' Get the STATPOP download url from the BFS API (deprecated)
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Superseded by [get_geo_admin_assets()]: STATPOP is published as a STAC
#' collection, and the BFS asset API is no longer the documented source.
#'
#' @param year Year wanted.
#'
#' @return The download url of that year's hectare grid.
#'
#' @keywords internal
#' @export
get_bfs_statpop_metadata <- function(year) {
  lifecycle::deprecate_warn("0.4.0", "get_bfs_statpop_metadata()", "get_geo_admin_assets()")

  get_geo_admin_assets("ch.bfs.statistik-bevoelkerung_haushalte") |>
    resolve_assets(pattern = "_ha_", formats = c("parquet", "csv"), years = as.integer(year)) |>
    dplyr::pull("href")
}

#' Download an asset (deprecated)
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Superseded by [download_geo_admin_asset()], which validates checksums, reuses
#' a persistent cache and never leaves a partial file behind. The previous
#' implementation shelled out to `curl` without checking the exit status.
#'
#' @param download_url Source url.
#' @param destination_path Target directory.
#' @param file_ext File extension of the temporary file.
#'
#' @return Path of the downloaded file.
#'
#' @keywords internal
#' @export
download_file <- function(download_url, destination_path, file_ext) {
  lifecycle::deprecate_warn("0.4.0", "download_file()", "download_geo_admin_asset()")

  dir.create(destination_path, recursive = TRUE, showWarnings = FALSE)
  temp <- tempfile(tmpdir = destination_path, fileext = file_ext)
  fetch_to_file(download_url, temp)

  temp
}

#' Download and unpack a zip archive (deprecated)
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Superseded by [download_geo_admin_asset()], which extracts zip members itself.
#'
#' @param download_url Source url.
#' @param destination_path Target directory.
#' @param file_filter Regular expression selecting members to extract.
#' @param file_ext File extension of the temporary file.
#'
#' @return The paths of the extracted files, invisibly.
#'
#' @keywords internal
#' @export
download_zip <- function(download_url, destination_path, file_filter = NULL, file_ext = ".zip") {
  lifecycle::deprecate_warn("0.4.0", "download_zip()", "download_geo_admin_asset()")

  temp <- tempfile(fileext = file_ext)
  on.exit(unlink(temp), add = TRUE)
  fetch_to_file(download_url, temp)

  members <- utils::unzip(temp, list = TRUE)$Name
  wanted <- if (is.null(file_filter)) members else members[stringr::str_detect(members, file_filter)]
  utils::unzip(temp, files = wanted, exdir = destination_path, junkpaths = TRUE)

  invisible(file.path(destination_path, basename(wanted)))
}
