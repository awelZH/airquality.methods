# STAC API of data.geo.admin.ch: query items, parse assets, resolve the file
# hierarchy down to exactly one asset per item.

geo_admin_user_agent <- "airquality.methods (https://github.com/awelZH/airquality.methods)"


# ---- Validation -----------------------------------------------------------

#' Ensure exactly one asset (one row) was passed
#'
#' @param asset A one-row data frame from [get_geo_admin_assets()].
#' @param call Environment used for the error call.
#'
#' @return `asset`, invisibly validated.
#'
#' @keywords internal
check_single_asset <- function(asset, call = rlang::caller_env()) {
  if (!is.data.frame(asset) || nrow(asset) != 1) {
    cli::cli_abort(
      "{.arg asset} must be exactly one row from {.fn get_geo_admin_assets}.",
      call = call
    )
  }
  check_names(names(asset), c("asset", "href", "format"), "asset", call = call)

  asset
}

#' Compare an asset's declared crs with the expected one
#'
#' Assets that do not declare `proj:epsg` pass through unchecked.
#'
#' @param asset A one-row data frame from [get_geo_admin_assets()].
#' @param crs Expected coordinate reference system.
#' @param call Environment used for the error call.
#'
#' @return `TRUE`, invisibly.
#'
#' @keywords internal
check_asset_crs <- function(asset, crs, call = rlang::caller_env()) {
  epsg <- asset[["proj:epsg"]]

  if (!is.null(epsg) && !is.na(epsg) && sf::st_crs(as.integer(epsg)) != sf::st_crs(crs)) {
    cli::cli_abort(
      c(
        "x" = "Asset {.val {asset$asset}} is in EPSG:{epsg}, expected {sf::st_crs(crs)$input}.",
        "i" = "Filter the assets first, for example {.code dplyr::filter(`proj:epsg` == 2056)}."
      ),
      call = call
    )
  }

  invisible(TRUE)
}


# ---- Request --------------------------------------------------------------

#' Build the request for the items of a STAC collection
#'
#' @param collection Collection id, for example
#'   `"ch.bfs.statistik-bevoelkerung_haushalte"`.
#' @param bbox Optional extent, by default in LV95 (see [as_bbox()]). The STAC
#'   API expects WGS84; the conversion happens internally.
#' @param bbox_crs Coordinate reference system of a numeric `bbox`.
#' @param datetime Optional RFC-3339 instant or interval, for example
#'   `"2020-01-01T00:00:00Z/.."`.
#' @param limit Number of items per page.
#' @param stac_version API version. `"v1"` is current, `"v0.9"` is deprecated.
#' @param base_url Base url of the STAC API.
#'
#' @return An [httr2::request()] object.
#'
#' @keywords internal
geo_admin_items_request <- function(collection,
                                    bbox = NULL,
                                    bbox_crs = 2056,
                                    datetime = NULL,
                                    limit = 100,
                                    stac_version = c("v1", "v0.9"),
                                    base_url = "https://data.geo.admin.ch/api/stac") {
  stac_version <- rlang::arg_match(stac_version)
  if (!rlang::is_string(collection)) {
    cli::cli_abort("{.arg collection} must be a single string.")
  }

  bbox_query <- NULL
  if (!is.null(bbox)) {
    bb <- transform_bbox(as_bbox(bbox, bbox_crs), 4326)
    # round outwards so the box cannot shrink below the requested extent
    bbox_query <- paste(c(floor(bb[1:2] * 1e6), ceiling(bb[3:4] * 1e6)) / 1e6, collapse = ",")
  }

  httr2::request(base_url) |>
    httr2::req_url_path_append(stac_version, "collections", collection, "items") |>
    httr2::req_url_query(bbox = bbox_query, datetime = datetime, limit = limit) |>
    httr2::req_user_agent(geo_admin_user_agent) |>
    httr2::req_retry(max_tries = 5)
}


# ---- Pagination -----------------------------------------------------------

#' Follow the STAC link with `rel = "next"`
#'
#' Interface expected by [httr2::req_perform_iterative()].
#'
#' @param resp The last response.
#' @param req The original request.
#'
#' @return A request for the next page, or `NULL` when the last page is reached.
#'
#' @keywords internal
next_stac_page <- function(resp, req) {
  next_href <- resp |>
    httr2::resp_body_json() |>
    purrr::pluck("links", .default = list()) |>
    purrr::keep(\(link) identical(link[["rel"]], "next")) |>
    purrr::map_chr("href")

  if (length(next_href) == 0) {
    return(NULL)
  }

  httr2::req_url(req, next_href[[1]])
}


# ---- Parsing --------------------------------------------------------------

#' Turn the assets of one STAC item into a table
#'
#' All scalar asset fields are kept, so collection-specific metadata such as
#' `proj:epsg` or `file:checksum` survives into the result.
#'
#' @param item One parsed STAC item.
#'
#' @return A tibble with one row per asset, or `NULL` if the item has none.
#'
#' @keywords internal
parse_item_assets <- function(item) {
  assets <- item[["assets"]]
  if (length(assets) == 0) {
    return(NULL)
  }
  props <- item[["properties"]]

  assets |>
    purrr::map(\(asset) {
      asset |>
        purrr::keep(\(field) is.atomic(field) && length(field) == 1) |>
        tibble::as_tibble()
    }) |>
    purrr::list_rbind(names_to = "asset") |>
    tibble::add_column(
      collection     = item[["collection"]] %||% NA_character_,
      item           = item[["id"]],
      datetime       = props[["datetime"]] %||% NA_character_,
      start_datetime = props[["start_datetime"]] %||% NA_character_,
      end_datetime   = props[["end_datetime"]] %||% NA_character_,
      .before = 1
    )
}

#' Turn one page of the API response into an asset table
#'
#' @param resp One [httr2::response()].
#'
#' @return A tibble with one row per asset.
#'
#' @keywords internal
parse_stac_page <- function(resp) {
  resp |>
    httr2::resp_body_json() |>
    purrr::pluck("features", .default = list()) |>
    purrr::map(parse_item_assets) |>
    purrr::list_rbind()
}

#' Derive the file format from an asset url
#'
#' `".csv.zip"` becomes `"csv"`, `".tiff"` becomes `"tif"`.
#'
#' @param href Asset url.
#'
#' @return Character vector of formats.
#'
#' @keywords internal
asset_format <- function(href) {
  file <- href |>
    stringr::str_remove("[?#].*$") |>
    basename() |>
    tolower() |>
    stringr::str_remove("\\.(zip|gz)$")
  ext <- stringr::str_extract(file, "(?<=\\.)[a-z0-9]+$")
  ext[ext %in% c("tiff", "geotiff")] <- "tif"

  ext
}

#' Derive the compression from an asset url
#'
#' @param href Asset url.
#'
#' @return `"zip"`, `"gz"` or `NA`.
#'
#' @keywords internal
asset_compression <- function(href) {
  href |>
    stringr::str_remove("[?#].*$") |>
    tolower() |>
    stringr::str_extract("(?<=\\.)(zip|gz)$")
}

#' Determine the reference year of an item
#'
#' An unambiguous year in the item id wins (for example `"..._2020"`), otherwise
#' the year from `datetime`, `end_datetime` or `start_datetime`. Four-digit
#' numbers beyond next year are not years (EPSG 2056, for instance), and an id
#' carrying several candidate years is not trusted either.
#'
#' Items that end up without a year are silently ignored by every year filter
#' downstream, so this warns instead.
#'
#' @param item Item ids.
#' @param datetime,start_datetime,end_datetime Item timestamps.
#'
#' @return Integer vector of years, `NA` where none could be determined.
#'
#' @keywords internal
item_year <- function(item, datetime, start_datetime, end_datetime) {
  max_year <- as.integer(format(Sys.Date(), "%Y")) + 1L

  from_id <- purrr::map_int(item, \(id) {
    candidates <- as.integer(stringr::str_extract_all(id, "(?<!\\d)(19|20)\\d{2}(?!\\d)")[[1]])
    candidates <- unique(candidates[candidates <= max_year])
    if (length(candidates) == 1) candidates else NA_integer_
  })
  from_datetime <- dplyr::coalesce(datetime, end_datetime, start_datetime) |>
    stringr::str_sub(1, 4) |>
    as.integer()

  years <- dplyr::coalesce(from_id, from_datetime)

  if (anyNA(years)) {
    undated <- unique(item[is.na(years)])
    cli::cli_warn(c(
      "!" = "{length(undated)} item{?s} without a year; {?it/they} will be ignored by year filters.",
      "i" = "Affected: {.val {utils::head(undated, 6)}}"
    ))
  }

  years
}


# ---- Query assets ---------------------------------------------------------

#' Query all assets of a data.geo.admin.ch collection
#'
#' Walks every page of the STAC items endpoint and returns one row per asset,
#' enriched with the derived `year`, `format` and `compression`.
#'
#' @param collection Collection id.
#' @param ... Further arguments passed to [geo_admin_items_request()]
#'   (`bbox` in LV95, `datetime`, `limit`, `stac_version`, ...).
#' @param max_pages Maximum number of pages to request.
#'
#' @return A tibble with one row per asset.
#'
#' @examplesIf interactive()
#' assets <- get_geo_admin_assets("ch.bafu.luftreinhaltung-feinstaub_pm2_5")
#' dplyr::count(assets, year, format)
#'
#' @export
get_geo_admin_assets <- function(collection, ..., max_pages = Inf) {
  req <- geo_admin_items_request(collection, ...)

  resps <- httr2::req_perform_iterative(req, next_req = next_stac_page, max_reqs = max_pages)

  last_resp <- resps[[length(resps)]]
  if (length(resps) >= max_pages && !is.null(next_stac_page(last_resp, req))) {
    cli::cli_warn(
      "{.arg max_pages} ({max_pages}) reached but more pages exist; the result is incomplete."
    )
  }

  assets <- tibble::as_tibble(httr2::resps_data(resps, parse_stac_page))
  if (nrow(assets) == 0) {
    return(assets)
  }

  assets |>
    dplyr::mutate(
      year = item_year(.data$item, .data$datetime, .data$start_datetime, .data$end_datetime),
      .after = "item"
    ) |>
    dplyr::mutate(
      format      = asset_format(.data$href),
      compression = asset_compression(.data$href),
      .after = "asset"
    )
}


# ---- Asset selection ------------------------------------------------------

#' Keep the preferred file format per group
#'
#' Second stage of the file hierarchy: filter by content first (for example
#' `"_ha_2056"`), then keep the best available format per item.
#'
#' @param assets Tibble from [get_geo_admin_assets()].
#' @param formats Formats in decreasing preference. Other formats are dropped.
#' @param by Column(s) within which the preference applies.
#'
#' @return The filtered assets. Groups without a matching format raise a warning.
#'
#' @keywords internal
select_preferred_format <- function(assets, formats = c("parquet", "csv", "tif"), by = "item") {
  check_names(names(assets), c("format", by), "assets")

  selected <- assets |>
    dplyr::filter(.data$format %in% formats) |>
    dplyr::filter(
      match(.data$format, formats) == min(match(.data$format, formats)),
      .by = dplyr::all_of(by)
    )

  dropped <- assets |>
    dplyr::select(dplyr::all_of(by)) |>
    dplyr::distinct() |>
    dplyr::anti_join(selected, by = by)

  if (nrow(dropped) > 0) {
    cli::cli_warn(c(
      "!" = "Dropped {nrow(dropped)} group{?s} without an asset in format{?s} {.val {formats}}.",
      "i" = "Affected: {.val {dropped[[by[[1]]]]}}"
    ))
  }

  selected
}

#' Resolve the file hierarchy down to one asset per item
#'
#' Applied in order:
#' 1. content: `pattern` as a regular expression on the asset name (`"_ha_"`)
#' 2. crs: `proj:epsg == epsg` (assets without the field are kept)
#' 3. format: the first available format from `formats`, per item
#' 4. uniqueness: prefer names carrying the EPSG code, otherwise abort with the
#'    list of candidates
#' 5. years: keep `years` only; unavailable years are reported
#'
#' @param assets Tibble from [get_geo_admin_assets()].
#' @param pattern Regular expression matched against the asset name.
#' @param formats Formats in decreasing preference.
#' @param epsg Expected EPSG code, or `NULL` to skip the crs stage.
#' @param years Wanted years (`NULL` = all available).
#'
#' @return A tibble with one row per item, sorted by year.
#'
#' @export
resolve_assets <- function(assets,
                           pattern = NULL,
                           formats = c("tif", "parquet", "csv"),
                           epsg = 2056,
                           years = NULL) {
  check_names(names(assets), c("item", "year", "asset", "format"), "assets")
  label <- dplyr::first(assets[["collection"]], default = "assets")

  candidates <- assets
  if (!is.null(pattern)) {
    candidates <- dplyr::filter(candidates, stringr::str_detect(.data$asset, pattern))
  }
  if (!is.null(epsg) && "proj:epsg" %in% names(candidates)) {
    candidates <- dplyr::filter(candidates, is.na(.data$`proj:epsg`) | .data$`proj:epsg` == epsg)
  }
  candidates <- dplyr::filter(candidates, .data$format %in% formats)

  if (nrow(candidates) == 0) {
    cli::cli_abort(c(
      "x" = "{label}: no asset matches pattern {.val {pattern %||% '(none)'}}, EPSG {epsg} and format{?s} {.val {formats}}.",
      "i" = "Assets present (excerpt): {.val {utils::head(unique(assets$asset), 6)}}"
    ))
  }

  resolved <- select_preferred_format(candidates, formats)

  if (!is.null(epsg)) {
    epsg_regex <- paste0("(?<!\\d)", epsg, "(?!\\d)")
    resolved <- resolved |>
      dplyr::mutate(.epsg_in_name = stringr::str_detect(.data$asset, epsg_regex)) |>
      dplyr::filter(.data$.epsg_in_name | !any(.data$.epsg_in_name), .by = "item") |>
      dplyr::select(-".epsg_in_name")
  }

  ambiguous <- dplyr::filter(resolved, dplyr::n() > 1, .by = "item")
  if (nrow(ambiguous) > 0) {
    cli::cli_abort(c(
      "x" = "{label}: the file hierarchy is ambiguous, several assets remain per item.",
      "i" = "Candidates: {.val {unique(ambiguous$asset)}}",
      "i" = "Tighten {.arg pattern}, for example in the collection spec."
    ))
  }

  if (!is.null(years)) {
    available <- sort(unique(stats::na.omit(resolved$year)))
    missing_years <- setdiff(years, available)
    if (length(missing_years) > 0) {
      cli::cli_inform(c(
        "i" = "{label}: not available: {paste(missing_years, collapse = ', ')}",
        " " = "available: {paste(available, collapse = ', ')}"
      ))
    }
    resolved <- dplyr::filter(resolved, .data$year %in% years)
  }

  dplyr::arrange(resolved, .data$year)
}

#' Short label for a collection id
#'
#' `"ch.bafu.luftreinhaltung-x_y"` becomes `"luftreinhaltung_x_y"`.
#'
#' @param collection Collection id.
#'
#' @return A character vector of labels.
#'
#' @keywords internal
collection_label <- function(collection) {
  collection |>
    stringr::str_remove("^ch\\.[^.]+\\.") |>
    stringr::str_replace_all("[^A-Za-z0-9]+", "_")
}
