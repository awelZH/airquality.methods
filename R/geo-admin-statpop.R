# STATPOP (ch.bfs.statistik-bevoelkerung_haushalte): read the hectare grid and
# remove the inhabitants that cannot be located from their collector pixels.

# ---- Collector pixel correction -------------------------------------------

#' Subtract the inhabitants that cannot be located
#'
#' Every municipality has one collector pixel holding the inhabitants that cannot
#' be assigned to a location. This subtracts them from that raster cell and logs
#' every pixel, so the correction stays auditable instead of silently changing
#' the totals.
#'
#' Status per collector pixel:
#' * `"subtracted"`: fully removed
#' * `"exceeds_cell"`: more to subtract than the cell holds; the cell went to 0
#' * `"cell_missing"`: the cell is empty (`NA`) in the raster, nothing subtracted
#' * `"outside_extent"`: the pixel lies outside the bbox, nothing subtracted
#'
#' @param x A two-dimensional `stars` object holding `variable`.
#' @param noloc Table of collector pixels with `coords` and `variable`.
#' @param variable Attribute to correct (inhabitants).
#' @param coords Coordinate columns in `noloc`.
#' @param anchor What the collector pixel coordinates refer to, see
#'   [table_to_stars()].
#'
#' @return A list with `stars` (the corrected raster) and `noloc` (one log row
#'   per collector pixel).
#'
#' @export
subtract_noloc <- function(x,
                           noloc,
                           variable = "BBTOT",
                           coords = c("E_KOORD", "N_KOORD"),
                           anchor = c("lowerleft", "center")) {
  anchor <- rlang::arg_match(anchor)
  check_names(names(noloc), c(coords, variable), "noloc")
  check_names(names(x), variable, "x")

  x <- stars::st_as_stars(x)
  xy <- attr(stars::st_dimensions(x), "raster")$dimensions
  if (length(dim(x)) != 2) {
    cli::cli_abort(
      "{.arg x} must be two-dimensional (only {.val {xy}}), but has {.val {names(dim(x))}}."
    )
  }
  dim_x <- stars::st_dimensions(x)[[xy[[1]]]]
  dim_y <- stars::st_dimensions(x)[[xy[[2]]]]

  # match on the cell centre, so rounding at cell borders cannot matter
  half <- if (anchor == "lowerleft") abs(c(dim_x$delta, dim_y$delta)) / 2 else c(0, 0)

  pixels <- noloc |>
    dplyr::summarise(
      n_records = dplyr::n(),
      noloc = sum(.data[[variable]], na.rm = TRUE),
      .by = dplyr::all_of(coords)
    ) |>
    dplyr::mutate(
      .col = floor((.data[[coords[[1]]]] + half[[1]] - dim_x$offset) / dim_x$delta) + 2 - dim_x$from,
      .row = floor((.data[[coords[[2]]]] + half[[2]] - dim_y$offset) / dim_y$delta) + 2 - dim_y$from,
      .inside = .data$.col >= 1 & .data$.col <= dim(x)[[xy[[1]]]] &
        .data$.row >= 1 & .data$.row <= dim(x)[[xy[[2]]]]
    )

  values <- x[[variable]]
  idx <- cbind(pixels$.col, pixels$.row)
  cell_before <- rep(NA_real_, nrow(pixels))
  cell_before[pixels$.inside] <- values[idx[pixels$.inside, , drop = FALSE]]

  report <- pixels |>
    dplyr::mutate(
      cell_before = cell_before,
      status = dplyr::case_when(
        !.data$.inside ~ "outside_extent",
        is.na(.data$cell_before) ~ "cell_missing",
        .data$noloc > .data$cell_before ~ "exceeds_cell",
        .default = "subtracted"
      ),
      subtracted = dplyr::if_else(
        .data$status %in% c("subtracted", "exceeds_cell"),
        pmin(.data$noloc, .data$cell_before),
        0
      ),
      cell_after = .data$cell_before - .data$subtracted
    )

  to_update <- report$subtracted > 0
  new_values <- report$cell_after[to_update]
  if (is.integer(values)) new_values <- as.integer(round(new_values))
  values[idx[to_update, , drop = FALSE]] <- new_values
  x[[variable]] <- values

  problems <- report |>
    dplyr::filter(.data$status %in% c("exceeds_cell", "cell_missing")) |>
    dplyr::count(.data$status)
  if (nrow(problems) > 0) {
    cli::cli_warn(c(
      "!" = "{sum(problems$n)} collector pixel{?s} could not be subtracted in full.",
      rlang::set_names(paste0(problems$status, ": ", problems$n), rep("*", nrow(problems))),
      "i" = "See the {.field noloc} column of the result for details."
    ))
  }

  list(
    stars = x,
    noloc = dplyr::select(
      report,
      dplyr::all_of(coords),
      "n_records", "noloc", "cell_before", "subtracted", "cell_after", "status"
    )
  )
}


# ---- Reading STATPOP ------------------------------------------------------

#' Column names of the census years 1990 and 2000
#'
#' The census hectare tables of `ch.bfs.statistik-bevoelkerung_haushalte` name
#' their columns by census: `KX`/`KY` (1990) and `X`/`Y` (2000) for the lower
#' left corner, `P9BTOT`/`P00BTOT` for the inhabitants. Mapped onto the STATPOP
#' names, they are read by the same code. Extend the list for further census
#' variables, for example the men:
#' `c(statpop_census_aliases(), list(BBMTOT = c("P9BMTOT", "P00BMTOT")))`.
#' Check the BFS documentation that a census variable means the same as its
#' STATPOP counterpart.
#'
#' @return A named list: STATPOP column name to the census column names.
#'
#' @examples
#' statpop_census_aliases()
#'
#' @export
statpop_census_aliases <- function() {
  list(
    BBTOT = c("P9BTOT", "P00BTOT"),
    E_KOORD = c("KX", "X"),
    N_KOORD = c("KY", "Y")
  )
}

#' Empty the cells listed with zero in every attribute
#'
#' The census tables list hectares without inhabitants (they carry other
#' variables); STATPOP lists inhabited cells only. Setting the zeros to `NA`
#' makes both mean the same: an empty cell is an uninhabited one.
#'
#' @param x A `stars` object.
#'
#' @return `x` with `NA` where all attributes are 0.
#'
#' @keywords internal
empty_zero_cells <- function(x) {
  zero <- purrr::map(names(x), \(n) !is.na(x[[n]]) & x[[n]] == 0) |> purrr::reduce(`&`)
  for (n in names(x)) x[[n]][zero] <- NA
  x
}

#' Whether an asset is declared in the given crs
#'
#' @param asset A one-row data frame from [get_geo_admin_assets()].
#' @param crs Coordinate reference system.
#'
#' @return `TRUE` if `proj:epsg` is missing or names `crs`.
#'
#' @keywords internal
in_crs <- function(asset, crs) {
  epsg <- asset[["proj:epsg"]]
  is.null(epsg) || is.na(epsg) || sf::st_crs(as.integer(epsg)) == sf::st_crs(crs)
}

#' The collector pixel log of an item without collector pixels
#'
#' @param coords Coordinate columns.
#'
#' @return A tibble with the columns of the `noloc` log of [subtract_noloc()]
#'   and no rows.
#'
#' @keywords internal
empty_noloc_log <- function(coords = c("E_KOORD", "N_KOORD")) {
  log <- tibble::tibble(
    x = numeric(), y = numeric(),
    n_records = integer(), noloc = numeric(), cell_before = numeric(),
    subtracted = numeric(), cell_after = numeric(), status = character()
  )
  names(log)[1:2] <- coords
  log
}

#' Read and correct one STATPOP item
#'
#' @inheritParams read_statpop_ha
#' @param ha_asset One resolved hectare grid asset.
#' @param noloc_assets All candidate collector pixel assets.
#'
#' @return A one-row tibble, see [read_statpop_ha()].
#'
#' @keywords internal
read_statpop_item <- function(ha_asset, noloc_assets, variables, bbox, noloc_variable,
                              year_col, coords, anchor, correct_noloc, crs, cache_dir,
                              aliases = NULL, drop_zero = TRUE, ...) {
  inform_reading(ha_asset, cache_dir)
  raster <- read_asset_stars(
    ha_asset,
    variables = variables, bbox = bbox, coords = coords, anchor = anchor,
    crs = crs, cache_dir = cache_dir, aliases = aliases, ...
  )
  if (drop_zero) raster <- empty_zero_cells(raster)

  result <- tibble::tibble(
    item   = ha_asset$item,
    year   = ha_asset$year,
    format = ha_asset$format,
    href   = ha_asset$href
  )

  without_correction <- function(message = NULL) {
    if (!is.null(message)) cli::cli_warn(message)
    dplyr::mutate(
      result,
      stars = list(raster), noloc_subtracted = NA_real_, noloc_unmatched = NA_real_,
      noloc = list(NULL)
    )
  }

  if (!correct_noloc) {
    return(without_correction())
  }

  noloc_asset <- dplyr::filter(noloc_assets, .data$item == ha_asset$item)
  if (nrow(noloc_asset) == 0 && !in_crs(ha_asset, crs)) {
    # the census years (in the fallback crs) come without collector pixels: nothing to subtract
    cli::cli_inform(c("i" = "Item {.val {ha_asset$item}}: census year, no collector pixels to subtract."))
    return(dplyr::mutate(
      result,
      stars = list(raster), noloc_subtracted = 0, noloc_unmatched = 0,
      noloc = list(empty_noloc_log(coords))
    ))
  }
  if (nrow(noloc_asset) != 1) {
    return(without_correction(c(
      "!" = "Item {.val {ha_asset$item}}: found {nrow(noloc_asset)} collector pixel file{?s} (expected 1), no correction applied.",
      "i" = "The population totals therefore still include inhabitants that cannot be located.",
      "i" = "Check {.arg noloc_pattern} and {.arg noloc_formats} against the assets of this item."
    )))
  }

  noloc <- download_geo_admin_asset(noloc_asset, cache_dir) |>
    read_asset_table(noloc_asset$format)
  check_names(names(noloc), year_col, noloc_asset$asset)

  noloc_year <- unique(noloc[[year_col]])
  if (length(noloc_year) != 1) {
    cli::cli_abort(
      "{.val {noloc_asset$asset}} holds {length(noloc_year)} values in {.field {year_col}}, expected 1."
    )
  }
  if (!is.na(ha_asset$year) && noloc_year != ha_asset$year) {
    cli::cli_warn(c(
      "!" = "Item {.val {ha_asset$item}}: metadata year {ha_asset$year}, but {year_col} = {noloc_year}.",
      "i" = "{year_col} is used; the year selection is based on the metadata."
    ))
  }

  corrected <- subtract_noloc(
    raster, noloc,
    variable = noloc_variable, coords = coords, anchor = anchor
  )

  result |>
    dplyr::mutate(
      year             = as.integer(.env$noloc_year),
      stars            = list(corrected$stars),
      noloc_subtracted = sum(corrected$noloc$subtracted),
      noloc_unmatched  = sum(corrected$noloc$noloc) - sum(corrected$noloc$subtracted),
      noloc            = list(corrected$noloc)
    )
}

#' Read the STATPOP hectare grid, one stars object per year
#'
#' The file hierarchy is resolved with [resolve_assets()] (pattern, EPSG, format
#' preference, years). Inhabitants that cannot be located are subtracted from
#' their collector pixels and carried alongside the raster:
#' * `noloc_subtracted`: inhabitants removed within the extent
#' * `noloc_unmatched`: inhabitants not removed, because the collector pixel lies
#'   outside the bbox or its cell is empty
#' * `noloc`: the per-pixel log
#'
#' The invariant `sum(noloc file) = noloc_subtracted + noloc_unmatched` holds.
#'
#' The collection also holds the federal censuses 1990 and 2000, as csv in LV03
#' only and with their own column names (`KX`/`KY` or `X`/`Y`, `P9BTOT`,
#' `P00BTOT`). They are read in the same call: the LV03 asset is taken where no
#' LV95 one exists (`epsg_fallback`), its columns are renamed by `aliases`, and
#' it is shifted onto the LV95 hectare grid ([shift_lv03_to_lv95()]), so all
#' years share one grid. The censuses have no collector pixels:
#' `noloc_subtracted` and `noloc_unmatched` are 0 and the log is empty. Census
#' variables other than the headcount need an alias of their own.
#'
#' @param assets Tibble from [get_geo_admin_assets()].
#' @param years Wanted years (`NULL` = all available).
#' @param variables Variables to read. Must contain `noloc_variable` when
#'   correcting. Only `noloc_variable` is corrected.
#' @param formats Format preference for the hectare grid.
#' @param bbox Extent, by default the Canton of Zurich.
#' @param ha_pattern Regular expression for the hectare grid assets.
#' @param noloc_pattern Regular expression for the collector pixel assets.
#' @param noloc_formats Format preference for the collector pixel assets.
#' @param noloc_variable The inhabitant variable, in both datasets.
#' @param year_col Year column inside the collector pixel file.
#' @param coords,anchor Coordinate columns and their reference point.
#' @param correct_noloc Apply the correction?
#' @param crs Expected coordinate reference system.
#' @param cache_dir Cache directory.
#' @param epsg_fallback EPSG code of the assets read for years without one in
#'   `crs` (the censuses in LV03), or `NULL` to read the years in `crs` only.
#' @param aliases Other names of the columns, by default those of the censuses;
#'   see [read_asset_table()].
#' @param drop_zero Empty the cells listed with 0 in all `variables`? The
#'   censuses list uninhabited hectares, STATPOP does not; cells emptied by the
#'   collector pixel correction stay 0.
#' @param ... Further arguments passed to [read_asset_stars()], `cellsize` for
#'   instance.
#'
#' @return A tibble with one row per year: `item`, `year`, `format`, `href`,
#'   `stars`, `noloc_subtracted`, `noloc_unmatched`, `noloc`.
#'
#' @examplesIf interactive()
#' assets <- get_geo_admin_assets("ch.bfs.statistik-bevoelkerung_haushalte")
#' statpop <- read_statpop_ha(assets, years = c(1990, 2000, 2015:2024))
#' dplyr::select(statpop, year, format, noloc_subtracted, noloc_unmatched)
#'
#' @export
read_statpop_ha <- function(assets,
                            years = NULL,
                            variables = "BBTOT",
                            formats = c("parquet", "csv", "tif"),
                            bbox = bbox_zh_lv95,
                            ha_pattern = "_ha_",
                            noloc_pattern = "_noloc_",
                            noloc_formats = c("csv", "parquet"),
                            noloc_variable = "BBTOT",
                            year_col = "ERHJAHR",
                            coords = c("E_KOORD", "N_KOORD"),
                            anchor = c("lowerleft", "center"),
                            correct_noloc = TRUE,
                            crs = 2056,
                            cache_dir = geo_admin_cache_dir(),
                            epsg_fallback = 21781,
                            aliases = statpop_census_aliases(),
                            drop_zero = TRUE,
                            ...) {
  anchor <- rlang::arg_match(anchor)
  if (correct_noloc && !is.null(variables) && !noloc_variable %in% variables) {
    cli::cli_abort(
      "{.arg variables} must contain {.val {noloc_variable}} when {.arg correct_noloc} is TRUE."
    )
  }
  if (correct_noloc && length(variables) > 1) {
    cli::cli_inform(c("i" = "Only {.val {noloc_variable}} is corrected for collector pixels."))
  }

  ha <- resolve_assets(
    assets,
    pattern = ha_pattern, formats = formats, epsg = crs, years = years, epsg_fallback = epsg_fallback
  )

  noloc_assets <- assets |>
    dplyr::filter(
      stringr::str_detect(.data$asset, noloc_pattern),
      .data$format %in% noloc_formats
    ) |>
    dplyr::slice_min(match(.data$format, noloc_formats), n = 1, with_ties = FALSE, by = "item")

  if (correct_noloc && nrow(noloc_assets) == 0) {
    cli::cli_warn(c(
      "!" = "No asset matches {.arg noloc_pattern} {.val {noloc_pattern}} in format{?s} {.val {noloc_formats}}.",
      "i" = "No collector pixel correction will be applied; population totals stay uncorrected."
    ))
  }

  ha |>
    vctrs::vec_chop() |> # one row per year
    purrr::map(\(ha_asset) read_statpop_item(
      ha_asset, noloc_assets, variables, bbox, noloc_variable,
      year_col, coords, anchor, correct_noloc, crs, cache_dir,
      aliases = aliases, drop_zero = drop_zero, ...
    )) |>
    purrr::list_rbind() |>
    dplyr::arrange(.data$year)
}
