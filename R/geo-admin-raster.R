# Bounding boxes, table-to-raster conversion and reading single assets as stars.

#' Default extent: Canton of Zurich in LV95
#'
#' Rounded outwards to 5 km. For the exact cantonal outline pass any `sf` object
#' as `bbox` (for example from swissBOUNDARIES3D); its extent is used.
#'
#' @format Named numeric vector of length 4 (`xmin`, `ymin`, `xmax`, `ymax`).
#'
#' @export
bbox_zh_lv95 <- c(xmin = 2665000, ymin = 1220000, xmax = 2720000, ymax = 1285000)

#' Default extent: Switzerland in LV95
#'
#' Rounded to 10 km with a margin.
#'
#' @format Named numeric vector of length 4 (`xmin`, `ymin`, `xmax`, `ymax`).
#'
#' @export
bbox_ch_lv95 <- c(xmin = 2480000, ymin = 1070000, xmax = 2840000, ymax = 1300000)


# ---- Bounding box ---------------------------------------------------------

#' Normalise a bounding box
#'
#' Accepts the several shapes an extent arrives in and always returns an
#' [sf::st_bbox()] object in `crs`.
#'
#' @param bbox `NULL`, a numeric vector `c(xmin, ymin, xmax, ymax)` in `crs`
#'   (LV95 by default), or an object carrying its own crs (`sf`, `sfc`, `bbox`,
#'   `stars`), which is transformed to `crs` when needed.
#' @param crs Target coordinate reference system (EPSG code or [sf::st_crs()]).
#' @param call Environment used for the error call.
#'
#' @return An [sf::st_bbox()] object in `crs`, or `NULL` if `bbox` is `NULL`.
#'
#' @keywords internal
as_bbox <- function(bbox, crs = 2056, call = rlang::caller_env()) {
  if (is.null(bbox)) {
    return(NULL)
  }
  crs <- sf::st_crs(crs)

  if (inherits(bbox, c("sf", "sfc", "bbox", "stars"))) {
    bb <- sf::st_bbox(bbox)
    if (is.na(sf::st_crs(bb))) attr(bb, "crs") <- crs

    return(if (sf::st_crs(bb) == crs) bb else transform_bbox(bb, crs))
  }

  if (!is.numeric(bbox) || length(bbox) != 4 || anyNA(bbox)) {
    cli::cli_abort(
      "{.arg bbox} must be a numeric vector c(xmin, ymin, xmax, ymax).",
      call = call
    )
  }
  if (!is.null(names(bbox))) {
    check_names(names(bbox), c("xmin", "ymin", "xmax", "ymax"), "bbox", call = call)
    bbox <- bbox[c("xmin", "ymin", "xmax", "ymax")]
  }
  if (!isTRUE(sf::st_is_longlat(crs)) && all(abs(bbox) <= 360)) {
    cli::cli_abort(
      c(
        "x" = "{.arg bbox} looks like degrees (WGS84), but coordinates in {crs$input} are expected.",
        "i" = "LV95 example: {.code c(2590000, 1190000, 2610000, 1210000)}"
      ),
      call = call
    )
  }

  bb <- sf::st_bbox(
    stats::setNames(as.numeric(bbox), c("xmin", "ymin", "xmax", "ymax")),
    crs = crs
  )
  if (bb[["xmin"]] >= bb[["xmax"]] || bb[["ymin"]] >= bb[["ymax"]]) {
    cli::cli_abort(
      "{.arg bbox} is invalid: xmin < xmax and ymin < ymax are required.",
      call = call
    )
  }

  bb
}

#' Transform a bounding box into another coordinate reference system
#'
#' The edges are densified so that the transformed box fully encloses the curved
#' edges, not just the four corner points.
#'
#' @param bbox An [sf::st_bbox()] object.
#' @param crs Target coordinate reference system.
#' @param n Number of points sampled along each edge.
#'
#' @return An [sf::st_bbox()] object in `crs`.
#'
#' @keywords internal
transform_bbox <- function(bbox, crs, n = 50) {
  xs <- seq(bbox[["xmin"]], bbox[["xmax"]], length.out = n)
  ys <- seq(bbox[["ymin"]], bbox[["ymax"]], length.out = n)
  edges <- rbind(
    cbind(xs, bbox[["ymin"]]), cbind(xs, bbox[["ymax"]]),
    cbind(bbox[["xmin"]], ys), cbind(bbox[["xmax"]], ys)
  )

  sf::st_multipoint(edges) |>
    sf::st_sfc(crs = sf::st_crs(bbox)) |>
    sf::st_transform(crs) |>
    sf::st_bbox()
}


# ---- Table to raster ------------------------------------------------------

#' Convert a regular point or cell table into a stars object
#'
#' Cells are filled directly by their index rather than rasterised from
#' geometries. That is exact, fast, and it verifies that the coordinates really
#' do lie on a regular grid instead of silently snapping them.
#'
#' @param data Table with coordinate columns and value columns.
#' @param variables Columns that become attributes (`NULL` = all numeric columns
#'   except the coordinates).
#' @param coords Names of the coordinate columns (easting, northing).
#' @param cellsize Cell size in crs units.
#' @param anchor What the coordinates refer to: the lower left cell corner (as in
#'   the BFS hectare grid) or the cell centre.
#' @param crs Coordinate reference system of the coordinates.
#' @param bbox Optional extent (see [as_bbox()]), snapped outwards onto the grid.
#'   With a fixed `bbox` all results share the same grid and can be combined.
#'
#' @return A two-dimensional `stars` object with one attribute per variable.
#'
#' @export
table_to_stars <- function(data,
                           variables = NULL,
                           coords = c("E_KOORD", "N_KOORD"),
                           cellsize = 100,
                           anchor = c("lowerleft", "center"),
                           crs = 2056,
                           bbox = NULL) {
  anchor <- rlang::arg_match(anchor)
  check_names(names(data), coords, "data")
  variables <- variables %||% setdiff(names(data)[purrr::map_lgl(data, is.numeric)], coords)
  check_names(names(data), variables, "data")
  if (nrow(data) == 0 && is.null(bbox)) {
    cli::cli_abort("No data and no {.arg bbox}: the raster extent is undefined.")
  }

  # lower left cell corner
  shift <- if (anchor == "center") cellsize / 2 else 0
  x0 <- data[[coords[[1]]]] - shift
  y0 <- data[[coords[[2]]]] - shift

  # derive the grid origin and verify every coordinate sits on the grid
  origin <- if (nrow(data) > 0) c(x0[[1]], y0[[1]]) %% cellsize else c(0, 0)
  steps_x <- (x0 - origin[[1]]) / cellsize
  steps_y <- (y0 - origin[[2]]) / cellsize
  off_grid <- abs(steps_x - round(steps_x)) > 1e-6 | abs(steps_y - round(steps_y)) > 1e-6
  if (any(off_grid)) {
    cli::cli_abort(c(
      "x" = "{sum(off_grid)} coordinate{?s} {?does/do} not lie on the {cellsize} m grid.",
      "i" = "Check {.arg cellsize}, {.arg anchor} and {.arg coords}."
    ))
  }

  if (is.null(bbox)) {
    ext <- c(xmin = min(x0), ymin = min(y0), xmax = max(x0) + cellsize, ymax = max(y0) + cellsize)
  } else {
    bb <- as_bbox(bbox, crs)
    snap <- function(value, o, fun) o + fun((value - o) / cellsize) * cellsize
    ext <- c(
      xmin = snap(bb[["xmin"]], origin[[1]], floor),
      ymin = snap(bb[["ymin"]], origin[[2]], floor),
      xmax = snap(bb[["xmax"]], origin[[1]], ceiling),
      ymax = snap(bb[["ymax"]], origin[[2]], ceiling)
    )
    keep <- x0 >= ext[["xmin"]] & x0 < ext[["xmax"]] & y0 >= ext[["ymin"]] & y0 < ext[["ymax"]]
    data <- data[keep, ]
    x0 <- x0[keep]
    y0 <- y0[keep]
  }

  nx <- as.integer(round((ext[["xmax"]] - ext[["xmin"]]) / cellsize))
  ny <- as.integer(round((ext[["ymax"]] - ext[["ymin"]]) / cellsize))
  idx <- cbind(
    as.integer(round((x0 - ext[["xmin"]]) / cellsize)) + 1L,
    as.integer(round((ext[["ymax"]] - y0) / cellsize))
  )

  if (anyDuplicated(idx) > 0) {
    cli::cli_abort(c(
      "x" = "Several rows fall into the same raster cell.",
      "i" = "Does the table hold more than one year or variant? Filter or aggregate first."
    ))
  }

  layers <- purrr::map(rlang::set_names(variables), \(variable) {
    values <- data[[variable]]
    layer <- array(values[NA_integer_], dim = c(nx, ny)) # NA of the column's own type
    layer[idx] <- values
    layer
  })

  grid <- stars::st_as_stars(sf::st_bbox(ext, crs = sf::st_crs(crs)), nx = nx, ny = ny, values = NA)

  stars::st_as_stars(layers, dimensions = stars::st_dimensions(grid))
}


# ---- Asset to stars -------------------------------------------------------

#' Stop GDAL probing for sidecar files over the network
#'
#' Opening a remote raster, GDAL looks for companion files (`.aux.xml`, `.ovr`,
#' `.msk`, ...). On data.geo.admin.ch none of them exist, so every probe costs an
#' HTTP request and raises a 404 warning — 39 of them for a single GeoTIFF.
#' Restricting the extensions `/vsicurl/` may fetch suppresses the search.
#' Cloud-optimised GeoTIFFs carry their overviews inline, so nothing is lost.
#'
#' The setting is restored when the calling function exits, so it cannot leak
#' into other `/vsicurl/` use in the same session.
#'
#' @param extensions File extensions `/vsicurl/` is allowed to fetch.
#' @param envir Frame the reset is registered on.
#'
#' @return The previous value, invisibly.
#'
#' @keywords internal
local_quiet_vsicurl <- function(extensions = ".tif", envir = rlang::caller_env()) {
  name <- "CPL_VSIL_CURL_ALLOWED_EXTENSIONS"
  previous <- Sys.getenv(name, unset = NA_character_)

  withr::local_envvar(
    stats::setNames(list(paste(extensions, collapse = ",")), name),
    .local_envir = envir
  )

  invisible(previous)
}

#' Read a GeoTIFF asset as stars
#'
#' Uncompressed assets are streamed through GDAL's `/vsicurl/` driver so only the
#' requested window is transferred; compressed ones are downloaded and cached.
#'
#' @inheritParams read_asset_stars
#' @param cache_dir Cache directory for downloads.
#'
#' @return A `stars` object.
#'
#' @keywords internal
read_tif_stars <- function(asset, variables, bbox, crs, cache_dir) {
  compression <- asset[["compression"]] %||% NA_character_
  source <- if (is.na(compression)) {
    paste0("/vsicurl/", asset$href)
  } else {
    download_geo_admin_asset(asset, cache_dir)
  }

  if (is.na(compression)) local_quiet_vsicurl()

  x <- stars::read_stars(source, proxy = TRUE)
  if (sf::st_crs(x) != sf::st_crs(crs)) {
    cli::cli_abort("{.val {asset$asset}} is not in {sf::st_crs(crs)$input}.")
  }
  if (!is.null(bbox)) x <- sf::st_crop(x, bbox)
  x <- stars::st_as_stars(x)

  if ("band" %in% names(stars::st_dimensions(x))) x <- split(x, "band")
  if (is.null(variables)) {
    return(x)
  }

  if (length(x) == 1 && length(variables) == 1 && !variables %in% names(x)) {
    cli::cli_inform("Single band GeoTIFF {.val {asset$asset}}: naming the attribute {.val {variables}}.")
    names(x) <- variables
  }
  check_names(names(x), variables, paste("the bands of", asset$asset))

  x[variables]
}

#' Read one asset as a georeferenced stars object
#'
#' GeoTIFFs are read directly; tabular assets (parquet, csv hectare grids) are
#' downloaded, cached and rasterised via [table_to_stars()].
#'
#' @param asset A single row from [get_geo_admin_assets()].
#' @param variables Attributes or bands to keep (`NULL` = all). For large extents
#'   select only what is needed: Switzerland at 100 m is ~8 million cells per
#'   variable.
#' @param bbox Extent, numeric in LV95 or any `sf`/`bbox`/`stars` object.
#' @param coords,cellsize,anchor See [table_to_stars()] (tabular formats only).
#' @param crs Expected coordinate reference system.
#' @param cache_dir Cache directory for downloads.
#'
#' @return A `stars` object, normalised so that grids are directly comparable.
#'
#' @export
read_asset_stars <- function(asset,
                             variables = NULL,
                             bbox = NULL,
                             coords = c("E_KOORD", "N_KOORD"),
                             cellsize = 100,
                             anchor = c("lowerleft", "center"),
                             crs = 2056,
                             cache_dir = geo_admin_cache_dir()) {
  asset <- check_single_asset(asset)
  check_asset_crs(asset, crs)
  bbox <- as_bbox(bbox, crs)

  x <- switch(asset$format,
    tif = read_tif_stars(asset, variables, bbox, crs, cache_dir),
    parquet = ,
    csv = {
      columns <- if (!is.null(variables)) c(coords, variables)
      download_geo_admin_asset(asset, cache_dir) |>
        read_asset_table(asset$format, columns) |>
        table_to_stars(variables, coords, cellsize, anchor, crs, bbox)
    },
    cli::cli_abort(c(
      "x" = "Format {.val {asset$format}} cannot be read as a raster.",
      "i" = "Supported: tif, parquet, csv. Read vector data (gpkg) with {.fn sf::read_sf}."
    ))
  )

  # uniform dimensions (from = 1) so that grids can be compared directly
  sf::st_normalize(x)
}
