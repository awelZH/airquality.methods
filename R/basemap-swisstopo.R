# =============================================================================
#  Basemaps from the swisstopo web map service
# -----------------------------------------------------------------------------
#  One GetMap request returns the whole extent as a single georeferenced PNG,
#  which annotation_raster() then places at its exact LV95 corners. No tile
#  arithmetic, no reprojection, no geospatial stack — LV95 is metric, so a
#  fixed-ratio Cartesian panel is already the right projection.
#
#  Two things here are less obvious than they look:
#
#   * the PNG is read with native = TRUE. ggplot2's annotation_raster() passes
#     a nativeRaster straight through, while the non-native path goes via
#     grDevices::as.raster() and turns a 2000x1700 basemap into ~3.4 million R
#     strings.
#
#   * the response is checked for the PNG magic bytes. OGC servers report
#     errors as a ServiceException XML document with HTTP status 200, so a
#     failed request otherwise arrives as a perfectly successful download of
#     something that is not an image.
#
#  swisstopo's terms require attribution and ask that the service not be
#  queried at high intensity. That is why results are cached on disk by
#  default, and why the attribution rides along on the returned object.
# =============================================================================

# WKT for EPSG:2056, written next to a pinned basemap so the file is
# self-describing in QGIS and friends.
.LV95_WKT <- paste0(
  'PROJCS["CH1903+ / LV95",GEOGCS["CH1903+",DATUM["CH1903+",',
  'SPHEROID["Bessel 1841",6377397.155,299.1528128]],',
  'PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433]],',
  'PROJECTION["Hotine_Oblique_Mercator_Azimuth_Center"],',
  'PARAMETER["latitude_of_center",46.9524055555556],',
  'PARAMETER["longitude_of_center",7.43958333333333],',
  'PARAMETER["azimuth",90],PARAMETER["rectified_grid_angle",90],',
  'PARAMETER["scale_factor",1],PARAMETER["false_easting",2600000],',
  'PARAMETER["false_northing",1200000],UNIT["metre",1],AUTHORITY["EPSG","2056"]]')

.swisstopo_layers <- c(
  grey   = "ch.swisstopo.pixelkarte-grau",
  colour = "ch.swisstopo.pixelkarte-farbe",
  color  = "ch.swisstopo.pixelkarte-farbe",
  image  = "ch.swisstopo.swissimage"
)

#' Bounding box in LV95 around a set of sites
#'
#' @param sites Data frame with the site coordinates.
#' @param east,north Names of the easting and northing columns. As elsewhere,
#'   note that the default `x_lv95` is the **easting**.
#' @param radius Rose radius in metres, added on every side so the roses fit
#'   inside the map.
#' @param margin Extra space, as a fraction of the larger side.
#' @param asp Target width/height. The shorter axis is padded symmetrically to
#'   reach it; nothing is ever cropped. `NULL` = leave as is.
#'
#' @return A named numeric vector `xmin`, `ymin`, `xmax`, `ymax`, rounded
#'   outward to whole metres, of class `lv95_bbox`.
#'
#' @details
#' The outward rounding is load-bearing rather than cosmetic: the rounded box
#' is what goes into both the WMS request and the cache key, so the cached
#' file and the extent it claims to cover can never disagree.
#'
#' @examples
#' s <- data.frame(site = c("A", "B"),
#'                 x_lv95 = c(2686000, 2687000),
#'                 y_lv95 = c(1256000, 1257000))
#' bbox_lv95(s, radius = 200)
#' bbox_lv95(s, radius = 200, asp = 16 / 9)
#' @export
bbox_lv95 <- function(sites,
                      east   = "x_lv95",
                      north  = "y_lv95",
                      radius = 0,
                      margin = 0.08,
                      asp    = NULL) {

  # A bbox needs coordinates, not identities — so unlike polar_map() this
  # tolerates a site table with no site column. The coordinate checks are the
  # same ones, though; a transposed pair must not slip through here either.
  sites <- as.data.frame(sites)
  sites$.site <- if ("site" %in% names(sites)) as.character(sites$site) else
    as.character(seq_len(nrow(sites)))
  ce <- .polar_map_centres(sites, ".site", east, north)

  xr <- range(ce$e) + c(-1, 1) * radius
  yr <- range(ce$n) + c(-1, 1) * radius

  m  <- margin * max(diff(xr), diff(yr))
  xr <- xr + c(-1, 1) * m
  yr <- yr + c(-1, 1) * m

  if (!is.null(asp)) {
    if (!is.numeric(asp) || length(asp) != 1L || !is.finite(asp) || asp <= 0) {
      cli::cli_abort("{.arg asp} must be a single positive number (width / height).")
    }
    w <- diff(xr); h <- diff(yr)
    if (w / h < asp) {                       # too tall -> widen
      need <- asp * h - w
      xr <- xr + c(-1, 1) * need / 2
    } else {                                  # too wide -> heighten
      need <- w / asp - h
      yr <- yr + c(-1, 1) * need / 2
    }
  }

  structure(c(xmin = floor(xr[1L]), ymin = floor(yr[1L]),
              xmax = ceiling(xr[2L]), ymax = ceiling(yr[2L])),
            class = c("lv95_bbox", "numeric"))
}

#' Pixel dimensions for a bbox
#'
#' Deliberately not a `dpi` argument: dpi only means something once a physical
#' figure size is known, and the server knows neither. Pixels along the longer
#' edge, or ground resolution, are the quantities that actually determine the
#' request.
#' @keywords internal
.basemap_size <- function(bbox, px = 2000L, res = NULL, max_px = 10000L) {

  w_m <- bbox[["xmax"]] - bbox[["xmin"]]
  h_m <- bbox[["ymax"]] - bbox[["ymin"]]
  if (w_m <= 0 || h_m <= 0) cli::cli_abort("{.arg bbox} has zero extent.")

  if (!is.null(res)) {
    if (!is.numeric(res) || length(res) != 1L || res <= 0) {
      cli::cli_abort("{.arg res} must be a single positive number (metres per pixel).")
    }
    width  <- max(1L, as.integer(round(w_m / res)))
    height <- max(1L, as.integer(round(h_m / res)))
  } else if (w_m >= h_m) {
    width  <- as.integer(px)
    height <- max(1L, as.integer(round(px * h_m / w_m)))
  } else {
    height <- as.integer(px)
    width  <- max(1L, as.integer(round(px * w_m / h_m)))
  }

  # Clamp preserving aspect, so the image is never stretched.
  if (max(width, height) > max_px) {
    f      <- max_px / max(width, height)
    width  <- max(1L, as.integer(round(width * f)))
    height <- max(1L, as.integer(round(height * f)))
  }
  c(width = width, height = height)
}

#' The GetMap URL
#'
#' WMS 1.1.1 with `SRS=` on purpose: 1.3.0 returns byte-identical output here,
#' and staying on 1.1.1 keeps the axis-order question out of the code
#' entirely.
#' @keywords internal
.basemap_url <- function(bbox, layer, width, height) {
  paste0(
    "https://wms.geo.admin.ch/?SERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap",
    "&LAYERS=", utils::URLencode(layer, reserved = TRUE),
    "&STYLES=&SRS=EPSG:2056",
    "&BBOX=", paste(c(bbox[["xmin"]], bbox[["ymin"]],
                      bbox[["xmax"]], bbox[["ymax"]]), collapse = ","),
    "&WIDTH=", width, "&HEIGHT=", height, "&FORMAT=image/png")
}

#' Cache location for one request
#'
#' A deterministic name rather than a hash: the parameter tuple is short
#' enough to be the key, it adds no dependency, and you can list the cache and
#' see what is in it.
#' @keywords internal
.basemap_cache_path <- function(bbox, layer, width, height) {
  file.path(tools::R_user_dir("airquality.methods", "cache"), "basemap",
            # "%.0f", not "%d": the documented contract takes any named
            # numeric vector, and real LV95 site coordinates are often
            # fractional (y_lv95 = 1257939.9). "%d" aborts on those.
            sprintf("%s_%.0f_%.0f_%.0f_%.0f_%dx%d.png",
                    gsub("[^A-Za-z0-9]+", "-", layer),
                    bbox[["xmin"]], bbox[["ymin"]],
                    bbox[["xmax"]], bbox[["ymax"]],
                    as.integer(width), as.integer(height)))
}

#' World file contents for a bbox
#' @keywords internal
.basemap_worldfile <- function(bbox, width, height) {
  a <- (bbox[["xmax"]] - bbox[["xmin"]]) / width
  e <- -(bbox[["ymax"]] - bbox[["ymin"]]) / height
  sprintf("%.12f", c(a, 0, 0, e,
                     bbox[["xmin"]] + a / 2,
                     bbox[["ymax"]] + e / 2))
}

#' Read a world file back into a bbox
#' @keywords internal
.basemap_read_worldfile <- function(path, width, height) {
  n <- as.numeric(readLines(path, warn = FALSE))
  if (length(n) < 6L || anyNA(n[1:6])) {
    cli::cli_abort("{.file {path}} is not a valid world file.")
  }
  xmin <- n[5L] - n[1L] / 2
  ymax <- n[6L] - n[4L] / 2
  structure(c(xmin = xmin, ymin = ymax + n[4L] * height,
              xmax = xmin + n[1L] * width, ymax = ymax),
            class = c("lv95_bbox", "numeric"))
}

#' Verify that a file really is a PNG
#' @keywords internal
.basemap_check_png <- function(path, layer, width, height) {
  sig <- tryCatch(readBin(path, "raw", 8L), error = function(e) raw(0))
  if (identical(sig, as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))) {
    return(invisible(TRUE))
  }
  txt <- tryCatch(paste(utils::head(readLines(path, warn = FALSE), 20),
                        collapse = " "),
                  error = function(e) "")
  cli::cli_abort(c(
    "The WMS server did not return a PNG.",
    i = "Layer {.val {layer}}, {width}x{height} px.",
    x = "{substr(trimws(txt), 1, 400)}",
    i = "Check the layer name against
         {.url https://wms.geo.admin.ch/?SERVICE=WMS&REQUEST=GetCapabilities}."
  ))
}

#' A swisstopo basemap for a bounding box
#'
#' Fetches one georeferenced PNG from the swisstopo web map service and wraps
#' it for use with [polar_map()] or [annotation_basemap()].
#'
#' @param bbox A [bbox_lv95()] result, or any named vector with `xmin`, `ymin`,
#'   `xmax`, `ymax` in LV95 -- `sf::st_bbox()` of an LV95 object works, and so
#'   does [bbox_zh_lv95].
#' @param layer Layer to fetch. The aliases `"colour"` (default), `"grey"` and
#'   `"image"` map to `ch.swisstopo.pixelkarte-farbe`,
#'   `ch.swisstopo.pixelkarte-grau` and `ch.swisstopo.swissimage`; any other
#'   WMS layer id is passed through untouched.
#' @param px Pixels along the longer edge. The other edge follows from the
#'   bbox, so the image is never stretched.
#' @param res Metres per pixel, as an alternative to `px`.
#' @param max_px Upper bound on either edge; the service allows 10000.
#' @param file Optional path to pin a copy of the PNG, with a `.pgw` world
#'   file, a `.prj` and a `.layer` beside it. Once that file exists it is
#'   read directly and no request is made — which is what makes a Quarto
#'   render reproducible offline. The `.layer` sidecar records which WMS
#'   layer the pin holds, so a later call asking for a different one is
#'   warned about; a pin written before that sidecar existed carries none
#'   and is taken at face value. `refresh = TRUE` re-fetches and rewrites
#'   all three.
#' @param cache Use the on-disk cache under
#'   `tools::R_user_dir("airquality.methods", "cache")`.
#' @param refresh Re-fetch even when a cached or pinned copy exists.
#' @param quiet Suppress progress messages.
#'
#' @return An object of class `swisstopo_basemap`.
#'
#' @section Terms of use:
#' swisstopo geodata served by `wms.geo.admin.ch` are free to use **with
#' attribution**. [polar_map()] prints `"Kartengrundlage: \u00a9 swisstopo"` as
#' the plot caption whenever a basemap is drawn; do not remove it from
#' published figures. It rides on the basemap object rather than on
#' [polar_map_decor()], so it appears when, and only when, there is map
#' material to attribute. The same terms ask that the service not be queried
#' at high intensity, which is why `cache = TRUE` is the default — a repeated report
#' render must not hit the service again. See
#' <https://www.geo.admin.ch/en/general-terms-of-use-fsdi>.
#'
#' @examples
#' \dontrun{
#' s  <- read.csv("site_metadata.csv")   # site, x_lv95 (E), y_lv95 (N)
#' bb <- bbox_lv95(s, radius = 200)
#' bm <- basemap_swisstopo(bb)                     # colour Landeskarte
#' bm <- basemap_swisstopo(bb, layer = "grey")     # grey Landeskarte
#'
#' # Pin it next to the report so later renders need no network
#' bm <- basemap_swisstopo(bb, file = "report/figures/basemap.png")
#' }
#' @export
basemap_swisstopo <- function(bbox,
                              layer   = "colour",
                              px      = 2000L,
                              res     = NULL,
                              max_px  = 10000L,
                              file    = NULL,
                              cache   = TRUE,
                              refresh = FALSE,
                              quiet   = FALSE) {

  if (!requireNamespace("png", quietly = TRUE)) {
    cli::cli_abort(c(
      "Reading a basemap requires the {.pkg png} package.",
      i = 'Install it with {.run install.packages("png")}.'
    ))
  }
  # Recorded here and not where it is used: `layer` is normalised to its WMS
  # id further down, and an assigned argument is no longer `missing()`.
  asked <- c(if (!missing(layer)) "layer", if (!missing(px)) "px",
             if (!missing(res)) "res")

  need <- c("xmin", "ymin", "xmax", "ymax")
  if (!is.numeric(bbox) || !all(need %in% names(bbox))) {
    cli::cli_abort(c(
      "{.arg bbox} must be a numeric vector with names {.val {need}}.",
      i = "{.fn bbox_lv95} builds one from your site table."
    ))
  }
  layer <- unname(if (layer %in% names(.swisstopo_layers))
    .swisstopo_layers[[layer]] else layer)

  wh     <- .basemap_size(bbox, px = px, res = res, max_px = max_px)
  width  <- wh[["width"]]; height <- wh[["height"]]
  if (width * height > 4e7 && !quiet) {
    cli::cli_warn(c(
      "Requesting {width}x{height} px ({round(width * height / 1e6)} Mpx).",
      i = "That is a large download; consider a smaller {.arg px}."
    ))
  }

  # 1. A pinned file wins outright — that is the offline-render path.
  if (!is.null(file) && file.exists(file) && !refresh) {
    img <- png::readPNG(file, native = TRUE)
    d   <- dim(img)

    # Only complain when the pin actually disagrees. Re-rendering a report
    # repeats the same `px` on every pass -- that is the workflow, not a
    # mistake -- so a size that matches the file stays silent. The layer is
    # read from the sidecar written when the pin was made; a pin from before
    # that sidecar existed cannot be checked and is taken at face value.
    pinned_layer <- .basemap_pinned_layer(file)
    stale <- c(if ("layer" %in% asked && !is.null(pinned_layer) &&
                   !identical(pinned_layer, layer)) "layer",
               if (length(intersect(asked, c("px", "res"))) &&
                   (d[2L] != width || d[1L] != height))
                 intersect(asked, c("px", "res")))
    if (length(stale)) {
      cli::cli_warn(c(
        "!" = "{.file {file}} is already pinned, so
               {cli::qty(length(stale))}{.arg {stale}} {?is/are} ignored
               ({d[2L]}x{d[1L]} px on disk).",
        i = "Pass {.code refresh = TRUE} to re-fetch with
             {cli::qty(length(stale))}{?it/them}."
      ))
    }
    pgw <- paste0(tools::file_path_sans_ext(file), ".pgw")
    bb  <- if (file.exists(pgw)) .basemap_read_worldfile(pgw, d[2L], d[1L]) else bbox
    if (file.exists(pgw) && max(abs(bb[need] - bbox[need])) > 1) {
      cli::cli_warn(c(
        "{.file {file}} covers a different extent than {.arg bbox}.",
        i = "Using the extent from the world file; pass
             {.code refresh = TRUE} to re-fetch."
      ))
    }
    return(.swisstopo_basemap(img, bb, layer, d[2L], d[1L], file, file))
  }

  cache_path <- .basemap_cache_path(bbox, layer, width, height)
  path       <- NULL

  if (cache && file.exists(cache_path) && !refresh) {
    path <- cache_path
    if (!quiet) cli::cli_inform(c(v = "Basemap from cache."))
  } else {
    url <- .basemap_url(bbox, layer, width, height)
    if (!quiet) {
      cli::cli_inform(c(i = "Fetching {.val {layer}}, {width}x{height} px
                             ({round((bbox[['xmax']] - bbox[['xmin']]) / width, 2)}
                             m/px)."))
    }
    tmp <- tempfile(fileext = ".png")
    old <- getOption("HTTPUserAgent")
    on.exit(options(HTTPUserAgent = old), add = TRUE)
    options(HTTPUserAgent = sprintf("airquality.methods R/%s", getRversion()))

    # Some download methods only warn on a non-zero status, so the status has
    # to be read: otherwise a truncated or empty response falls through to
    # .basemap_check_png() and is reported as "not a PNG", which sends the
    # reader looking at the wrong end of the problem.
    status <- tryCatch(
      utils::download.file(url, tmp, mode = "wb", quiet = TRUE),
      error = function(e) {
        cli::cli_abort(c(
          "Could not reach the swisstopo web map service.",
          x = conditionMessage(e),
          i = "Check your connection, or pass {.arg file} pointing at a copy
               fetched earlier."
        ))
      })
    if (!identical(as.integer(status), 0L)) {
      cli::cli_abort(c(
        "The download from the swisstopo web map service failed
         (status {status}).",
        i = "Check your connection, or pass {.arg file} pointing at a copy
             fetched earlier."
      ))
    }
    .basemap_check_png(tmp, layer, width, height)

    if (cache) {
      dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
      # Rename only after validation, so a truncated or error response never
      # takes up residence in the cache.
      if (!file.rename(tmp, cache_path)) file.copy(tmp, cache_path, overwrite = TRUE)
      path <- cache_path
    } else {
      path <- tmp
    }
  }

  if (!is.null(file)) {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    file.copy(path, file, overwrite = TRUE)
    writeLines(.basemap_worldfile(bbox, width, height),
               paste0(tools::file_path_sans_ext(file), ".pgw"))
    writeLines(.LV95_WKT, paste0(tools::file_path_sans_ext(file), ".prj"))
    # Third sidecar, and the reason is the warning in step 1: a PNG cannot
    # say which layer it holds, so without this the only honest thing to do
    # with an explicit `layer` on a pinned file is to ignore it and say so --
    # every run, whether or not anything is actually wrong. Written here the
    # question becomes answerable, and the warning fires only when the pin
    # really does hold something else. A pin from before this sidecar existed
    # has none, and is taken at face value.
    writeLines(layer, .basemap_layer_file(file))
    if (!quiet) cli::cli_inform(c(v = "Pinned to {.file {file}}."))
  }

  .swisstopo_basemap(png::readPNG(path, native = TRUE), bbox, layer,
               width, height, path, file)
}

#' Where a pinned basemap records which WMS layer it holds
#'
#' Added 2026-08-27. The georeferencing has the `.pgw`/`.prj` pair; this is
#' the same idea for the one property a PNG cannot carry itself, and it
#' turns "your `layer` is ignored" from something said on every run into
#' something said when it is true.
#' @keywords internal
.basemap_layer_file <- function(file) {
  paste0(tools::file_path_sans_ext(file), ".layer")
}

#' The layer a pinned basemap records, or NULL if it records none
#' @keywords internal
.basemap_pinned_layer <- function(file) {
  f <- .basemap_layer_file(file)
  if (!file.exists(f)) return(NULL)
  l <- tryCatch(trimws(readLines(f, warn = FALSE)[1L]), error = function(e) NULL)
  if (is.null(l) || !length(l) || is.na(l) || !nzchar(l)) NULL else l
}


#' @keywords internal
.swisstopo_basemap <- function(img, bbox, layer, width, height, path, file) {
  w_m <- bbox[["xmax"]] - bbox[["xmin"]]
  h_m <- bbox[["ymax"]] - bbox[["ymin"]]
  structure(list(
    raster = img, bbox = bbox, layer = layer,
    width = width, height = height,
    res = c(x = w_m / width, y = h_m / height),
    asp = w_m / h_m, path = path, file = file,
    # Escaped rather than literal: R CMD check requires ASCII source outside
    # comments. This is the citation swisstopo's terms of use ask for.
    attribution = "Kartengrundlage: \u00a9 swisstopo"
  ), class = "swisstopo_basemap")
}

#' @export
print.swisstopo_basemap <- function(x, ...) {
  cli::cli_text("{.cls swisstopo_basemap} {.val {x$layer}}")
  cli::cli_ul(c(
    "extent: {x$bbox[['xmin']]}-{x$bbox[['xmax']]} E,
     {x$bbox[['ymin']]}-{x$bbox[['ymax']]} N (LV95)",
    "size: {x$width}x{x$height} px, {round(x$res[['x']], 2)} m/px,
     aspect {round(x$asp, 3)}",
    "file: {.file {x$file %||% x$path}}",
    "attribution: {x$attribution}"
  ))
  invisible(x)
}

#' Draw a basemap under a ggplot in LV95 coordinates
#'
#' Useful on its own: any ggplot whose x and y are LV95 metres can take a
#' swisstopo backdrop this way, with or without [polar_map()].
#'
#' @param basemap A [basemap_swisstopo()] object.
#' @param interpolate Smooth the image when it is resampled to the panel.
#'   `TRUE` by default — unlike the roses, where sharp cells are the honest
#'   default, a basemap is a continuous image and smoothing is what you want.
#' @return A ggplot2 layer.
#' @examples
#' \dontrun{
#' ggplot2::ggplot() +
#'   annotation_basemap(bm) +
#'   ggplot2::coord_fixed()
#' }
#' @export
annotation_basemap <- function(basemap, interpolate = TRUE) {
  if (!inherits(basemap, "swisstopo_basemap")) {
    cli::cli_abort(c(
      "{.arg basemap} must be a {.cls swisstopo_basemap}.",
      i = "{.fn basemap_swisstopo} returns one."
    ))
  }
  b <- basemap$bbox
  ggplot2::annotation_raster(
    basemap$raster,
    xmin = b[["xmin"]], xmax = b[["xmax"]],
    ymin = b[["ymin"]], ymax = b[["ymax"]], interpolate = interpolate)
}
