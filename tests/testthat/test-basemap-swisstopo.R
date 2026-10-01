test_that("bbox_lv95 pads, rounds outward, and hits the target aspect", {
  s  <- fake_sites()
  bb <- bbox_lv95(s, radius = 200, margin = 0)

  expect_s3_class(bb, "lv95_bbox")
  expect_named(bb, c("xmin", "ymin", "xmax", "ymax"))
  expect_equal(bb[["xmin"]], 2686000 - 200)
  expect_equal(bb[["xmax"]], 2687000 + 200)
  expect_true(all(bb == trunc(bb)))               # whole metres

  # margin widens by a fraction of the larger side, symmetrically
  b2 <- bbox_lv95(s, radius = 200, margin = 0.1)
  expect_lt(b2[["xmin"]], bb[["xmin"]])
  expect_gt(b2[["xmax"]], bb[["xmax"]])

  # asp pads the short axis and never shrinks
  for (a in c(0.5, 1, 16 / 9)) {
    ba <- bbox_lv95(s, radius = 200, asp = a)
    got <- (ba[["xmax"]] - ba[["xmin"]]) / (ba[["ymax"]] - ba[["ymin"]])
    expect_equal(unname(got), a, tolerance = 0.01)
    expect_gte(ba[["xmax"]] - ba[["xmin"]], bb[["xmax"]] - bb[["xmin"]] - 1)
  }
  expect_error(bbox_lv95(s, asp = -1), "positive")
})

test_that("bbox_lv95 works without a site column but still checks coordinates", {
  s <- data.frame(x_lv95 = c(2686000, 2687000), y_lv95 = c(1256000, 1257000))
  expect_s3_class(bbox_lv95(s), "lv95_bbox")
  expect_error(bbox_lv95(s, east = "y_lv95", north = "x_lv95"), "transposed")
})

test_that(".basemap_size preserves aspect and honours the cap", {
  bb <- c(xmin = 0, ymin = 0, xmax = 2000, ymax = 1000)

  wh <- .basemap_size(bb, px = 1000)
  expect_equal(unname(wh[["width"]]), 1000)
  expect_equal(unname(wh[["height"]]), 500)

  # tall bbox -> px applies to the longer (vertical) edge
  tall <- c(xmin = 0, ymin = 0, xmax = 1000, ymax = 2000)
  wh2  <- .basemap_size(tall, px = 1000)
  expect_equal(unname(wh2[["height"]]), 1000)
  expect_equal(unname(wh2[["width"]]), 500)

  # res is an alternative spelling of the same thing
  wh3 <- .basemap_size(bb, res = 2)
  expect_equal(unname(wh3[["width"]]), 1000)
  expect_equal(unname(wh3[["height"]]), 500)

  # the clamp keeps the aspect
  wh4 <- .basemap_size(bb, px = 50000, max_px = 10000)
  expect_equal(unname(wh4[["width"]]), 10000)
  expect_equal(unname(wh4[["height"]]), 5000)

  expect_error(.basemap_size(c(xmin = 0, ymin = 0, xmax = 0, ymax = 1)), "zero extent")
})

test_that(".basemap_url is stable", {
  bb <- c(xmin = 2685664, ymin = 1255267, xmax = 2689297, ymax = 1258391)
  expect_identical(
    .basemap_url(bb, "ch.swisstopo.pixelkarte-grau", 1200L, 1032L),
    paste0("https://wms.geo.admin.ch/?SERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap",
           "&LAYERS=ch.swisstopo.pixelkarte-grau&STYLES=&SRS=EPSG:2056",
           "&BBOX=2685664,1255267,2689297,1258391",
           "&WIDTH=1200&HEIGHT=1032&FORMAT=image/png")
  )
})

test_that(".basemap_cache_path is deterministic and readable", {
  bb <- c(xmin = 2685664, ymin = 1255267, xmax = 2689297, ymax = 1258391)
  p1 <- .basemap_cache_path(bb, "ch.swisstopo.pixelkarte-grau", 1200L, 1032L)
  p2 <- .basemap_cache_path(bb, "ch.swisstopo.pixelkarte-grau", 1200L, 1032L)

  expect_identical(p1, p2)
  expect_match(basename(p1),
               "^ch-swisstopo-pixelkarte-grau_2685664_1255267_2689297_1258391_1200x1032\\.png$")
  expect_false(identical(p1, .basemap_cache_path(bb, "x", 1200L, 1032L)))
})

test_that("the world file round-trips the bbox", {
  bb <- bbox_lv95(fake_sites(), radius = 200)
  w  <- 800L; h <- 640L

  f <- withr::local_tempfile(fileext = ".pgw")
  writeLines(.basemap_worldfile(bb, w, h), f)
  back <- .basemap_read_worldfile(f, w, h)

  expect_equal(unname(back[c("xmin", "ymin", "xmax", "ymax")]),
               unname(bb[c("xmin", "ymin", "xmax", "ymax")]),
               tolerance = 1e-6)
  empty <- withr::local_tempfile(fileext = ".pgw")
  writeLines(c("1.0", "0"), empty)             # too few numbers to be a world file
  expect_error(.basemap_read_worldfile(empty, w, h), "not a valid world file")
})

test_that("a ServiceException masquerading as a PNG is caught", {
  # This is the real failure mode: OGC servers report errors with HTTP 200,
  # so the download succeeds and the 'image' is XML.
  f <- withr::local_tempfile(fileext = ".png")
  writeLines(c('<?xml version="1.0"?>',
               '<ServiceExceptionReport><ServiceException code="LayerNotDefined">',
               'Layer "ch.swisstopo.nope" is not defined',
               '</ServiceException></ServiceExceptionReport>'), f)

  expect_error(.basemap_check_png(f, "ch.swisstopo.nope", 100L, 100L),
               "did not return a PNG")
  expect_error(.basemap_check_png(f, "ch.swisstopo.nope", 100L, 100L),
               "LayerNotDefined")
})

test_that("annotation_basemap validates its input", {
  expect_error(annotation_basemap(list()), "swisstopo_basemap")
})

# --- the request path, against a simulated service ----------------------------
# No test touches the network: download.file() is the one place the request
# leaves the package, so it is replaced by a fake WMS that answers with a PNG
# of exactly the requested size and records every URL it was asked for. The
# disk cache goes to a temporary directory, never the user's.

local_fake_wms <- function(env = parent.frame()) {
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir(.local_envir = env),
                      .local_envir = env)
  requests <- new.env()
  requests$urls <- character()
  testthat::local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      requests$urls <- c(requests$urls, url)
      w <- as.integer(sub(".*&WIDTH=([0-9]+).*", "\\1", url))
      h <- as.integer(sub(".*&HEIGHT=([0-9]+).*", "\\1", url))
      png::writePNG(array(0.5, dim = c(h, w, 3)), destfile)
      0L
    },
    .package = "utils", .env = env)
  requests
}

test_that("basemap_swisstopo fetches once and then serves from the cache", {
  skip_if_not_installed("png")
  wms <- local_fake_wms()

  bb <- bbox_lv95(fake_sites(), radius = 200)
  bm <- basemap_swisstopo(bb, px = 256, quiet = TRUE)

  expect_length(wms$urls, 1L)
  expect_match(wms$urls, "LAYERS=ch.swisstopo.pixelkarte-farbe", fixed = TRUE)
  expect_match(wms$urls, paste0("BBOX=", paste(bb[c("xmin", "ymin", "xmax", "ymax")],
                                              collapse = ",")), fixed = TRUE)
  expect_s3_class(bm, "swisstopo_basemap")
  expect_s3_class(bm$raster, "nativeRaster")     # compact, not 3.6M strings
  expect_equal(dim(bm$raster)[2], bm$width)
  expect_equal(max(bm$width, bm$height), 256)
  expect_equal(bm$attribution, "Kartengrundlage: \u00a9 swisstopo")
  expect_s3_class(annotation_basemap(bm), "ggproto")

  # A second call must be served from the cache, not the network.
  expect_message(basemap_swisstopo(bb, px = 256), "cache")
  expect_length(wms$urls, 1L)
})

test_that("a pinned file is written with its sidecars and read back without a request", {
  skip_if_not_installed("png")
  wms <- local_fake_wms()

  dir <- withr::local_tempdir()
  f   <- file.path(dir, "bm.png")
  bb  <- bbox_lv95(fake_sites(), radius = 200)

  basemap_swisstopo(bb, px = 256, layer = "grey", file = f, quiet = TRUE)
  expect_true(file.exists(f))
  expect_true(file.exists(file.path(dir, "bm.pgw")))
  expect_true(file.exists(file.path(dir, "bm.prj")))
  expect_equal(.basemap_pinned_layer(f), "ch.swisstopo.pixelkarte-grau")
  expect_length(wms$urls, 1L)

  again <- basemap_swisstopo(bb, px = 256, file = f, cache = FALSE, quiet = TRUE)
  expect_length(wms$urls, 1L)                    # the pin, not the service
  expect_s3_class(again, "swisstopo_basemap")
  expect_equal(unname(again$bbox[c("xmin", "ymin", "xmax", "ymax")]),
               unname(bb[c("xmin", "ymin", "xmax", "ymax")]), tolerance = 1)
})

# --- the offline path -------------------------------------------------------
# A pinned file is what a Quarto report renders from: once the PNG and its
# world file exist, no request is made. That path must therefore be testable
# without the network, and here it is -- the PNG is written locally.

pin_fake_basemap <- function(dir, bbox, px = 8L,
                             layer = "ch.swisstopo.pixelkarte-farbe") {
  f   <- file.path(dir, "bm.png")
  img <- array(runif(px * px * 3), dim = c(px, px, 3))
  png::writePNG(img, f)
  writeLines(.basemap_worldfile(bbox, px, px),
             file.path(dir, "bm.pgw"))
  writeLines(.LV95_WKT, file.path(dir, "bm.prj"))
  # The third sidecar, as basemap_swisstopo() writes it: which layer the
  # pin holds. `layer = NULL` makes a pin from before it existed.
  if (!is.null(layer)) writeLines(layer, .basemap_layer_file(f))
  f
}

test_that("a pinned PNG plus world file is read without any request", {
  skip_if_not_installed("png")

  dir <- withr::local_tempdir()
  bb  <- bbox_lv95(fake_sites(), radius = 200)
  f   <- pin_fake_basemap(dir, bb)

  bm <- basemap_swisstopo(bb, file = f, quiet = TRUE)

  expect_s3_class(bm, "swisstopo_basemap")
  expect_equal(bm$file, f)
  expect_equal(unname(bm$bbox[c("xmin", "ymin", "xmax", "ymax")]),
               unname(bb[c("xmin", "ymin", "xmax", "ymax")]), tolerance = 1)
  expect_equal(bm$attribution, "Kartengrundlage: \u00a9 swisstopo")
  expect_s3_class(annotation_basemap(bm), "ggproto")
})

test_that("a pinned file covering a different extent warns and keeps its own", {
  skip_if_not_installed("png")

  dir  <- withr::local_tempdir()
  bb   <- bbox_lv95(fake_sites(), radius = 200)
  f    <- pin_fake_basemap(dir, bb)
  else_where <- bb + 5000                     # same size, different place

  expect_warning(bm <- basemap_swisstopo(else_where, file = f, quiet = TRUE),
                 "different extent")
  expect_equal(unname(bm$bbox[["xmin"]]), unname(bb[["xmin"]]), tolerance = 1)
})

test_that("printing a basemap states extent, size and attribution", {
  skip_if_not_installed("png")

  dir <- withr::local_tempdir()
  bb  <- bbox_lv95(fake_sites(), radius = 200)
  bm  <- basemap_swisstopo(bb, file = pin_fake_basemap(dir, bb), quiet = TRUE)

  out <- paste(utils::capture.output(print(bm), type = "message"), collapse = " ")
  expect_match(out, "LV95")
  expect_match(out, "swisstopo")
  expect_invisible(suppressMessages(print(bm)))
})

test_that("a basemap under a plain ggplot needs no polar_map", {
  skip_if_not_installed("png")

  dir <- withr::local_tempdir()
  bb  <- bbox_lv95(fake_sites(), radius = 200)
  bm  <- basemap_swisstopo(bb, file = pin_fake_basemap(dir, bb), quiet = TRUE)

  p <- ggplot2::ggplot(fake_sites(), ggplot2::aes(x_lv95, y_lv95)) +
    annotation_basemap(bm) +
    ggplot2::geom_point() +
    ggplot2::coord_fixed(xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")],
                         expand = FALSE) +
    theme_polar_map()

  expect_equal(layer_geoms(p)[1], "GeomRasterAnn")
  expect_renders_clean(p)
})

test_that("the cache path survives a hand-built, fractional bbox", {
  # The documented contract is "any named vector with xmin/ymin/xmax/ymax",
  # and the project's own coordinates are fractional.
  bb <- c(xmin = 2685664.4, ymin = 1255267.2, xmax = 2689297.7, ymax = 1258391.9)

  path <- .basemap_cache_path(bb, "ch.swisstopo.pixelkarte-farbe", 100L, 80L)
  expect_match(basename(path), "^ch-swisstopo-pixelkarte-farbe_[0-9]+_[0-9]+_[0-9]+_[0-9]+_100x80[.]png$")
  expect_equal(path, .basemap_cache_path(bb, "ch.swisstopo.pixelkarte-farbe", 100L, 80L))
})

test_that("a pinned file wins over layer and size, and says so", {
  skip_if_not_installed("png")

  dir <- withr::local_tempdir()
  bb  <- bbox_lv95(fake_sites(), radius = 200)
  f   <- pin_fake_basemap(dir, bb)

  # asking for a different layer than the pin holds must not be silent: the
  # file is drawn either way, so an unwarned mismatch means a figure whose
  # caption and whose object disagree with what a reader sees
  expect_warning(basemap_swisstopo(bb, file = f, layer = "grey", quiet = TRUE),
                 "pinned")
  expect_warning(basemap_swisstopo(bb, file = f, px = 4000, quiet = TRUE),
                 "pinned")
  expect_no_warning(basemap_swisstopo(bb, file = f, quiet = TRUE))
  # a report re-renders with the same px every time: that must stay silent
  expect_no_warning(basemap_swisstopo(bb, file = f, px = 8, quiet = TRUE))
})

test_that("the pin records its layer, and only a real mismatch warns", {
  skip_if_not_installed("png")

  dir <- withr::local_tempdir()
  bb  <- bbox_lv95(fake_sites(), radius = 200)
  f   <- pin_fake_basemap(dir, bb, layer = "ch.swisstopo.pixelkarte-grau")

  expect_equal(.basemap_pinned_layer(f), "ch.swisstopo.pixelkarte-grau")

  # the layer the pin actually holds, asked for by its alias: silent, and
  # this is the case that used to warn on every single run
  expect_no_warning(basemap_swisstopo(bb, file = f, layer = "grey",
                                      quiet = TRUE))
  expect_warning(basemap_swisstopo(bb, file = f, layer = "colour",
                                   quiet = TRUE), "pinned")
})

test_that("a pin without the sidecar is taken at face value", {
  skip_if_not_installed("png")

  dir <- withr::local_tempdir()
  bb  <- bbox_lv95(fake_sites(), radius = 200)
  f   <- pin_fake_basemap(dir, bb, layer = NULL)   # pinned before 2026-08-27

  expect_null(.basemap_pinned_layer(f))
  expect_no_warning(basemap_swisstopo(bb, file = f, layer = "grey",
                                      quiet = TRUE))
})


test_that("a failed download is reported as such, not as a bad PNG", {
  skip_if_not_installed("png")

  testthat::local_mocked_bindings(
    download.file = function(...) 1L, .package = "utils")

  bb <- bbox_lv95(fake_sites(), radius = 200)
  expect_error(basemap_swisstopo(bb, px = 64, cache = FALSE, quiet = TRUE),
               "download")
})
