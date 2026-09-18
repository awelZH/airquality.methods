fake_raster <- function(name = "pm25") {
  grid <- make_reference_grid(c(2600000, 1200000, 2600300, 1200200), cellsize = 100)
  grid[[1]][] <- 1
  stats::setNames(grid, name)
}

test_that("legacy_pollutant reproduces the old short codes", {
  expect_equal(legacy_pollutant("ch.bafu.luftreinhaltung-feinstaub_pm2_5"), "pm25")
  expect_equal(legacy_pollutant("ch.bafu.luftreinhaltung-feinstaub_pm10"), "pm10")
  expect_equal(legacy_pollutant("ch.bafu.luftreinhaltung-ozon"), "mp98")
  expect_equal(legacy_pollutant("ch.bafu.luftreinhaltung-stickstoffdioxid"), "no2")
  expect_equal(legacy_pollutant("ch.bafu.luftreinhaltung-stickstoff_kritischer_eintrag"), "ndep_exmax")
  expect_true(is.na(legacy_pollutant("ch.bafu.something_else")))
})

test_that("read_bafu_raster_data warns as deprecated", {
  local_mocked_bindings(
    get_geo_admin_assets = function(...) tibble::tibble(collection = "x"),
    read_collection_rasters = function(...) {
      tibble::tibble(year = 2020L, stars = list(fake_raster()))
    }
  )

  expect_snapshot(
    x <- read_bafu_raster_data("ch.bafu.luftreinhaltung-feinstaub_pm2_5", 2020, bbox_zh_lv95)
  )
})

test_that("read_bafu_raster_data keeps the old nested return shape", {
  local_mocked_bindings(
    get_geo_admin_assets = function(...) tibble::tibble(collection = "x"),
    read_collection_rasters = function(...) {
      tibble::tibble(year = c(2020L, 2021L), stars = list(fake_raster(), fake_raster()))
    }
  )

  out <- suppressWarnings(
    read_bafu_raster_data("ch.bafu.luftreinhaltung-feinstaub_pm2_5", 2020:2021, bbox_zh_lv95)
  )

  # list(year -> list(pollutant -> stars)), exactly as before
  expect_type(out, "list")
  expect_named(out, c("2020", "2021"))
  expect_named(out[["2020"]], "pm25")
  expect_s3_class(out[["2020"]][["pm25"]], "stars")
})

test_that("legacy_collection_id accepts an id or a browser url", {
  # ressources.csv stores the browser url, not the bare id
  expect_equal(
    legacy_collection_id(
      "https://data.geo.admin.ch/browser/index.html#/collections/ch.bafu.luftreinhaltung-ozon"
    ),
    "ch.bafu.luftreinhaltung-ozon"
  )
  expect_equal(
    legacy_collection_id("https://data.geo.admin.ch/api/stac/v0.9/collections/ch.bafu.test_x"),
    "ch.bafu.test_x"
  )
  expect_equal(legacy_collection_id("ch.bafu.luftreinhaltung-ozon"), "ch.bafu.luftreinhaltung-ozon")
  # anything unrecognisable is passed through rather than turned into NA
  expect_equal(legacy_collection_id("something-else"), "something-else")
})

test_that("read_statpop_raster_data returns RELI and population", {
  local_mocked_bindings(
    get_geo_admin_assets = function(...) tibble::tibble(collection = "x"),
    read_statpop_ha = function(...) {
      raster <- c(fake_raster("RELI"), fake_raster("BBTOT"))
      tibble::tibble(year = 2020L, stars = list(raster))
    }
  )

  out <- suppressWarnings(read_statpop_raster_data(2020, boundary = bbox_zh_lv95))

  expect_s3_class(out, "stars")
  expect_named(out, c("RELI", "population"))
})

test_that("read_statpop_raster_data aborts when the year is unavailable", {
  local_mocked_bindings(
    get_geo_admin_assets = function(...) tibble::tibble(collection = "x"),
    read_statpop_ha = function(...) tibble::tibble(year = integer(), stars = list())
  )

  expect_error(
    suppressWarnings(read_statpop_raster_data(1800, boundary = bbox_zh_lv95)),
    "No STATPOP data"
  )
})

test_that("get_geo_admin_metadata returns urls filtered by asset name", {
  local_mocked_bindings(
    get_geo_admin_assets = function(...) {
      tibble::tibble(
        asset = c("a_2056.tif", "b_2056.csv"),
        href = c("https://x/a.tif", "https://x/b.csv")
      )
    }
  )

  out <- suppressWarnings(get_geo_admin_metadata("ch.test"))

  expect_equal(out, "https://x/a.tif")
})

test_that("get_bfs_statpop_metadata resolves the hectare grid asset", {
  local_mocked_bindings(
    get_geo_admin_assets = function(...) {
      tibble::tibble(
        collection = "ch.bfs.statistik-bevoelkerung_haushalte",
        item = "i2020", year = 2020L,
        asset = c("x_ha_2056.parquet", "x_noloc_2056.csv"),
        format = c("parquet", "csv"),
        href = c("https://x/ha.parquet", "https://x/noloc.csv")
      )
    }
  )

  out <- suppressWarnings(get_bfs_statpop_metadata(2020))

  expect_equal(out, "https://x/ha.parquet")
})

test_that("download_file writes to the requested directory", {
  target <- withr::local_tempdir()
  local_mocked_bindings(fetch_to_file = function(href, path, ...) writeLines("a;b", path))

  out <- suppressWarnings(download_file("https://x/a.csv", target, ".csv"))

  expect_true(file.exists(out))
  expect_equal(normalizePath(dirname(out)), normalizePath(target))
})

test_that("download_zip extracts the matching members", {
  dir <- withr::local_tempdir()
  source_zip <- file.path(dir, "source.zip")
  writeLines(c("a;b"), file.path(dir, "wanted.csv"))
  writeLines(c("x"), file.path(dir, "other.txt"))
  withr::with_dir(dir, utils::zip(source_zip, c("wanted.csv", "other.txt"), flags = "-q"))
  skip_if_not(file.exists(source_zip), "zip utility unavailable")

  target <- withr::local_tempdir()
  local_mocked_bindings(
    fetch_to_file = function(href, path, ...) file.copy(source_zip, path, overwrite = TRUE)
  )

  out <- suppressWarnings(download_zip("https://x/a.zip", target, file_filter = "\\.csv$"))

  expect_equal(basename(out), "wanted.csv")
  expect_true(file.exists(file.path(target, "wanted.csv")))
  expect_false(file.exists(file.path(target, "other.txt")))
})
