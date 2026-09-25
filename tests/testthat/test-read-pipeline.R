# read_collection_rasters(), read_statpop_ha() and read_geo_admin() with the
# network boundary stubbed out.

tif_assets <- function(years = 2020:2021, collection = "ch.test.collection") {
  tibble::tibble(
    collection = collection,
    item = paste0("item_", years),
    year = as.integer(years),
    asset = paste0("test_", years, "_2056.tif"),
    format = "tif",
    href = paste0("https://x/test_", years, "_2056.tif"),
    `proj:epsg` = 2056L
  )
}

small_raster <- function(name = "value", value = 1) {
  grid <- make_reference_grid(c(2600000, 1200000, 2600200, 1200200), cellsize = 100)
  grid[[1]][] <- value
  stats::setNames(grid, name)
}

test_that("read_collection_rasters returns one row per year with provenance", {
  local_mocked_bindings(read_asset_stars = function(asset, ...) small_raster())

  out <- read_collection_rasters(tif_assets(), formats = "tif", name = "pm2_5")

  expect_equal(nrow(out), 2)
  expect_named(out, c("item", "year", "format", "href", "stars"))
  expect_equal(out$year, c(2020L, 2021L))
  expect_equal(out$href, paste0("https://x/test_", 2020:2021, "_2056.tif"))
})

test_that("read_collection_rasters names single band rasters consistently", {
  local_mocked_bindings(read_asset_stars = function(asset, ...) small_raster("band1"))

  out <- read_collection_rasters(tif_assets(), formats = "tif", name = "pm2_5")

  expect_true(all(purrr::map_lgl(out$stars, \(x) identical(names(x), "pm2_5"))))
})

test_that("read_collection_rasters derives a name from the collection when none is given", {
  local_mocked_bindings(read_asset_stars = function(asset, ...) small_raster())

  out <- read_collection_rasters(tif_assets(), formats = "tif")

  expect_equal(names(out$stars[[1]]), "collection")
})

test_that("read_collection_rasters keeps multi-band rasters as they are", {
  local_mocked_bindings(
    read_asset_stars = function(asset, ...) c(small_raster("a"), small_raster("b"))
  )

  out <- read_collection_rasters(tif_assets(), formats = "tif", name = "ignored")

  expect_named(out$stars[[1]], c("a", "b"))
})

test_that("read_geo_admin adds collection, label and grid columns", {
  local_mocked_bindings(
    get_geo_admin_assets = function(collection, ...) tif_assets(collection = collection),
    read_asset_stars = function(asset, ...) small_raster()
  )

  out <- read_geo_admin("ch.bafu.luftreinhaltung-feinstaub_pm2_5", years = 2020:2021)

  expect_equal(nrow(out), 2)
  expect_equal(unique(out$label), "pm2_5")
  expect_equal(unique(out$res_x), 100)
  expect_true(all(c("collection", "label", "year", "grid", "stars") %in% names(out)))
  expect_equal(names(out)[1:3], c("collection", "label", "year"))
})

test_that("read_geo_admin reads several collections into one table", {
  local_mocked_bindings(
    get_geo_admin_assets = function(collection, ...) tif_assets(collection = collection),
    read_asset_stars = function(asset, ...) small_raster()
  )

  out <- read_geo_admin(
    c("ch.bafu.luftreinhaltung-feinstaub_pm2_5", "ch.bafu.luftreinhaltung-ozon"),
    years = 2020
  )

  expect_equal(nrow(out), 2)
  expect_setequal(out$label, c("pm2_5", "o3_max_98p_m1"))
})

test_that("read_geo_admin deduplicates the collections it is given", {
  local_mocked_bindings(
    get_geo_admin_assets = function(collection, ...) tif_assets(collection = collection),
    read_asset_stars = function(asset, ...) small_raster()
  )

  out <- read_geo_admin(rep("ch.bafu.luftreinhaltung-ozon", 3), years = 2020)

  expect_equal(nrow(out), 1)
})

test_that("read_geo_admin stops or skips on a failing collection, as asked", {
  local_mocked_bindings(
    get_geo_admin_assets = function(collection, ...) {
      if (collection == "ch.broken") cli::cli_abort("service unavailable")
      tif_assets(collection = collection)
    },
    read_asset_stars = function(asset, ...) small_raster()
  )

  expect_error(
    read_geo_admin(c("ch.bafu.luftreinhaltung-ozon", "ch.broken"), years = 2020),
    "failed"
  )

  expect_warning(
    out <- read_geo_admin(
      c("ch.bafu.luftreinhaltung-ozon", "ch.broken"),
      years = 2020, on_error = "warn"
    ),
    "Skipping"
  )
  expect_equal(nrow(out), 1)
})

test_that("read_geo_admin warns when nothing at all could be read", {
  local_mocked_bindings(
    get_geo_admin_assets = function(collection, ...) tif_assets(collection = collection),
    read_asset_stars = function(asset, ...) small_raster()
  )

  expect_message(
    expect_warning(
      out <- read_geo_admin("ch.bafu.luftreinhaltung-ozon", years = 1800),
      "No data found"
    ),
    "not available"
  )
  expect_equal(nrow(out), 0)
})

test_that("read_statpop_ha subtracts the collector pixels and books the result", {
  assets <- dplyr::bind_rows(
    tibble::tibble(
      collection = "ch.bfs.statistik-bevoelkerung_haushalte",
      item = "item_2020", year = 2020L,
      asset = "statpop_2020_ha_2056.parquet", format = "parquet",
      href = "https://x/ha.parquet", `proj:epsg` = 2056L
    ),
    tibble::tibble(
      collection = "ch.bfs.statistik-bevoelkerung_haushalte",
      item = "item_2020", year = 2020L,
      asset = "statpop_2020_noloc_2056.csv", format = "csv",
      href = "https://x/noloc.csv", `proj:epsg` = 2056L
    )
  )

  local_mocked_bindings(
    read_asset_stars = function(asset, ...) small_raster("BBTOT", value = 10),
    download_geo_admin_asset = function(asset, ...) "irrelevant.csv",
    read_asset_table = function(path, format, columns = NULL) {
      tibble::tibble(ERHJAHR = 2020L, E_KOORD = 2600000, N_KOORD = 1200000, BBTOT = 4)
    }
  )

  out <- read_statpop_ha(assets)

  expect_equal(nrow(out), 1)
  expect_equal(out$year, 2020L)
  expect_equal(out$noloc_subtracted, 4)
  expect_equal(out$noloc_unmatched, 0)
  expect_equal(sum(out$stars[[1]]$BBTOT, na.rm = TRUE), 4 * 10 - 4)
})

test_that("read_statpop_ha can be told not to correct, and says so in the columns", {
  assets <- tibble::tibble(
    collection = "ch.bfs.statistik-bevoelkerung_haushalte",
    item = "item_2020", year = 2020L,
    asset = "statpop_2020_ha_2056.parquet", format = "parquet",
    href = "https://x/ha.parquet", `proj:epsg` = 2056L
  )
  local_mocked_bindings(read_asset_stars = function(asset, ...) small_raster("BBTOT", value = 10))

  out <- read_statpop_ha(assets, correct_noloc = FALSE)

  expect_true(is.na(out$noloc_subtracted))
  expect_null(out$noloc[[1]])
  expect_equal(sum(out$stars[[1]]$BBTOT, na.rm = TRUE), 40)
})

test_that("read_statpop_ha warns loudly when no collector pixel file is found", {
  assets <- tibble::tibble(
    collection = "ch.bfs.statistik-bevoelkerung_haushalte",
    item = "item_2020", year = 2020L,
    asset = "statpop_2020_ha_2056.parquet", format = "parquet",
    href = "https://x/ha.parquet", `proj:epsg` = 2056L
  )
  local_mocked_bindings(read_asset_stars = function(asset, ...) small_raster("BBTOT", value = 10))

  expect_warning(
    expect_warning(out <- read_statpop_ha(assets), "No asset matches"),
    "no correction applied"
  )
  expect_true(is.na(out$noloc_subtracted))
})

test_that("read_statpop_ha refuses to correct a variable it was not asked to read", {
  assets <- tibble::tibble(
    collection = "ch.bfs.statistik-bevoelkerung_haushalte",
    item = "item_2020", year = 2020L, asset = "a_ha_2056.parquet",
    format = "parquet", href = "https://x/a.parquet"
  )

  expect_error(read_statpop_ha(assets, variables = "BB11"), "must contain")
})

test_that("get_opendataswiss_metadata picks the matching resources", {
  body <- list(result = list(resources = list(
    list(download_url = "https://x/data.csv"),
    list(download_url = "https://x/data.json"),
    list(download_url = "https://x/more.csv")
  )))

  urls <- httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200, url = "https://ckan/api",
        headers = list(`Content-Type` = "application/json"),
        body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
      )
    },
    get_opendataswiss_metadata("https://ckan/api")
  )

  expect_equal(urls, c("https://x/data.csv", "https://x/more.csv"))
})

test_that("get_opendataswiss_metadata aborts when nothing matches, listing what exists", {
  body <- list(result = list(resources = list(list(download_url = "https://x/data.json"))))

  expect_error(
    httr2::with_mocked_responses(
      function(req) {
        httr2::response(
          status_code = 200, url = "https://ckan/api",
          headers = list(`Content-Type` = "application/json"),
          body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
        )
      },
      get_opendataswiss_metadata("https://ckan/api")
    ),
    "data.json"
  )
})

test_that("get_opendataswiss_resources lists the resources with their version, in API order", {
  body <- list(result = list(resources = list(
    list(download_url = "https://x/b.csv", format = "CSV", modified = "2026-02-06T14:12:28+01:00", byte_size = 28498959),
    list(download_url = "https://x/a.txt", format = "TXT", modified = "2025-12-15T14:49:10+01:00", byte_size = 2680),
    list(url = "https://x/no-download", format = "HTML"),
    list(download_url = "https://x/c.csv", format = "CSV")
  )))

  resources <- httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200, url = "https://ckan/api",
        headers = list(`Content-Type` = "application/json"),
        body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
      )
    },
    get_opendataswiss_resources("https://ckan/api")
  )

  expect_s3_class(resources, "tbl_df")
  expect_named(resources, c("download_url", "format", "modified", "byte_size"))
  expect_equal(resources$download_url, c("https://x/b.csv", "https://x/a.txt", "https://x/c.csv"))
  expect_equal(resources$modified, c("2026-02-06T14:12:28+01:00", "2025-12-15T14:49:10+01:00", NA))
  expect_equal(resources$byte_size, c(28498959, 2680, NA))
})

test_that("get_opendataswiss_resources returns an empty table for a dataset without downloads", {
  body <- list(result = list(resources = list()))

  resources <- httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200, url = "https://ckan/api",
        headers = list(`Content-Type` = "application/json"),
        body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
      )
    },
    get_opendataswiss_resources("https://ckan/api")
  )

  expect_equal(nrow(resources), 0)
  expect_named(resources, c("download_url", "format", "modified", "byte_size"))
})

test_that("read_local_csv reads the cantonal export defaults", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("a;b", "1;2"), path)

  out <- read_local_csv(path, show_col_types = FALSE)

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("a", "b"))
  expect_equal(out$a, 1)
})

test_that("get_geolion_wfs_metadata builds a GetFeature url", {
  url <- get_geolion_wfs_metadata("https://maps.zh.ch/wfs/GemZHWFS")

  expect_match(url, "service=wfs")
  expect_match(url, "request=GetFeature")
  expect_match(url, "typename=ms%3Agem_grenzen")
  expect_match(url, "srsName=EPSG%3A2056")
})

test_that("get_geolion_wfs_metadata honours type, version and crs", {
  url <- get_geolion_wfs_metadata(
    "https://maps.zh.ch/wfs/GemZHWFS",
    type = "ms:gem_seen_grenzen", version = "1.1.0", crs = 21781
  )

  expect_match(url, "typename=ms%3Agem_seen_grenzen")
  expect_match(url, "version=1.1.0")
  expect_match(url, "srsName=EPSG%3A21781")
})
