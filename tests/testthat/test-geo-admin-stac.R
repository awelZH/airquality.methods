test_that("asset_format normalises extensions and strips compression", {
  expect_equal(asset_format("https://x/y/file.tif"), "tif")
  expect_equal(asset_format("https://x/y/file.tiff"), "tif")
  expect_equal(asset_format("https://x/y/file.geotiff"), "tif")
  expect_equal(asset_format("https://x/y/file.TIF"), "tif")
  expect_equal(asset_format("https://x/y/statpop.csv.zip"), "csv")
  expect_equal(asset_format("https://x/y/statpop.parquet"), "parquet")
  expect_equal(asset_format("https://x/y/data.csv.gz"), "csv")
})

test_that("asset_format ignores query strings and fragments", {
  expect_equal(asset_format("https://x/y/file.tif?token=abc"), "tif")
  expect_equal(asset_format("https://x/y/file.csv.zip#frag"), "csv")
})

test_that("asset_compression reports the outer archive only", {
  expect_equal(asset_compression("https://x/y/a.csv.zip"), "zip")
  expect_equal(asset_compression("https://x/y/a.csv.gz"), "gz")
  expect_true(is.na(asset_compression("https://x/y/a.tif")))
  expect_equal(asset_compression("https://x/y/a.csv.zip?t=1"), "zip")
})

test_that("item_year prefers an unambiguous year in the item id", {
  year <- item_year(
    item = "ch.bafu.pm2_5_2020",
    datetime = "2019-01-01T00:00:00Z",
    start_datetime = NA_character_,
    end_datetime = NA_character_
  )

  expect_equal(year, 2020L)
})

test_that("item_year ignores four-digit numbers that cannot be years", {
  # 2056 is the EPSG code, not a year; the datetime has to win
  year <- item_year(
    item = "pm2_5_2056",
    datetime = "2018-01-01T00:00:00Z",
    start_datetime = NA_character_,
    end_datetime = NA_character_
  )

  expect_equal(year, 2018L)
})

test_that("item_year falls back to datetime, end_datetime, start_datetime in order", {
  expect_equal(
    item_year("no_year_here", NA_character_, NA_character_, "2021-12-31T00:00:00Z"),
    2021L
  )
  expect_equal(
    item_year("no_year_here", NA_character_, "2017-01-01T00:00:00Z", NA_character_),
    2017L
  )
})

test_that("item_year warns when items end up without a year", {
  expect_warning(
    year <- item_year("no_year_here", NA_character_, NA_character_, NA_character_),
    "without a year"
  )
  expect_true(is.na(year))
})

test_that("item_year is vectorised and keeps the input order", {
  suppressWarnings(
    years <- item_year(
      item = c("a_2020", "b_2021", "ambiguous_2019_2020"),
      datetime = c(NA, NA, "2022-01-01T00:00:00Z"),
      start_datetime = rep(NA_character_, 3),
      end_datetime = rep(NA_character_, 3)
    )
  )

  # the ambiguous id is not trusted, the datetime is used instead
  expect_equal(years, c(2020L, 2021L, 2022L))
})

test_that("collection_label shortens a collection id", {
  expect_equal(
    collection_label("ch.bafu.luftreinhaltung-feinstaub_pm2_5"),
    "luftreinhaltung_feinstaub_pm2_5"
  )
  expect_equal(
    collection_label("ch.bfs.statistik-bevoelkerung_haushalte"),
    "statistik_bevoelkerung_haushalte"
  )
})

test_that("geo_admin_items_request builds a v1 items url", {
  req <- geo_admin_items_request("ch.bafu.test", limit = 50)

  expect_s3_class(req, "httr2_request")
  expect_match(req$url, "/api/stac/v1/collections/ch.bafu.test/items")
  expect_match(req$url, "limit=50")
})

test_that("geo_admin_items_request converts an LV95 bbox to WGS84", {
  req <- geo_admin_items_request("ch.bafu.test", bbox = bbox_zh_lv95)

  bbox_param <- httr2::url_parse(req$url)$query$bbox
  values <- as.numeric(strsplit(bbox_param, ",", fixed = TRUE)[[1]])

  expect_length(values, 4)
  # canton of Zurich in degrees
  expect_true(values[[1]] > 8 && values[[1]] < 9)
  expect_true(values[[2]] > 47 && values[[2]] < 48)
  expect_true(values[[3]] > values[[1]])
  expect_true(values[[4]] > values[[2]])
})

test_that("geo_admin_items_request rejects a non-string collection", {
  expect_error(geo_admin_items_request(c("a", "b")), "single string")
  expect_error(geo_admin_items_request("ch.x", stac_version = "v2"), "v1")
})

test_that("parse_item_assets flattens assets into one row each", {
  item <- list(
    id = "item_2020",
    collection = "ch.test",
    properties = list(datetime = "2020-01-01T00:00:00Z"),
    assets = list(
      "a_2056.tif" = list(href = "https://x/a_2056.tif", "proj:epsg" = 2056L, type = "image/tiff"),
      "a_2056.csv" = list(href = "https://x/a_2056.csv", "proj:epsg" = 2056L)
    )
  )

  out <- parse_item_assets(item)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 2)
  expect_equal(out$asset, c("a_2056.tif", "a_2056.csv"))
  expect_equal(unique(out$item), "item_2020")
  expect_equal(unique(out$collection), "ch.test")
  expect_equal(unique(out$datetime), "2020-01-01T00:00:00Z")
})

test_that("parse_item_assets drops non-scalar asset fields and handles empty items", {
  item <- list(
    id = "i", collection = "c", properties = list(),
    assets = list(a = list(href = "https://x/a.tif", roles = list("data", "cloud-optimized")))
  )

  out <- parse_item_assets(item)

  expect_equal(nrow(out), 1)
  expect_false("roles" %in% names(out))
  expect_true(is.na(out$datetime))

  expect_null(parse_item_assets(list(id = "i", assets = list())))
})

test_that("select_preferred_format keeps the best available format per item", {
  assets <- tibble::tibble(
    item = c("i1", "i1", "i2", "i2"),
    asset = c("a.parquet", "a.csv", "b.csv", "b.tif"),
    format = c("parquet", "csv", "csv", "tif")
  )

  out <- select_preferred_format(assets, formats = c("parquet", "csv", "tif"))

  expect_equal(nrow(out), 2)
  expect_equal(out$format, c("parquet", "csv"))
})

test_that("select_preferred_format warns about groups it has to drop", {
  assets <- tibble::tibble(
    item = c("i1", "i2"),
    asset = c("a.parquet", "b.gpkg"),
    format = c("parquet", "gpkg")
  )

  expect_warning(out <- select_preferred_format(assets, formats = "parquet"), "i2")
  expect_equal(nrow(out), 1)
})

test_that("resolve_assets applies pattern, epsg, format and year in order", {
  assets <- tibble::tibble(
    collection = "ch.test",
    item = c("i2020", "i2020", "i2021", "i2021"),
    year = c(2020L, 2020L, 2021L, 2021L),
    asset = c("x_ha_2056.parquet", "x_noloc_2056.csv", "x_ha_2056.parquet", "x_ha_21781.parquet"),
    format = c("parquet", "csv", "parquet", "parquet"),
    `proj:epsg` = c(2056L, 2056L, 2056L, 21781L)
  )

  out <- resolve_assets(assets, pattern = "_ha_", formats = c("parquet", "csv"), epsg = 2056)

  expect_equal(nrow(out), 2)
  expect_equal(out$year, c(2020L, 2021L))
  expect_true(all(grepl("_ha_2056", out$asset)))
})

test_that("resolve_assets filters to the requested years and reports the rest", {
  assets <- tibble::tibble(
    collection = "ch.test",
    item = c("i2020", "i2021"),
    year = c(2020L, 2021L),
    asset = c("a_2056.tif", "b_2056.tif"),
    format = c("tif", "tif")
  )

  expect_message(
    out <- resolve_assets(assets, formats = "tif", years = c(2021, 2099)),
    "2099"
  )
  expect_equal(out$year, 2021L)
})

test_that("resolve_assets aborts when nothing matches", {
  assets <- tibble::tibble(
    collection = "ch.test", item = "i", year = 2020L,
    asset = "a.gpkg", format = "gpkg"
  )

  expect_error(resolve_assets(assets, formats = "tif"), "no asset")
})

test_that("resolve_assets aborts on genuinely ambiguous items", {
  assets <- tibble::tibble(
    collection = "ch.test",
    item = c("i", "i"),
    year = c(2020L, 2020L),
    asset = c("a_variant1.tif", "a_variant2.tif"),
    format = c("tif", "tif")
  )

  expect_error(resolve_assets(assets, formats = "tif", epsg = NULL), "ambiguous")
})

test_that("check_single_asset requires exactly one row with the core columns", {
  good <- tibble::tibble(asset = "a.tif", href = "https://x/a.tif", format = "tif")

  expect_equal(check_single_asset(good), good)
  expect_error(check_single_asset(good[0, ]), "exactly one row")
  expect_error(check_single_asset(dplyr::bind_rows(good, good)), "exactly one row")
  expect_error(check_single_asset(list(asset = "a")), "exactly one row")
})

test_that("check_asset_crs compares the declared epsg with the expected one", {
  asset <- tibble::tibble(asset = "a.tif", href = "h", format = "tif", `proj:epsg` = 2056L)

  expect_true(check_asset_crs(asset, 2056))
  expect_error(check_asset_crs(asset, 21781), "EPSG:2056")

  # assets without a declared epsg pass through
  expect_true(check_asset_crs(tibble::tibble(asset = "a", `proj:epsg` = NA_integer_), 2056))
})
