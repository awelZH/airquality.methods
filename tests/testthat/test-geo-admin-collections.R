test_that("collection_spec keeps reader, label, resampling and fixed arguments", {
  spec <- collection_spec(read_collection_rasters, label = "no2", name = "no2", formats = "tif")

  expect_type(spec$reader, "closure")
  expect_equal(spec$label, "no2")
  expect_equal(spec$resampling, "average")
  expect_equal(spec$args, list(name = "no2", formats = "tif"))
})

test_that("collection_spec validates its arguments", {
  expect_error(collection_spec("not a function", "x"), "must be a function")
  expect_error(collection_spec(read_collection_rasters, c("a", "b")), "single string")
  expect_error(collection_spec(read_collection_rasters, "x", resampling = c("a", "b")), "single string")
})

test_that("geo_admin_specs covers every collection used productively", {
  specs <- geo_admin_specs()

  expect_contains(
    names(specs),
    c(
      "ch.bfs.statistik-bevoelkerung_haushalte",
      "ch.bafu.luftreinhaltung-stickstoffdioxid",
      "ch.bafu.luftreinhaltung-feinstaub_pm10",
      "ch.bafu.luftreinhaltung-feinstaub_pm2_5",
      "ch.bafu.luftreinhaltung-ozon",
      "ch.bafu.luftreinhaltung-stickstoffdeposition",
      "ch.bafu.luftreinhaltung-stickstoff_kritischer_eintrag"
    )
  )
})

test_that("geo_admin_specs resolves its readers when called, not when built", {
  specs <- geo_admin_specs()

  expect_identical(specs[["ch.bfs.statistik-bevoelkerung_haushalte"]]$reader, read_statpop_ha)
  expect_identical(specs[["ch.bafu.luftreinhaltung-ozon"]]$reader, read_collection_rasters)
})

test_that("geo_admin_specs gives every collection a unique attribute name", {
  names_used <- geo_admin_specs() |>
    purrr::map_chr(\(spec) spec$args$name %||% spec$args$variables %||% spec$label)

  expect_equal(anyDuplicated(names_used), 0L)
})

test_that("geo_admin_specs sums counts and averages everything else", {
  specs <- geo_admin_specs()

  expect_equal(specs[["ch.bfs.statistik-bevoelkerung_haushalte"]]$resampling, "sum")
  expect_true(all(
    purrr::map_chr(specs[setdiff(names(specs), "ch.bfs.statistik-bevoelkerung_haushalte")], "resampling") ==
      "average"
  ))
})

test_that("get_collection_spec returns the registered spec", {
  spec <- get_collection_spec("ch.bafu.luftreinhaltung-feinstaub_pm2_5")

  expect_equal(spec$label, "pm2_5")
})

test_that("get_collection_spec falls back to the generic reader and says so", {
  expect_message(spec <- get_collection_spec("ch.foo.bar-baz"), "No spec")

  expect_identical(spec$reader, read_collection_rasters)
  expect_equal(spec$label, "bar_baz")
  expect_equal(spec$args$name, "bar_baz")
})

test_that("read_geo_admin validates the collections argument", {
  expect_error(read_geo_admin(character(0)), "at least one collection id")
  expect_error(read_geo_admin(42), "at least one collection id")
})

test_that("read_collection_rasters returns a typed empty tibble when nothing matches", {
  assets <- tibble::tibble(
    collection = "ch.test", item = "i", year = 2020L,
    asset = "a.tif", format = "tif", href = "https://x/a.tif"
  )

  expect_message(out <- read_collection_rasters(assets, years = 2099), "not available")

  expect_equal(nrow(out), 0)
  expect_named(out, c("item", "year", "format", "href", "stars"))
  expect_type(out$year, "integer")
})

test_that("read_geo_admin returns a typed empty tibble when nothing is found", {
  local_mocked_bindings(
    get_geo_admin_assets = function(collection, ...) {
      tibble::tibble(
        collection = collection, item = "i", year = 2020L,
        asset = "a.tif", format = "tif", href = "https://x/a.tif"
      )
    }
  )

  expect_message(
    expect_warning(out <- read_geo_admin("ch.bafu.luftreinhaltung-ozon", years = 1995), "No data found"),
    "not available"
  )

  # a caller that filters or selects must get an empty result, not
  # "column `year` not found"
  expect_equal(nrow(out), 0)
  expect_contains(names(out), c("collection", "label", "year", "grid", "stars"))
  expect_type(out$year, "integer")
  expect_equal(nrow(dplyr::filter(out, .data$year > 2000)), 0)
})

test_that("read_collection_rasters says where each year is read from", {
  local_mocked_bindings(
    read_asset_stars = function(asset, ...) stars::st_as_stars(matrix(1, 1, 1))
  )
  assets <- tibble::tibble(
    collection = "ch.test", item = "ch.test-2020", year = 2020L,
    asset = "a.tif", format = "tif", href = "https://x/a.tif", compression = NA_character_
  )

  expect_message(
    read_collection_rasters(assets, years = 2020, bbox = NULL),
    "ch.test-2020.*streamed from the web"
  )
})
