# The HTTP layer, against recorded STAC responses. No network.

json_response <- function(body, url = "https://data.geo.admin.ch/api/stac/v1/x") {
  httr2::response(
    status_code = 200,
    url = url,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(jsonlite_encode(body))
  )
}

# jsonlite is not a dependency; httr2 brings it, so use it through httr2's own
# requirement rather than declaring one of our own.
jsonlite_encode <- function(x) {
  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")
}

stac_item <- function(id, year, assets) {
  list(
    id = id,
    collection = "ch.test.collection",
    properties = list(datetime = paste0(year, "-01-01T00:00:00Z")),
    assets = assets
  )
}

tif_asset <- function(name, epsg = 2056) {
  list(
    href = paste0("https://data.geo.admin.ch/ch.test.collection/", name),
    `proj:epsg` = epsg,
    `file:checksum` = "1220abc",
    type = "image/tiff; application=geotiff"
  )
}

page <- function(items, next_href = NULL) {
  links <- if (is.null(next_href)) list() else list(list(rel = "next", href = next_href))
  list(type = "FeatureCollection", features = items, links = links)
}

test_that("get_geo_admin_assets parses one page into one row per asset", {
  body <- page(list(
    stac_item("item_2020", 2020, list(
      "test_2020_2056.tif" = tif_asset("test_2020_2056.tif"),
      "test_2020_2056.csv" = list(href = "https://x/test_2020_2056.csv")
    ))
  ))

  assets <- httr2::with_mocked_responses(
    function(req) json_response(body),
    get_geo_admin_assets("ch.test.collection")
  )

  expect_equal(nrow(assets), 2)
  expect_equal(assets$asset, c("test_2020_2056.tif", "test_2020_2056.csv"))
  expect_equal(assets$format, c("tif", "csv"))
  expect_equal(unique(assets$year), 2020L)
  expect_equal(unique(assets$collection), "ch.test.collection")
  expect_equal(assets$`file:checksum`[[1]], "1220abc")
})

test_that("get_geo_admin_assets follows the rel = 'next' link across pages", {
  first <- page(
    list(stac_item("item_2020", 2020, list("a_2020_2056.tif" = tif_asset("a_2020_2056.tif")))),
    next_href = "https://data.geo.admin.ch/api/stac/v1/x?cursor=2"
  )
  second <- page(
    list(stac_item("item_2021", 2021, list("a_2021_2056.tif" = tif_asset("a_2021_2056.tif"))))
  )

  calls <- 0L
  assets <- httr2::with_mocked_responses(
    function(req) {
      calls <<- calls + 1L
      json_response(if (calls == 1L) first else second)
    },
    get_geo_admin_assets("ch.test.collection")
  )

  expect_equal(calls, 2L)
  expect_equal(nrow(assets), 2)
  expect_equal(assets$year, c(2020L, 2021L))
})

test_that("get_geo_admin_assets warns when max_pages cuts the listing short", {
  endless <- page(
    list(stac_item("item_2020", 2020, list("a_2020_2056.tif" = tif_asset("a_2020_2056.tif")))),
    next_href = "https://data.geo.admin.ch/api/stac/v1/x?cursor=next"
  )

  expect_warning(
    httr2::with_mocked_responses(
      function(req) json_response(endless),
      get_geo_admin_assets("ch.test.collection", max_pages = 2)
    ),
    "incomplete"
  )
})

test_that("get_geo_admin_assets returns an empty tibble for an empty collection", {
  assets <- httr2::with_mocked_responses(
    function(req) json_response(page(list())),
    get_geo_admin_assets("ch.test.collection")
  )

  expect_equal(nrow(assets), 0)
})

test_that("get_geo_admin_assets survives items without assets", {
  body <- page(list(
    stac_item("empty_2019", 2019, list()),
    stac_item("item_2020", 2020, list("a_2020_2056.tif" = tif_asset("a_2020_2056.tif")))
  ))

  assets <- httr2::with_mocked_responses(
    function(req) json_response(body),
    get_geo_admin_assets("ch.test.collection")
  )

  expect_equal(nrow(assets), 1)
  expect_equal(assets$year, 2020L)
})

test_that("the whole chain resolves recorded assets down to one file per year", {
  body <- page(list(
    stac_item("item_2020", 2020, list(
      "test_2020_2056.tif" = tif_asset("test_2020_2056.tif"),
      "test_2020_21781.tif" = tif_asset("test_2020_21781.tif", epsg = 21781)
    )),
    stac_item("item_2021", 2021, list(
      "test_2021_2056.tif" = tif_asset("test_2021_2056.tif")
    ))
  ))

  resolved <- httr2::with_mocked_responses(
    function(req) json_response(body),
    get_geo_admin_assets("ch.test.collection") |> resolve_assets(formats = "tif")
  )

  expect_equal(nrow(resolved), 2)
  expect_equal(resolved$year, c(2020L, 2021L))
  expect_true(all(grepl("_2056\\.tif$", resolved$asset)))
})
