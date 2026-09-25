# A 3x2 block of hectare cells, coordinates on the lower left corner as BFS
# delivers them.
ha_table <- function() {
  tidyr::expand_grid(
    E_KOORD = c(2600000, 2600100, 2600200),
    N_KOORD = c(1200000, 1200100)
  ) |>
    dplyr::mutate(BBTOT = seq_len(dplyr::n()), OTHER = BBTOT * 10)
}

test_that("table_to_stars builds a grid of the right size and extent", {
  x <- table_to_stars(ha_table(), variables = "BBTOT", cellsize = 100)

  expect_s3_class(x, "stars")
  expect_equal(unname(dim(x)), c(3L, 2L))
  expect_equal(sf::st_crs(x), sf::st_crs(2056))

  bb <- sf::st_bbox(x)
  expect_equal(unname(bb[["xmin"]]), 2600000)
  expect_equal(unname(bb[["ymin"]]), 1200000)
  expect_equal(unname(bb[["xmax"]]), 2600300)
  expect_equal(unname(bb[["ymax"]]), 1200200)
})

test_that("table_to_stars places values in the correct cells", {
  data <- tibble::tibble(
    E_KOORD = c(2600000, 2600100),
    N_KOORD = c(1200000, 1200100),
    BBTOT = c(7, 42)
  )

  x <- table_to_stars(data, variables = "BBTOT", cellsize = 100)

  # the value belongs to the cell whose centre sits 50 m up and right
  cells <- sf::st_as_sf(x, as_points = TRUE, na.rm = FALSE)
  coords <- sf::st_coordinates(cells)
  expect_equal(cells$BBTOT[coords[, "X"] == 2600050 & coords[, "Y"] == 1200050], 7)
  expect_equal(cells$BBTOT[coords[, "X"] == 2600150 & coords[, "Y"] == 1200150], 42)
})

test_that("table_to_stars keeps every requested variable as its own attribute", {
  x <- table_to_stars(ha_table(), variables = c("BBTOT", "OTHER"), cellsize = 100)

  expect_named(x, c("BBTOT", "OTHER"))
  expect_equal(sum(x$OTHER, na.rm = TRUE), 10 * sum(x$BBTOT, na.rm = TRUE))
})

test_that("table_to_stars defaults to every numeric column except the coordinates", {
  x <- table_to_stars(ha_table(), cellsize = 100)

  expect_named(x, c("BBTOT", "OTHER"))
})

test_that("table_to_stars conserves the sum of the input", {
  data <- ha_table()

  x <- table_to_stars(data, variables = "BBTOT", cellsize = 100)

  expect_equal(sum(x$BBTOT, na.rm = TRUE), sum(data$BBTOT))
})

test_that("table_to_stars leaves gaps as NA rather than zero", {
  data <- tibble::tibble(
    E_KOORD = c(2600000, 2600200),
    N_KOORD = c(1200000, 1200000),
    BBTOT = c(1, 3)
  )

  x <- table_to_stars(data, variables = "BBTOT", cellsize = 100)

  expect_equal(unname(dim(x)), c(3L, 1L))
  expect_equal(sum(is.na(x$BBTOT)), 1)
})

test_that("table_to_stars honours anchor = 'center'", {
  centred <- tibble::tibble(E_KOORD = 2600050, N_KOORD = 1200050, BBTOT = 5)

  x <- table_to_stars(centred, variables = "BBTOT", cellsize = 100, anchor = "center")

  expect_equal(unname(sf::st_bbox(x)[["xmin"]]), 2600000)
  expect_equal(unname(sf::st_bbox(x)[["ymin"]]), 1200000)
})

test_that("table_to_stars rejects coordinates off the grid", {
  data <- tibble::tibble(
    E_KOORD = c(2600000, 2600042),
    N_KOORD = c(1200000, 1200000),
    BBTOT = c(1, 2)
  )

  expect_error(table_to_stars(data, variables = "BBTOT", cellsize = 100), "100 m grid")
})

test_that("table_to_stars rejects duplicate cells instead of silently dropping data", {
  data <- tibble::tibble(
    E_KOORD = c(2600000, 2600000),
    N_KOORD = c(1200000, 1200000),
    BBTOT = c(1, 2)
  )

  expect_error(table_to_stars(data, variables = "BBTOT", cellsize = 100), "same raster cell")
})

test_that("table_to_stars crops to a bbox and snaps it outwards onto the grid", {
  x <- table_to_stars(
    ha_table(),
    variables = "BBTOT",
    cellsize = 100,
    bbox = c(2600050, 1200050, 2600150, 1200150)
  )

  bb <- sf::st_bbox(x)
  expect_equal(unname(bb[["xmin"]]), 2600000)
  expect_equal(unname(bb[["xmax"]]), 2600200)
  expect_equal(unname(dim(x)), c(2L, 2L))
})

test_that("table_to_stars gives the same grid for different subsets of one bbox", {
  data <- ha_table()
  bbox <- c(2600000, 1200000, 2600300, 1200200)

  full <- table_to_stars(data, variables = "BBTOT", cellsize = 100, bbox = bbox)
  part <- table_to_stars(data[1:2, ], variables = "BBTOT", cellsize = 100, bbox = bbox)

  expect_equal(stars::st_dimensions(full), stars::st_dimensions(part))
})

test_that("table_to_stars requires an extent when there is no data", {
  empty <- ha_table()[0, ]

  expect_error(table_to_stars(empty, variables = "BBTOT"), "extent is undefined")
  expect_s3_class(
    table_to_stars(empty, variables = "BBTOT", bbox = c(2600000, 1200000, 2600300, 1200200)),
    "stars"
  )
})

test_that("table_to_stars names missing columns", {
  expect_error(table_to_stars(ha_table(), coords = c("X", "Y")), "X")
  expect_error(table_to_stars(ha_table(), variables = "NOPE"), "NOPE")
})

test_that("table_to_stars preserves the type of integer columns", {
  data <- dplyr::mutate(ha_table(), BBTOT = as.integer(BBTOT))

  x <- table_to_stars(data, variables = "BBTOT", cellsize = 100)

  expect_type(x$BBTOT, "integer")
})

test_that("read_asset_stars rasterises a tabular asset end to end", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(
    c("E_KOORD;N_KOORD;BBTOT", "2600000;1200000;10", "2600100;1200000;20"),
    path
  )
  local_mocked_bindings(download_geo_admin_asset = function(asset, ...) path)
  asset <- tibble::tibble(
    asset = "x_ha_2056.csv", href = "https://x/x_ha_2056.csv",
    format = "csv", `proj:epsg` = 2056L
  )

  x <- read_asset_stars(asset, variables = "BBTOT", cellsize = 100)

  expect_s3_class(x, "stars")
  expect_named(x, "BBTOT")
  expect_equal(sum(x$BBTOT, na.rm = TRUE), 30)
  expect_equal(sf::st_crs(x), sf::st_crs(2056))
})

test_that("read_asset_stars rejects formats that are not rasters", {
  asset <- tibble::tibble(asset = "x.gpkg", href = "https://x/x.gpkg", format = "gpkg")

  expect_error(read_asset_stars(asset), "cannot be read as a raster")
  expect_error(read_asset_stars(asset), "sf::read_sf")
})

test_that("read_asset_stars refuses an asset in the wrong crs", {
  asset <- tibble::tibble(
    asset = "x.tif", href = "https://x/x.tif", format = "tif", `proj:epsg` = 21781L
  )

  expect_error(read_asset_stars(asset, crs = 2056), "EPSG:21781")
})

# A small GeoTIFF whose crs carries no EPSG code, like the BAFU PM10 maps 1998-2001: the proj string of
# `epsg` without the datum shift (+towgs84).
tif_without_epsg <- function(epsg = 2056, env = parent.frame()) {
  proj <- sub(" [+]towgs84=[^ ]+", "", sf::st_crs(epsg)$proj4string)
  x <- stars::st_as_stars(
    sf::st_bbox(c(xmin = 2600000, ymin = 1200000, xmax = 2600400, ymax = 1200400)),
    dx = 100, values = 1
  )
  x <- sf::st_set_crs(x, sf::st_crs(proj))
  path <- withr::local_tempfile(fileext = ".tif", .local_envir = env)
  stars::write_stars(x, path)
  path
}

tif_asset <- function(epsg = 2056L) {
  asset <- tibble::tibble(
    asset = "x_2056.tif", href = "https://x/x_2056.tif", format = "tif", compression = "deflate"
  )
  if (!is.null(epsg)) asset[["proj:epsg"]] <- epsg
  asset
}

test_that("the fixture reproduces a GeoTIFF without EPSG code", {
  crs <- sf::st_crs(stars::read_stars(tif_without_epsg(), proxy = TRUE))

  expect_true(is.na(crs$epsg))
  expect_false(crs == sf::st_crs(2056))
})

test_that("read_asset_stars reads a GeoTIFF without EPSG code as EPSG:2056 if the STAC metadata say so", {
  path <- tif_without_epsg()
  local_mocked_bindings(download_geo_admin_asset = function(asset, ...) path)

  expect_message(x <- read_asset_stars(tif_asset(), crs = 2056), "no EPSG code")

  expect_true(sf::st_crs(x) == sf::st_crs(2056))
  expect_equal(dim(x), c(x = 4, y = 4))
})

test_that("read_asset_stars refuses a GeoTIFF without EPSG code in another projection", {
  path <- tif_without_epsg(21781)
  local_mocked_bindings(download_geo_admin_asset = function(asset, ...) path)

  expect_error(read_asset_stars(tif_asset(), crs = 2056), "is not in EPSG:2056")
})

test_that("read_asset_stars refuses a GeoTIFF without EPSG code if the STAC metadata name no crs", {
  path <- tif_without_epsg()
  local_mocked_bindings(download_geo_admin_asset = function(asset, ...) path)

  expect_error(read_asset_stars(tif_asset(epsg = NULL), crs = 2056), "is not in EPSG:2056")
})

test_that("read_asset_stars reads a GeoTIFF in EPSG:2056 without a message", {
  x <- stars::st_as_stars(sf::st_bbox(c(xmin = 2600000, ymin = 1200000, xmax = 2600400, ymax = 1200400),
                                      crs = sf::st_crs(2056)), dx = 100, values = 1)
  path <- withr::local_tempfile(fileext = ".tif")
  stars::write_stars(x, path)
  local_mocked_bindings(download_geo_admin_asset = function(asset, ...) path)

  expect_no_message(y <- read_asset_stars(tif_asset(), crs = 2056))
  expect_true(sf::st_crs(y) == sf::st_crs(2056))
})

test_that("read_asset_stars validates that exactly one asset was passed", {
  assets <- tibble::tibble(
    asset = c("a.csv", "b.csv"), href = c("https://x/a", "https://x/b"), format = "csv"
  )

  expect_error(read_asset_stars(assets), "exactly one row")
})

test_that("local_quiet_vsicurl scopes the GDAL setting to its caller", {
  name <- "CPL_VSIL_CURL_ALLOWED_EXTENSIONS"
  withr::local_envvar(stats::setNames(list(NA), name))

  inside <- local({
    f <- function() {
      local_quiet_vsicurl()
      Sys.getenv(name, unset = NA_character_)
    }
    f()
  })

  expect_equal(inside, ".tif")
  # restored when the calling function returned
  expect_true(is.na(Sys.getenv(name, unset = NA_character_)))
})

test_that("local_quiet_vsicurl restores a pre-existing value", {
  name <- "CPL_VSIL_CURL_ALLOWED_EXTENSIONS"
  withr::local_envvar(stats::setNames(list(".vrt"), name))

  f <- function() {
    local_quiet_vsicurl()
    Sys.getenv(name)
  }

  expect_equal(f(), ".tif")
  expect_equal(Sys.getenv(name), ".vrt")
})
