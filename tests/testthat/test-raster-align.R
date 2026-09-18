year_rows <- function(years) {
  tibble::tibble(collection = "ch.test", label = "test", year = as.integer(years))
}

test_that("select_year_sources takes the exact year when it exists", {
  out <- select_year_sources(year_rows(c(2018, 2020, 2022)), 2020, "exact", Inf)

  expect_equal(out$status, "same_year")
  expect_equal(out$rows$year, 2020L)
  expect_equal(out$weights, 1)
})

test_that("select_year_sources returns NULL when 'exact' cannot be satisfied", {
  expect_null(select_year_sources(year_rows(c(2018, 2022)), 2020, "exact", Inf))
})

test_that("select_year_sources picks the nearest year", {
  out <- select_year_sources(year_rows(c(2015, 2021)), 2020, "nearest", Inf)

  expect_equal(out$status, "nearest_year")
  expect_equal(out$rows$year, 2021L)
})

test_that("select_year_sources prefers the earlier year on a tie", {
  out <- select_year_sources(year_rows(c(2019, 2021)), 2020, "nearest", Inf)

  expect_equal(out$rows$year, 2019L)
})

test_that("select_year_sources respects max_year_diff", {
  expect_null(select_year_sources(year_rows(c(2010, 2030)), 2020, "nearest", 5))
  expect_equal(
    select_year_sources(year_rows(c(2010, 2030)), 2020, "nearest", 10)$rows$year,
    2010L
  )
})

test_that("select_year_sources interpolates linearly between two years", {
  out <- select_year_sources(year_rows(c(2000, 2005)), 2002, "linear", Inf)

  expect_equal(out$status, "interpolated")
  expect_equal(out$rows$year, c(2000L, 2005L))
  expect_equal(out$weights, c(0.6, 0.4))
  expect_equal(sum(out$weights), 1)
})

test_that("select_year_sources never extrapolates beyond the series", {
  # 2025 lies after the last model year, so linear falls back to nearest
  out <- select_year_sources(year_rows(c(2000, 2005, 2020)), 2025, "linear", Inf)

  expect_equal(out$status, "nearest_year")
  expect_equal(out$rows$year, 2020L)
  expect_equal(out$weights, 1)

  before <- select_year_sources(year_rows(c(2000, 2005)), 1990, "linear", Inf)
  expect_equal(before$status, "nearest_year")
  expect_equal(before$rows$year, 2000L)
})

test_that("select_year_sources weights are proportional to the distance", {
  out <- select_year_sources(year_rows(c(2010, 2020)), 2018, "linear", Inf)

  expect_equal(out$weights, c(0.2, 0.8))
})

test_that("method_for resolves a single unnamed value for every collection", {
  expect_equal(method_for("ch.anything", "sum", "average"), "sum")
})

test_that("method_for looks a named vector up by collection", {
  methods <- c("ch.a" = "sum", "ch.b" = "near")

  expect_equal(method_for("ch.a", methods, "average"), "sum")
  expect_equal(method_for("ch.b", methods, "average"), "near")
  expect_equal(method_for("ch.unknown", methods, "average"), "average")
})

test_that("resampling_methods reads the spec defaults", {
  methods <- resampling_methods()

  expect_type(methods, "character")
  expect_equal(methods[["ch.bfs.statistik-bevoelkerung_haushalte"]], "sum")
  expect_equal(methods[["ch.bafu.luftreinhaltung-feinstaub_pm2_5"]], "average")
})

test_that("make_reference_grid rounds the extent outwards onto the cell size", {
  grid <- make_reference_grid(c(2600050, 1200050, 2600350, 1200250), cellsize = 100)

  bb <- sf::st_bbox(grid)
  expect_equal(unname(bb[["xmin"]]), 2600000)
  expect_equal(unname(bb[["ymin"]]), 1200000)
  expect_equal(unname(bb[["xmax"]]), 2600400)
  expect_equal(unname(bb[["ymax"]]), 1200300)
  expect_equal(unname(dim(grid)), c(4L, 3L))
})

test_that("make_reference_grid aligns on the statpop grid for common cell sizes", {
  for (cellsize in c(100, 200, 500, 1000)) {
    grid <- make_reference_grid(bbox_zh_lv95, cellsize = cellsize)
    bb <- sf::st_bbox(grid)
    expect_equal(bb[["xmin"]] %% cellsize, 0)
    expect_equal(bb[["ymin"]] %% cellsize, 0)
  }
})

test_that("make_reference_grid rejects a nonsensical cell size", {
  expect_error(make_reference_grid(bbox_zh_lv95, cellsize = 0), "positive number")
  expect_error(make_reference_grid(bbox_zh_lv95, cellsize = -100), "positive number")
  expect_error(make_reference_grid(bbox_zh_lv95, cellsize = c(100, 200)), "positive number")
})

test_that("grid_template keeps only the geometry of a raster", {
  source <- make_reference_grid(c(2600000, 1200000, 2600300, 1200200), cellsize = 100)
  source[[1]][] <- 42

  template <- grid_template(source)

  expect_equal(sf::st_bbox(template), sf::st_bbox(source))
  expect_equal(dim(template), dim(source))
  expect_true(all(is.na(template[[1]])))
})

test_that("grid_template rejects things that are not regular rasters", {
  expect_error(grid_template(42), "stars object or a path")
})

test_that("raster_grid describes resolution, size and origin", {
  grid <- make_reference_grid(c(2600000, 1200000, 2600300, 1200200), cellsize = 100)

  info <- raster_grid(grid)

  expect_equal(info$res_x, 100)
  expect_equal(info$res_y, 100)
  expect_type(info$grid, "character")
  # rasters that share a grid must share the description
  expect_equal(info$grid, raster_grid(grid)$grid)
})

test_that("raster_grid distinguishes different grids", {
  coarse <- make_reference_grid(c(2600000, 1200000, 2600400, 1200400), cellsize = 200)
  fine <- make_reference_grid(c(2600000, 1200000, 2600400, 1200400), cellsize = 100)

  expect_false(identical(raster_grid(coarse)$grid, raster_grid(fine)$grid))
})

test_that("resample_to_grid conserves the total when coarsening with method 'sum'", {
  fine <- make_reference_grid(c(2600000, 1200000, 2600400, 1200400), cellsize = 100)
  fine[[1]][] <- 1
  names(fine) <- "pop"
  coarse <- make_reference_grid(c(2600000, 1200000, 2600400, 1200400), cellsize = 200)

  out <- resample_to_grid(fine, coarse, method = "sum")

  expect_equal(sum(out$pop, na.rm = TRUE), sum(fine$pop, na.rm = TRUE))
  expect_equal(unname(dim(out)), c(2L, 2L))
})

test_that("resample_to_grid averages concentrations when coarsening", {
  fine <- make_reference_grid(c(2600000, 1200000, 2600200, 1200200), cellsize = 100)
  fine[[1]][] <- c(10, 20, 30, 40)
  names(fine) <- "pm2_5"
  coarse <- make_reference_grid(c(2600000, 1200000, 2600200, 1200200), cellsize = 200)

  out <- resample_to_grid(fine, coarse, method = "average")

  expect_equal(unname(dim(out)), c(1L, 1L))
  expect_equal(as.numeric(out$pm2_5), mean(c(10, 20, 30, 40)))
})

test_that("resample_to_grid keeps every attribute", {
  source <- make_reference_grid(c(2600000, 1200000, 2600200, 1200200), cellsize = 100)
  source <- c(
    stats::setNames(source, "a"),
    stats::setNames(source, "b")
  )
  source$a[] <- 1
  source$b[] <- 2
  target <- make_reference_grid(c(2600000, 1200000, 2600200, 1200200), cellsize = 200)

  out <- resample_to_grid(source, target, method = "average")

  expect_named(out, c("a", "b"))
  expect_equal(as.numeric(out$a), 1)
  expect_equal(as.numeric(out$b), 2)
})

test_that("resample_to_grid does not invent data beyond the source extent", {
  source <- make_reference_grid(c(2600000, 1200000, 2600200, 1200200), cellsize = 100)
  source[[1]][] <- 5
  names(source) <- "value"
  # target reaches well beyond the source
  target <- make_reference_grid(c(2600000, 1200000, 2600600, 1200600), cellsize = 100)

  out <- resample_to_grid(source, target, method = "average")

  expect_equal(sum(!is.na(out$value)), 4)
})

test_that("source_overlap_mask marks exactly the overlapping target cells", {
  source <- make_reference_grid(c(2600000, 1200000, 2600200, 1200200), cellsize = 100)
  target <- make_reference_grid(c(2600000, 1200000, 2600400, 1200400), cellsize = 100)

  mask <- source_overlap_mask(source, target)

  expect_equal(dim(mask), c(4L, 4L))
  expect_equal(sum(mask), 4)
})
