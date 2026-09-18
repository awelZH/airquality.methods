# A 3x2 hectare raster of inhabitants, lower left corner 2600000 / 1200000.
population_raster <- function(values = c(10, 20, 30, 40, 50, 60)) {
  tidyr::expand_grid(
    E_KOORD = c(2600000, 2600100, 2600200),
    N_KOORD = c(1200000, 1200100)
  ) |>
    dplyr::mutate(BBTOT = values) |>
    table_to_stars(variables = "BBTOT", cellsize = 100)
}

test_that("subtract_noloc removes the collector pixel from its own cell only", {
  x <- population_raster()
  noloc <- tibble::tibble(E_KOORD = 2600100, N_KOORD = 1200000, BBTOT = 5)

  out <- subtract_noloc(x, noloc)

  expect_equal(sum(out$stars$BBTOT, na.rm = TRUE), sum(10, 20, 30, 40, 50, 60) - 5)
  expect_equal(out$noloc$status, "subtracted")
  expect_equal(out$noloc$cell_before, 30)
  expect_equal(out$noloc$cell_after, 25)
  expect_equal(out$noloc$subtracted, 5)
})

test_that("subtract_noloc returns a log with one row per collector pixel", {
  x <- population_raster()
  noloc <- tibble::tibble(
    E_KOORD = c(2600000, 2600200),
    N_KOORD = c(1200000, 1200100),
    BBTOT = c(1, 2)
  )

  out <- subtract_noloc(x, noloc)

  expect_equal(nrow(out$noloc), 2)
  expect_named(
    out$noloc,
    c("E_KOORD", "N_KOORD", "n_records", "noloc", "cell_before", "subtracted", "cell_after", "status")
  )
})

test_that("subtract_noloc aggregates several records for the same cell", {
  x <- population_raster()
  noloc <- tibble::tibble(
    E_KOORD = c(2600000, 2600000),
    N_KOORD = c(1200000, 1200000),
    BBTOT = c(2, 3)
  )

  out <- subtract_noloc(x, noloc)

  expect_equal(nrow(out$noloc), 1)
  expect_equal(out$noloc$n_records, 2)
  expect_equal(out$noloc$noloc, 5)
  expect_equal(out$noloc$subtracted, 5)
})

test_that("subtract_noloc flags a collector pixel larger than its cell", {
  x <- population_raster()
  noloc <- tibble::tibble(E_KOORD = 2600000, N_KOORD = 1200000, BBTOT = 999)

  expect_warning(out <- subtract_noloc(x, noloc), "exceeds_cell")

  expect_equal(out$noloc$status, "exceeds_cell")
  expect_equal(out$noloc$subtracted, 10) # only what the cell actually held
  expect_equal(out$noloc$cell_after, 0)
})

test_that("subtract_noloc flags a collector pixel on an empty cell", {
  data <- tibble::tibble(
    E_KOORD = c(2600000, 2600200),
    N_KOORD = c(1200000, 1200000),
    BBTOT = c(10, 30)
  )
  x <- table_to_stars(data, variables = "BBTOT", cellsize = 100)
  noloc <- tibble::tibble(E_KOORD = 2600100, N_KOORD = 1200000, BBTOT = 5)

  expect_warning(out <- subtract_noloc(x, noloc), "cell_missing")

  expect_equal(out$noloc$status, "cell_missing")
  expect_equal(out$noloc$subtracted, 0)
  expect_equal(sum(out$stars$BBTOT, na.rm = TRUE), 40)
})

test_that("subtract_noloc flags a collector pixel outside the extent", {
  x <- population_raster()
  noloc <- tibble::tibble(E_KOORD = 2700000, N_KOORD = 1250000, BBTOT = 7)

  out <- subtract_noloc(x, noloc)

  expect_equal(out$noloc$status, "outside_extent")
  expect_equal(out$noloc$subtracted, 0)
  expect_equal(sum(out$stars$BBTOT, na.rm = TRUE), 210)
})

test_that("subtract_noloc keeps the accounting invariant", {
  x <- population_raster()
  noloc <- tibble::tibble(
    E_KOORD = c(2600000, 2600100, 2700000),
    N_KOORD = c(1200000, 1200100, 1250000),
    BBTOT = c(5, 7, 11)
  )

  out <- subtract_noloc(x, noloc)

  # every inhabitant of the noloc file is either subtracted or accounted unmatched
  subtracted <- sum(out$noloc$subtracted)
  unmatched <- sum(out$noloc$noloc) - subtracted
  expect_equal(subtracted + unmatched, sum(noloc$BBTOT))
  expect_equal(sum(out$stars$BBTOT, na.rm = TRUE), 210 - subtracted)
})

test_that("subtract_noloc works with cell centre coordinates", {
  x <- population_raster()
  noloc <- tibble::tibble(E_KOORD = 2600150, N_KOORD = 1200050, BBTOT = 5)

  out <- subtract_noloc(x, noloc, anchor = "center")

  expect_equal(out$noloc$status, "subtracted")
  expect_equal(out$noloc$cell_before, 30)
})

test_that("subtract_noloc keeps an integer raster integer", {
  data <- tibble::tibble(E_KOORD = 2600000, N_KOORD = 1200000, BBTOT = 10L)
  x <- table_to_stars(data, variables = "BBTOT", cellsize = 100)
  noloc <- tibble::tibble(E_KOORD = 2600000, N_KOORD = 1200000, BBTOT = 3L)

  out <- subtract_noloc(x, noloc)

  expect_type(out$stars$BBTOT, "integer")
  expect_equal(as.numeric(out$stars$BBTOT), 7)
})

test_that("subtract_noloc validates its inputs", {
  x <- population_raster()

  expect_error(
    subtract_noloc(x, tibble::tibble(E = 1, N = 2, BBTOT = 3)),
    "E_KOORD"
  )
  expect_error(
    subtract_noloc(x, tibble::tibble(E_KOORD = 2600000, N_KOORD = 1200000, POP = 3)),
    "BBTOT"
  )
})

test_that("subtract_noloc refuses rasters that are not two-dimensional", {
  x <- population_raster()
  cube <- c(x, x, along = list(year = c(2020L, 2021L)))

  expect_error(subtract_noloc(cube, tibble::tibble(E_KOORD = 2600000, N_KOORD = 1200000, BBTOT = 1)),
               "two-dimensional")
})
