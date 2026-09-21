# Unit tests for R/municipalities.R. All inputs are synthetic (helper-municipalities.R); no network.

test_that("drop_foreign_enclaves() removes enclaves of other cantons only", {
  result <- drop_foreign_enclaves(make_map())

  expect_equal(nrow(result), 4)
  expect_false(any(grepl("ausserkantonal", result$art_text)))
})

test_that("assign_municipalities() assigns every cell exactly once", {
  cells <- tibble::tibble(
    x = c(50, 150, 1050, 250, 5000),
    y = c(50, 150, 1050, 50, 5000),
    year = 2020,
    population = c(1, 2, 4, 8, 32)
  )

  result <- assign_municipalities(cells, drop_foreign_enclaves(make_map()))

  expect_equal(nrow(result), nrow(cells))
  expect_equal(sum(result$population), sum(cells$population))
  expect_equal(result$bfsnr, c(1L, 1L, 1L, 2L, NA))
  expect_equal(result$gemeindename, c("A", "A", "A", "B", NA))
})

test_that("assign_municipalities() gives lake cells the nearest municipality", {
  # (50, 250) lies 50 m from A, (350, 250) 50 m from B
  cells <- tibble::tibble(x = c(50, 350), y = c(250, 250), population = c(16, 64))

  expect_message(
    result <- assign_municipalities(cells, drop_foreign_enclaves(make_map())),
    "nearest"
  )
  expect_equal(result$bfsnr, c(1L, 2L))
  expect_equal(result$gemeindename, c("A", "B"))
})

test_that("assign_municipalities() leaves cells in a foreign enclave outside the canton", {
  cells <- tibble::tibble(x = 2050, y = 2050, population = 23)

  result <- assign_municipalities(cells, drop_foreign_enclaves(make_map()))

  expect_true(is.na(result$bfsnr))
})

test_that("assign_municipalities() rejects ambiguous bfs numbers", {
  map <- make_map()
  map$gemeindename[2] <- "other name"

  expect_error(assign_municipalities(tibble::tibble(x = 50, y = 50), map), "bfs")
})


# ---- STATPOP collector pixels -------------------------------------------------

test_that("noloc_from_aligned() returns the subtracted inhabitants per cell centre", {
  result <- noloc_from_aligned(make_aligned_noloc())

  expect_named(result, c("year", "x", "y", "noloc"))
  expect_equal(result$year, c(2020, 2020))
  expect_equal(result$x, c(50, 5050))
  expect_equal(result$noloc, c(6, 9))
})

test_that("noloc_from_aligned() copes with years without correction", {
  aligned <- make_aligned_noloc()
  aligned$noloc <- list(NULL, NULL)

  result <- noloc_from_aligned(aligned)

  expect_equal(nrow(result), 0)
  expect_named(result, c("year", "x", "y", "noloc"))
})

test_that("redistribute_noloc() spreads collector inhabitants over the inhabited cells of their municipality", {
  cells <- make_cells_noloc()
  noloc <- tibble::tibble(year = 2020, x = 50, y = 50, noloc = 8)

  result <- redistribute_noloc(cells, noloc, drop_foreign_enclaves(make_map()))

  # A: 40 located + 8 collector inhabitants, proportional to the cells' inhabitants
  expect_equal(result$population, c(12, 36, 0, 20, 7))
})

test_that("redistribute_noloc() keeps the municipality's weighted mean", {
  cells <- make_cells_noloc()
  noloc <- tibble::tibble(year = 2020, x = 50, y = 50, noloc = 8)

  result <- redistribute_noloc(cells, noloc, drop_foreign_enclaves(make_map()))
  a_before <- dplyr::filter(cells, bfsnr == 1)
  a_after <- dplyr::filter(result, bfsnr == 1)

  expect_equal(
    stats::weighted.mean(a_after$no2, a_after$population),
    stats::weighted.mean(a_before$no2, a_before$population)
  )
})

test_that("redistribute_noloc() ignores collector pixels outside the canton and works per year", {
  cells <- dplyr::bind_rows(make_cells_noloc(), dplyr::mutate(make_cells_noloc(), year = 2021))
  noloc <- tibble::tibble(year = c(2020, 2021), x = c(5050, 250), y = c(5050, 50), noloc = c(100, 4))

  result <- redistribute_noloc(cells, noloc, drop_foreign_enclaves(make_map()))

  expect_equal(result$population[result$year == 2020], c(10, 30, 0, 20, 7))
  expect_equal(result$population[result$year == 2021], c(10, 30, 0, 24, 7))
})

test_that("redistribute_noloc() warns when a municipality has no inhabited cell", {
  cells <- dplyr::mutate(make_cells_noloc(), population = dplyr::if_else(bfsnr %in% 2L, 0, population))
  noloc <- tibble::tibble(year = 2020, x = 250, y = 50, noloc = 5)

  expect_warning(result <- redistribute_noloc(cells, noloc, drop_foreign_enclaves(make_map())), "B")
  expect_equal(result$population, cells$population)
})

test_that("redistribute_noloc() leaves cells unchanged without collector pixels", {
  cells <- make_cells_noloc()
  noloc <- tibble::tibble(year = numeric(), x = numeric(), y = numeric(), noloc = numeric())

  expect_equal(redistribute_noloc(cells, noloc, drop_foreign_enclaves(make_map())), cells)
})
