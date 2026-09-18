# End-to-end alignment on synthetic rasters: no network, but the real code path
# from read_geo_admin()'s output through to a stacked cube.

grid_of <- function(cellsize, value, name, extent = c(2600000, 1200000, 2600400, 1200400)) {
  grid <- make_reference_grid(extent, cellsize = cellsize)
  grid[[1]][] <- value
  stats::setNames(grid, name)
}

read_geo_admin_result <- function() {
  dplyr::bind_rows(
    tibble::tibble(
      collection = "ch.bfs.statistik-bevoelkerung_haushalte",
      label = "statpop",
      year = c(2020L, 2021L),
      stars = list(grid_of(100, 1, "BBTOT"), grid_of(100, 2, "BBTOT")),
      noloc_subtracted = c(50, 60),
      noloc_unmatched = c(5, 6),
      noloc = list(tibble::tibble(x = 1), tibble::tibble(x = 2))
    ),
    tibble::tibble(
      collection = "ch.bafu.luftreinhaltung-feinstaub_pm2_5",
      label = "pm2_5",
      year = c(2020L, 2021L),
      stars = list(grid_of(200, 10, "pm2_5"), grid_of(200, 20, "pm2_5"))
    )
  )
}

test_that("align_to_reference puts every collection on the reference grid", {
  out <- align_to_reference(read_geo_admin_result())

  expect_equal(nrow(out), 2)
  expect_equal(out$year, c(2020L, 2021L))
  expect_equal(out$res_x, c(100, 100)) # the statpop grid, not the pm2_5 grid

  first <- out$stars[[1]]
  expect_named(first, c("BBTOT", "pm2_5"))
  expect_equal(unname(dim(first)), c(4L, 4L))
})

test_that("align_to_reference leaves the reference itself untouched", {
  data <- read_geo_admin_result()

  out <- align_to_reference(data)

  reference <- dplyr::filter(data, .data$label == "statpop")
  expect_equal(
    as.numeric(out$stars[[1]]$BBTOT),
    as.numeric(reference$stars[[1]]$BBTOT)
  )
})

test_that("align_to_reference refines the coarser collection onto the finer grid", {
  out <- align_to_reference(read_geo_admin_result())

  # pm2_5 is a concentration: refining must not change its value
  expect_equal(unique(as.numeric(out$stars[[1]]$pm2_5)), 10)
  expect_equal(unique(as.numeric(out$stars[[2]]$pm2_5)), 20)
})

test_that("align_to_reference carries the noloc bookkeeping through", {
  out <- align_to_reference(read_geo_admin_result())

  expect_equal(out$noloc_subtracted, c(50, 60))
  expect_equal(out$noloc_unmatched, c(5, 6))
  expect_equal(nrow(out$noloc[[1]]), 1)
})

test_that("align_to_reference logs the source of every attribute", {
  out <- align_to_reference(read_geo_admin_result())
  log <- out$sources[[1]]

  expect_equal(log$attribute, "pm2_5")
  expect_equal(log$source_year, 2020L)
  expect_equal(log$method, "average")
  expect_equal(log$status, "same_year")
  expect_equal(log$source_res_x, 200)
})

test_that("align_to_reference demands the reference and a second collection", {
  data <- read_geo_admin_result()

  expect_error(align_to_reference(data, reference = "ch.not.here"), "not part of")
  expect_error(
    align_to_reference(dplyr::filter(data, .data$label == "statpop")),
    "only one collection"
  )
})

test_that("align_to_grid puts every year on one freely chosen grid", {
  grid <- make_reference_grid(c(2600000, 1200000, 2600400, 1200400), cellsize = 200)

  out <- align_to_grid(read_geo_admin_result(), grid)

  expect_equal(nrow(out), 2)
  expect_equal(out$res_x, c(200, 200))
  expect_equal(dplyr::n_distinct(out$grid), 1)
  expect_named(out$stars[[1]], c("BBTOT", "pm2_5"))
})

test_that("align_to_grid conserves the population total when coarsening", {
  data <- read_geo_admin_result()
  grid <- make_reference_grid(c(2600000, 1200000, 2600400, 1200400), cellsize = 200)

  out <- align_to_grid(data, grid)
  log <- dplyr::filter(out$sources[[1]], .data$attribute == "BBTOT")

  expect_equal(log$method, "sum")
  expect_equal(log$total_source, log$total_aligned)
  expect_equal(sum(out$stars[[1]]$BBTOT, na.rm = TRUE), 16) # 16 cells of 1
})

test_that("align_to_grid averages concentrations rather than summing them", {
  grid <- make_reference_grid(c(2600000, 1200000, 2600400, 1200400), cellsize = 400)

  out <- align_to_grid(read_geo_admin_result(), grid)

  expect_equal(as.numeric(out$stars[[1]]$pm2_5), 10)
  expect_equal(as.numeric(out$stars[[1]]$BBTOT), 16)
})

test_that("align_to_grid keeps missing years as NA layers instead of dropping them", {
  grid <- make_reference_grid(c(2600000, 1200000, 2600400, 1200400), cellsize = 200)

  out <- align_to_grid(read_geo_admin_result(), grid, years = 2020:2022)

  expect_equal(out$year, 2020:2022)
  expect_true(all(is.na(out$stars[[3]]$BBTOT)))
  expect_equal(unique(out$sources[[3]]$status), "missing")
})

test_that("align_to_grid interpolates only where asked to", {
  data <- read_geo_admin_result() |>
    dplyr::filter(.data$label != "pm2_5" | .data$year == 2020L)
  grid <- make_reference_grid(c(2600000, 1200000, 2600400, 1200400), cellsize = 200)

  strict <- align_to_grid(data, grid, years = 2021)
  expect_equal(
    dplyr::filter(strict$sources[[1]], .data$attribute == "pm2_5")$status,
    "missing"
  )

  filled <- align_to_grid(data, grid, years = 2021, year_match = c(
    "ch.bafu.luftreinhaltung-feinstaub_pm2_5" = "nearest"
  ))
  pm <- dplyr::filter(filled$sources[[1]], .data$attribute == "pm2_5")
  expect_equal(pm$status, "nearest_year")
  expect_equal(pm$source_year, 2020L)
})

test_that("align_to_grid records an interpolation transparently", {
  data <- tibble::tibble(
    collection = "ch.bafu.luftreinhaltung-stickstoffdeposition",
    label = "n_deposition",
    year = c(2000L, 2005L),
    stars = list(grid_of(200, 10, "n_deposition"), grid_of(200, 20, "n_deposition"))
  )
  grid <- make_reference_grid(c(2600000, 1200000, 2600400, 1200400), cellsize = 200)

  out <- align_to_grid(data, grid, years = 2002, year_match = "linear")
  log <- out$sources[[1]]

  expect_equal(log$status, "interpolated")
  expect_equal(log$source_year, 2000L)
  expect_equal(log$source_year_after, 2005L)
  expect_equal(log$weight_after, 0.4)
  # 0.6 * 10 + 0.4 * 20
  expect_equal(unique(as.numeric(out$stars[[1]]$n_deposition)), 14)
})

test_that("align_to_grid rejects a no_data_value that occurs in the data", {
  data <- tibble::tibble(
    collection = "ch.test", label = "test", year = 2020L,
    stars = list(grid_of(100, -9999, "value"))
  )
  grid <- make_reference_grid(c(2600000, 1200000, 2600400, 1200400), cellsize = 200)

  expect_error(align_to_grid(data, grid), "no_data_value")
})

test_that("the aligned result stacks into exactly one cube", {
  grid <- make_reference_grid(c(2600000, 1200000, 2600400, 1200400), cellsize = 200)

  cubes <- read_geo_admin_result() |>
    align_to_grid(grid) |>
    stack_years(by = character(0))

  expect_equal(nrow(cubes), 1)
  cube <- cubes$cube[[1]]
  expect_named(cube, c("BBTOT", "pm2_5"))
  expect_equal(stars::st_get_dimension_values(cube, "year"), c(2020L, 2021L))
})

test_that("duplicate attribute names across collections abort with a usable hint", {
  data <- dplyr::bind_rows(
    tibble::tibble(
      collection = "ch.a", label = "a", year = 2020L,
      stars = list(grid_of(100, 1, "value"))
    ),
    tibble::tibble(
      collection = "ch.b", label = "b", year = 2020L,
      stars = list(grid_of(100, 2, "value"))
    )
  )
  grid <- make_reference_grid(c(2600000, 1200000, 2600400, 1200400), cellsize = 100)

  expect_error(align_to_grid(data, grid), "Duplicate attribute names")
})
