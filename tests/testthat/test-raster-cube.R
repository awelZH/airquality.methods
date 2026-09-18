one_raster <- function(value = 1, cellsize = 100, name = "pm2_5") {
  grid <- make_reference_grid(c(2600000, 1200000, 2600300, 1200200), cellsize = cellsize)
  grid[[1]][] <- value
  stats::setNames(grid, name)
}

year_tibble <- function(years, values = seq_along(years), cellsize = 100) {
  tibble::tibble(
    collection = "ch.test",
    label = "test",
    year = as.integer(years),
    stars = purrr::map2(values, cellsize, \(v, cs) one_raster(v, cs))
  )
}

test_that("stack_years builds one cube with a year dimension", {
  out <- stack_years(year_tibble(c(2020, 2021, 2022)))

  expect_equal(nrow(out), 1)
  expect_named(out, c("collection", "label", "grid", "years", "cube"))

  cube <- out$cube[[1]]
  expect_equal(unname(dim(cube)), c(3L, 2L, 3L))
  expect_equal(stars::st_get_dimension_values(cube, "year"), c(2020L, 2021L, 2022L))
})

test_that("stack_years keeps the values of each year", {
  cube <- stack_years(year_tibble(c(2020, 2021), values = c(5, 9)))$cube[[1]]

  expect_equal(unique(as.numeric(dplyr::slice(cube, "year", 1)$pm2_5)), 5)
  expect_equal(unique(as.numeric(dplyr::slice(cube, "year", 2)$pm2_5)), 9)
})

test_that("stack_years handles a single year without dropping the dimension", {
  cube <- stack_years(year_tibble(2020))$cube[[1]]

  expect_equal(unname(dim(cube)), c(3L, 2L, 1L))
  expect_equal(stars::st_get_dimension_values(cube, "year"), 2020L)
})

test_that("stack_years treats years as instants, so gaps stay gaps", {
  cube <- stack_years(year_tibble(c(2020, 2023, 2024)))$cube[[1]]

  expect_equal(stars::st_get_dimension_values(cube, "year"), c(2020L, 2023L, 2024L))
})

test_that("stack_years produces one cube per distinct grid", {
  mixed <- year_tibble(c(2020, 2021, 2022), cellsize = c(100, 100, 200))

  out <- stack_years(mixed)

  expect_equal(nrow(out), 2)
  expect_setequal(purrr::map_int(out$years, length), c(2L, 1L))
})

test_that("stack_years sorts years within a cube regardless of input order", {
  shuffled <- year_tibble(c(2022, 2020, 2021), values = c(3, 1, 2))

  cube <- stack_years(shuffled)$cube[[1]]

  expect_equal(stars::st_get_dimension_values(cube, "year"), c(2020L, 2021L, 2022L))
  expect_equal(unique(as.numeric(dplyr::slice(cube, "year", 1)$pm2_5)), 1)
})

test_that("stack_years rejects duplicate years on the same grid", {
  duplicated_years <- year_tibble(c(2020, 2020))

  expect_error(stack_years(duplicated_years), "share a year")
})

test_that("stack_years rejects differing attribute names", {
  mismatched <- tibble::tibble(
    collection = "ch.test", label = "test", year = c(2020L, 2021L),
    stars = list(one_raster(name = "a"), one_raster(name = "b"))
  )

  expect_error(stack_years(mismatched), "Attribute names differ")
})

test_that("tibble_to_cube is the inverse of as_tibble", {
  cube <- stack_years(year_tibble(c(2020, 2021), values = c(5, 9)))$cube[[1]]

  restored <- cube |>
    tibble::as_tibble() |>
    tibble_to_cube(cube)

  expect_equal(unname(dim(restored)), unname(dim(cube)))
  expect_equal(as.numeric(restored$pm2_5), as.numeric(cube$pm2_5))
  expect_equal(sf::st_crs(restored), sf::st_crs(cube))
  expect_equal(stars::st_get_dimension_values(restored, "year"), c(2020L, 2021L))
})

test_that("tibble_to_cube fills rows removed by a filter with NA", {
  cube <- stack_years(year_tibble(c(2020, 2021), values = c(5, 9)))$cube[[1]]

  restored <- cube |>
    tibble::as_tibble() |>
    dplyr::filter(.data$year == 2020) |>
    dplyr::mutate(year = NULL) |>
    tibble_to_cube(cube)

  expect_equal(unname(dim(restored)), c(3L, 2L))
  expect_equal(unique(as.numeric(restored$pm2_5)), 5)
})

test_that("tibble_to_cube supports a summary per cell without a year", {
  cube <- stack_years(year_tibble(c(2020, 2021), values = c(5, 9)))$cube[[1]]

  summary <- cube |>
    tibble::as_tibble() |>
    dplyr::summarise(pm2_5_mean = mean(.data$pm2_5), .by = c("x", "y")) |>
    tibble_to_cube(cube)

  expect_equal(unname(dim(summary)), c(3L, 2L))
  expect_equal(unique(as.numeric(summary$pm2_5_mean)), 7)
})

test_that("tibble_to_cube keeps character attributes as characters", {
  cube <- stack_years(year_tibble(2020, values = 5))$cube[[1]]

  classes <- cube |>
    tibble::as_tibble() |>
    dplyr::mutate(class = dplyr::if_else(.data$pm2_5 > 3, "high", "low"), pm2_5 = NULL) |>
    tibble_to_cube(cube)

  expect_type(classes$class, "character")
  expect_equal(unique(as.character(classes$class)), "high")
})

test_that("tibble_to_cube rejects coordinates off the template grid", {
  cube <- stack_years(year_tibble(2020))$cube[[1]]
  data <- tibble::as_tibble(cube)
  data$x <- data$x + 7

  expect_error(tibble_to_cube(data, cube), "do not lie on the grid")
})

test_that("tibble_to_cube rejects several rows per cell", {
  cube <- stack_years(year_tibble(2020))$cube[[1]]
  data <- tibble::as_tibble(cube)

  expect_error(tibble_to_cube(dplyr::bind_rows(data, data), cube), "Several rows per cell")
})

test_that("tibble_to_cube requires at least one attribute column", {
  cube <- stack_years(year_tibble(2020))$cube[[1]]
  data <- dplyr::select(tibble::as_tibble(cube), "x", "y", "year")

  expect_error(tibble_to_cube(data, cube), "no attribute columns")
})

test_that("tibble_to_cube refuses a year column the template has no dimension for", {
  flat <- one_raster()
  data <- tibble::as_tibble(flat)
  data$year <- 2020L

  expect_error(tibble_to_cube(data, flat), "no year dimension")
})

test_that("tibble_to_cube validates that coordinates are present", {
  cube <- stack_years(year_tibble(2020))$cube[[1]]

  expect_error(tibble_to_cube(tibble::tibble(a = 1), cube), "x")
})
