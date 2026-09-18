sample_data <- function() {
  tibble::tibble(
    site = rep(c("A", "B"), each = 5),
    year = 2020L,
    value = c(1, 2, 3, 4, 5, 10, 20, 30, 40, 50)
  )
}

test_that("aggregate_groups returns one row per group with the documented columns", {
  out <- aggregate_groups(sample_data(), "value", "site")

  expect_equal(nrow(out), 2)
  expect_contains(
    names(out),
    c("site", "n", "minimum", "lower", "middle", "upper", "maximum", "mean",
      "standarddeviation", "standarderror", "medianabsolutedeviation", "sum")
  )
})

test_that("aggregate_groups computes the statistics correctly", {
  out <- dplyr::filter(aggregate_groups(sample_data(), "value", "site"), site == "A")

  expect_equal(out$n, 5)
  expect_equal(out$minimum, 1)
  expect_equal(out$maximum, 5)
  expect_equal(out$middle, 3)
  expect_equal(out$mean, 3)
  expect_equal(out$sum, 15)
  expect_equal(out$standarddeviation, stats::sd(1:5))
  expect_equal(out$standarderror, stats::sd(1:5) / sqrt(5))
})

test_that("aggregate_groups ignores NA when counting and summarising", {
  data <- tibble::tibble(site = "A", value = c(1, NA, 3))

  out <- aggregate_groups(data, "value", "site", nmin = 1)

  expect_equal(out$n, 2)
  expect_equal(out$mean, 2)
  expect_equal(out$sum, 4)
})

test_that("aggregate_groups blanks statistics for groups below nmin", {
  data <- tibble::tibble(site = c("A", "A", "A", "B"), value = c(1, 2, 3, 99))

  out <- aggregate_groups(data, "value", "site", nmin = 3)

  small <- dplyr::filter(out, site == "B")
  expect_equal(small$n, 1)
  expect_true(is.na(small$mean))
  expect_true(is.na(small$sum))
  expect_true(is.na(small$maximum))

  large <- dplyr::filter(out, site == "A")
  expect_equal(large$mean, 2)
})

test_that("aggregate_groups completes missing group combinations with n = 0", {
  data <- tibble::tibble(
    site = c("A", "A", "A", "B", "B", "B"),
    year = c(2020L, 2020L, 2020L, 2021L, 2021L, 2021L),
    value = c(1, 2, 3, 4, 5, 6)
  )

  out <- aggregate_groups(data, "value", c("site", "year"))

  expect_equal(nrow(out), 4) # 2 sites x 2 years
  empty <- dplyr::filter(out, site == "A", year == 2021L)
  expect_equal(empty$n, 0)
  expect_true(is.na(empty$mean))
})

test_that("aggregate_groups never emits NaN or Inf, and stays quiet doing it", {
  data <- tibble::tibble(site = c("A", "B"), value = c(NA_real_, 1))

  expect_no_warning(out <- aggregate_groups(data, "value", "site", nmin = 1))

  numeric_columns <- dplyr::select(out, dplyr::where(is.numeric))
  expect_false(any(purrr::map_lgl(numeric_columns, \(col) any(is.nan(col)))))
  expect_false(any(purrr::map_lgl(numeric_columns, \(col) any(is.infinite(col)))))
})

test_that("aggregate_groups honours custom percentiles", {
  data <- tibble::tibble(site = "A", value = 1:100)

  out <- aggregate_groups(
    data, "value", "site",
    perc = list(ymin = 0.1, lower = 0.25, middle = 0.5, upper = 0.75, ymax = 0.9)
  )

  expect_equal(out$minlower, unname(stats::quantile(1:100, 0.1)))
  expect_equal(out$maxupper, unname(stats::quantile(1:100, 0.9)))
})

test_that("aggregate_groups validates its column arguments", {
  expect_error(aggregate_groups(sample_data(), "nope", "site"), "nope")
  expect_error(aggregate_groups(sample_data(), "value", "nope"), "nope")
})

test_that("aggregate_groups returns an ungrouped tibble", {
  out <- aggregate_groups(sample_data(), "value", "site")

  expect_s3_class(out, "tbl_df")
  expect_false(dplyr::is_grouped_df(out))
})
