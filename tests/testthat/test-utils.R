test_that("check_names accepts complete sets and is silent", {
  expect_true(check_names(c("a", "b", "c"), c("a", "b")))
  expect_silent(check_names(c("a", "b"), character(0)))
})

test_that("check_names aborts and names the missing columns", {
  expect_error(check_names(c("a", "b"), c("a", "x", "y")), "x")
  expect_error(check_names(c("a", "b"), c("a", "x", "y")), "y")
  # the available names are reported as a hint, so the message is actionable
  expect_error(check_names(c("a", "b"), "x"), "b")
})

test_that("round_off rounds half away from zero, unlike base round", {
  # base R rounds half to even: round(0.5) == 0, round(2.5) == 2
  expect_equal(round_off(c(0.5, 1.5, 2.5)), c(1, 2, 3))
  expect_equal(round_off(c(-0.5, -1.5, -2.5)), c(-1, -2, -3))
})

test_that("round_off respects digits", {
  expect_equal(round_off(1.005, 2), 1.01)
  expect_equal(round_off(2.675, 2), 2.68)
  expect_equal(round_off(1.2345, 3), 1.235)
})

test_that("round_off leaves whole numbers and zero untouched", {
  expect_equal(round_off(c(0, 3, -7)), c(0, 3, -7))
})

test_that("round_off is symmetric around zero", {
  x <- c(0.5, 1.45, 2.675, 12.5, 0.125)
  expect_equal(round_off(-x, 2), -round_off(x, 2))
})

test_that("round_off propagates NA", {
  expect_equal(round_off(c(1.5, NA, 2.5)), c(2, NA, 3))
})

test_that("check_names signals errors of the given class", {
  expect_error(check_names(c("a", "b"), "x", class = "my_input_error"), class = "my_input_error")
  expect_error(check_names(c("a", "b"), "x", what = "the test data"), "the test data")
})

test_that("write_local_csv creates missing directories", {
  dir <- withr::local_tempdir()
  file <- file.path(dir, "sub", "folder", "out.csv")

  write_local_csv(tibble::tibble(a = 1), file)

  expect_equal(readLines(file), c("a", "1"))
})

test_that("write_local_csv appends without header, but writes one when the file is new", {
  file <- withr::local_tempfile(fileext = ".csv")

  write_local_csv(tibble::tibble(run = "a", value = 1), file, append = TRUE)
  write_local_csv(tibble::tibble(run = "b", value = 2), file, append = TRUE)

  expect_equal(readLines(file), c("run;value", "a;1", "b;2"))
})
