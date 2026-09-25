titled <- function(title) ggplot2::ggplot() + ggplot2::ggtitle(title)

# ---- plot_catalog() -----------------------------------------------------------------

test_that("plot_catalog() gives one row for a single plot, without parameter and year", {
  result <- plot_catalog(titled("a"), "rsd_norm")

  expect_named(result, c("plot", "parameter", "year", "figure"))
  expect_equal(result$plot, "rsd_norm")
  expect_true(is.na(result$parameter))
  expect_true(is.na(result$year))
  expect_s3_class(result$figure[[1]], "ggplot")
})

test_that("plot_catalog() takes the names of a list as parameter or year", {
  per_parameter <- plot_catalog(list(NO2 = titled("a"), PM10 = titled("b")), "timeseries", names_to = "parameter")
  per_year <- plot_catalog(list(`2020` = titled("a"), `2021` = titled("b")), "ndep_hist", names_to = "year")

  expect_equal(per_parameter$parameter, c("NO2", "PM10"))
  expect_true(all(is.na(per_parameter$year)))
  expect_equal(per_year$year, c("2020", "2021"))
  expect_true(all(is.na(per_year$parameter)))
})

test_that("plot_catalog() takes the names of a nested list as parameter and year", {
  plots <- list(
    NO2 = list(alle = titled("a"), `2020` = titled("b")),
    PM10 = list(`2020` = titled("c"))
  )

  result <- plot_catalog(plots, "distribution_cumulative", names_to = c("parameter", "year"))

  expect_equal(result$parameter, c("NO2", "NO2", "PM10"))
  expect_equal(result$year, c("alle", "2020", "2020"))
  expect_equal(result$figure[[3]]$labels$title, "c")
})

test_that("plot_catalog() stops on unnamed lists", {
  expect_error(plot_catalog(list(titled("a")), "x", names_to = "parameter"), "named", class = "plot_catalog_error")
})

# ---- get_plot(), catalog_entries() ----------------------------------------------------

make_catalog <- function() {
  dplyr::bind_rows(
    plot_catalog(titled("single"), "rsd_norm"),
    plot_catalog(list(NO2 = titled("no2"), PM10 = titled("pm10")), "timeseries", names_to = "parameter"),
    plot_catalog(list(NO2 = list(`2020` = titled("no2 2020"), `2021` = titled("no2 2021"))), "map", names_to = c("parameter", "year"))
  )
}

test_that("get_plot() returns the one plot matching plot, parameter and year", {
  catalog <- make_catalog()

  expect_equal(get_plot(catalog, "rsd_norm")$labels$title, "single")
  expect_equal(get_plot(catalog, "timeseries", "PM10")$labels$title, "pm10")
  expect_equal(get_plot(catalog, "map", "NO2", 2021)$labels$title, "no2 2021")
})

test_that("get_plot() stops if no plot or several plots match, naming the available ones", {
  catalog <- make_catalog()

  expect_error(get_plot(catalog, "timeseries", "O3"), class = "plot_catalog_error")
  expect_error(get_plot(catalog, "timeseries", "O3"), "PM10")
  expect_error(get_plot(catalog, "timeseries"), class = "plot_catalog_error")
  expect_error(get_plot(catalog, "tiemseries"), "timeseries", class = "plot_catalog_error")
})

test_that("catalog_entries() returns all rows of a plot, optionally of some parameters and years", {
  catalog <- make_catalog()

  expect_equal(nrow(catalog_entries(catalog, "map")), 2)
  expect_equal(catalog_entries(catalog, "map", "NO2", 2020)$year, "2020")
  expect_error(catalog_entries(catalog, "map", "PM10"), class = "plot_catalog_error")
})

# ---- print_tabset() ---------------------------------------------------------------------

test_that("print_tabset() writes one tab per plot, titled by its name", {
  output <- withr::with_pdf(NULL, utils::capture.output(print_tabset(list(absolut = titled("a"), relativ = titled("b")))))

  expect_contains(output, c("::: {.panel-tabset}", "##### absolut", "##### relativ"))
})

test_that("print_tabset() calls functions for the content of a tab and takes the heading level", {
  output <- utils::capture.output(print_tabset(list(eins = \() cat("content\n")), level = 3))

  expect_contains(output, c("### eins", "content"))
})
