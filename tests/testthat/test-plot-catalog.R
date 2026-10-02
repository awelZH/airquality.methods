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

# ---- keys of the caller's own ---------------------------------------------------------

test_that("plot_catalog() takes the names of a list as a key of the caller's own", {
  result <- plot_catalog(list(A1 = titled("a1"), B2 = titled("b2")), "rose", names_to = "site")

  expect_named(result, c("plot", "parameter", "year", "site", "figure"))
  expect_equal(result$site, c("A1", "B2"))
  expect_equal(result$plot, c("rose", "rose"))
  expect_true(all(is.na(result$parameter)))
  expect_true(all(is.na(result$year)))
  expect_equal(result$figure[[2]]$labels$title, "b2")
})

test_that("plot_catalog() mixes parameter, year and own keys in a nested list", {
  plots <- list(
    PN = list(A1 = list(Tag = titled("pn a1 tag"), Nacht = titled("pn a1 nacht"))),
    Dp = list(A1 = list(Tag = titled("dp a1 tag")))
  )

  result <- plot_catalog(plots, "rose", names_to = c("parameter", "site", "daypart"))

  expect_named(result, c("plot", "parameter", "year", "site", "daypart", "figure"))
  expect_equal(result$parameter, c("PN", "PN", "Dp"))
  expect_equal(result$site, c("A1", "A1", "A1"))
  expect_equal(result$daypart, c("Tag", "Nacht", "Tag"))
  expect_equal(result$figure[[2]]$labels$title, "pn a1 nacht")
})

test_that("plot_catalog() takes the plot names from the list with names_to = \"plot\"", {
  result <- plot_catalog(list(regime_mean = titled("m"), distance = titled("d")), names_to = "plot")

  expect_named(result, c("plot", "parameter", "year", "figure"))
  expect_equal(result$plot, c("regime_mean", "distance"))
  expect_true(all(is.na(result$parameter)))
  expect_equal(get_plot(result, "distance")$labels$title, "d")
})

test_that("plot_catalog() takes plot names and further keys together", {
  plots <- list(map = list(A1 = titled("map a1")), rose = list(A1 = titled("rose a1"), B2 = titled("rose b2")))

  result <- plot_catalog(plots, names_to = c("plot", "site"))

  expect_equal(result$plot, c("map", "rose", "rose"))
  expect_equal(result$site, c("A1", "A1", "B2"))
})

test_that("plot_catalog() needs the plot name exactly once, as argument or as list names", {
  plots <- list(a = titled("a"))

  expect_error(plot_catalog(plots, "x", names_to = "plot"), "plot", class = "plot_catalog_error")
  expect_error(plot_catalog(titled("a")), "plot", class = "plot_catalog_error")
  expect_error(plot_catalog(plots, names_to = "site"), "plot", class = "plot_catalog_error")
})

test_that("plot_catalog() refuses repeated keys and the reserved name figure", {
  plots <- list(a = list(b = titled("a")))

  expect_error(plot_catalog(plots, "x", names_to = c("site", "site")), "site", class = "plot_catalog_error")
  expect_error(plot_catalog(list(a = titled("a")), "x", names_to = "figure"), "figure", class = "plot_catalog_error")
})

make_catalog_own_keys <- function() {
  dplyr::bind_rows(
    plot_catalog(list(NO2 = titled("no2"), PM10 = titled("pm10")), "timeseries", names_to = "parameter"),
    plot_catalog(list(A1 = list(Tag = titled("a1 tag"), Nacht = titled("a1 nacht")), B2 = list(Tag = titled("b2 tag"))),
                 "rose", names_to = c("site", "daypart"))
  )
}

test_that("get_plot() filters on own keys given by name", {
  catalog <- make_catalog_own_keys()

  expect_equal(get_plot(catalog, "rose", site = "A1", daypart = "Nacht")$labels$title, "a1 nacht")
  expect_equal(get_plot(catalog, "rose", site = "B2")$labels$title, "b2 tag")
  expect_equal(get_plot(catalog, "timeseries", "PM10")$labels$title, "pm10")
})

test_that("catalog_entries() filters on own keys, several values at once", {
  catalog <- make_catalog_own_keys()

  expect_equal(nrow(catalog_entries(catalog, "rose")), 3)
  expect_equal(catalog_entries(catalog, "rose", daypart = "Tag")$site, c("A1", "B2"))
  expect_equal(nrow(catalog_entries(catalog, "rose", site = c("A1", "B2"), daypart = "Tag")), 2)
})

test_that("catalog_entries() stops on a key the catalog does not have, naming the keys it has", {
  catalog <- make_catalog_own_keys()

  expect_error(catalog_entries(catalog, "rose", station = "A1"), class = "plot_catalog_error")
  expect_error(catalog_entries(catalog, "rose", station = "A1"), "daypart")
  expect_error(catalog_entries(catalog, "rose", "A1", NULL, "Tag"), "named", class = "plot_catalog_error")
})

test_that("get_plot() names the combinations of own keys when it cannot pick one plot", {
  catalog <- make_catalog_own_keys()

  # \s+: cli may wrap the line inside a combination
  expect_error(get_plot(catalog, "rose", site = "A1"), "A1\\s+/\\s+Nacht", class = "plot_catalog_error")
  expect_error(get_plot(catalog, "rose", site = "C3"), "site\\s+/\\s+daypart", class = "plot_catalog_error")
  expect_error(get_plot(catalog, "timeseries", "O3"), "parameter", class = "plot_catalog_error")
})

test_that("plot_catalog() does not call the plot NA when the plot names are missing from the list", {
  expect_error(plot_catalog(list(titled("a")), names_to = "plot"), "named list", class = "plot_catalog_error")
  expect_no_match(
    conditionMessage(rlang::catch_cnd(plot_catalog(list(titled("a")), names_to = "plot"))),
    "NA"
  )
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
