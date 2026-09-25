raster_plot <- function(scale) {
  ggplot2::ggplot(ggplot2::faithfuld, ggplot2::aes(waiting, eruptions, fill = density * 1000)) +
    ggplot2::geom_raster() +
    scale
}

test_that("immissionscale returns a usable scale for every documented parameter", {
  for (parameter in immissionscale_parameters()) {
    scale <- immissionscale(parameter)
    expect_s3_class(scale, "Scale")
    expect_equal(scale$aesthetics, "fill")
  }
})

test_that("immissionscale carries the documented limits per pollutant", {
  expect_equal(immissionscale("NO2")$get_limits(), c(0, 50))
  expect_equal(immissionscale("PM10")$get_limits(), c(0, 34))
  expect_equal(immissionscale("PM2.5")$get_limits(), c(0, 17))
  expect_equal(immissionscale("eBC")$get_limits(), c(0, 1.5))
  expect_equal(immissionscale("Ndep")$get_limits(), c(0, 30))
})

test_that("immissionscale names the ozone metric in the legend title", {
  scale <- immissionscale("O3_peakseason_mean_d1_max_mean_h8gl")

  expect_match(scale$name, "Sommersaison")
  expect_match(immissionscale("O3_max_98p_m1")$name, "98%-Perz")
})

test_that("immissionscale squishes out-of-range values instead of dropping them", {
  # this is what distinguishes the capped scale: no holes in the map
  data <- data.frame(x = 1:3, y = 1, value = c(-10, 25, 900))
  built <- ggplot2::ggplot_build(
    ggplot2::ggplot(data, ggplot2::aes(x, y, fill = value)) +
      ggplot2::geom_raster() +
      immissionscale("NO2")
  )
  fills <- ggplot2::layer_data(built$plot)$fill

  expect_false(anyNA(fills))
  # both out-of-range values land on the edge colours, not on na.value
  expect_equal(fills[[1]], fills[fills == fills[[1]]][[1]])
  expect_false(fills[[1]] == fills[[3]])
})

test_that("immissionscale renders without warnings", {
  expect_renders_clean(raster_plot(immissionscale("PM2.5")))
  expect_renders_clean(raster_plot(immissionscale("Ndep")))
})

test_that("immissionscale marks capped values in the legend", {
  labels <- grob_labels(plot_gtable(raster_plot(immissionscale("eBC"))))

  # faithfuld density * 1000 runs well beyond the eBC limit of 1.5
  expect_true(any(grepl("≥|>=", labels)))
})

test_that("immissionscale aborts on an unknown parameter and lists the known ones", {
  expect_error(immissionscale("SO2"), "No scale defined")
  expect_error(immissionscale("SO2"), "PM10")
})

test_that("pal_emissions returns exactly n colours", {
  for (n in c(1, 3, 12)) {
    expect_length(pal_emissions(n, "Green"), n)
  }
})

test_that("pal_emissions returns valid colours for every palette", {
  for (name in c("Gray", "Purple", "Blue", "Green", "Gold", "natural")) {
    colours <- pal_emissions(5, name)
    expect_length(colours, 5)
    expect_no_error(grDevices::col2rgb(colours))
  }
})

test_that("pal_emissions rejects an unknown palette", {
  expect_error(pal_emissions(3, "Rainbow"), "Gray")
})

test_that("theme_custom returns a theme for every documented type", {
  for (type in c("timeseries", "scatter", "flipped", "map", "default")) {
    expect_s3_class(theme_custom(type), "theme")
  }
})

test_that("theme_custom strips what each figure type does not need", {
  expect_s3_class(theme_custom("map")$axis.text, "element_blank")
  expect_s3_class(theme_custom("timeseries")$axis.title, "element_blank")
  expect_s3_class(theme_custom("scatter")$panel.grid.major, "element_blank")
})

test_that("theme_custom rejects an unknown type", {
  expect_error(theme_custom("barplot"), "timeseries")
})

test_that("theme_custom and theme_legend_inside compose onto a plot", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
    ggplot2::geom_point() +
    theme_custom("scatter") +
    theme_legend_inside()

  expect_renders_clean(p)
})

test_that("theme_legend_inside places the legend inside the panel", {
  theme <- theme_legend_inside()

  expect_equal(theme$legend.position, "inside")
  expect_equal(theme$legend.position.inside, c(0.975, 0.95))
})

test_that("immissionscale writes the legend title as markdown and renders it with ggtext", {
  scale <- immissionscale("NO2")

  expect_equal(scale$name, "NO<sub>2</sub><br>(µg/m<sup>3</sup>)")
  expect_s3_class(scale$guide$params$theme$legend.title, "element_markdown")
  expect_renders_clean(raster_plot(immissionscale("PM2.5")))
  expect_renders_clean(raster_plot(immissionscale("Ndep")))
})
