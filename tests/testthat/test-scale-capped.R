# What makes a capped scale capped is that out-of-range values keep a colour
# instead of turning into NA, and that the legend says so. The first half is
# tested here on the built layer data; the legend half is in
# test-scale_capped_guide.R, because it only exists once the plot is drawn.

tile_plot <- function(z, ...) {
  d <- data.frame(x = seq_along(z), y = 1, z = z)
  ggplot2::ggplot(d, ggplot2::aes(x, y, fill = z)) +
    ggplot2::geom_tile() +
    scale_fill_capped(...)
}

test_that("scale_capped builds the three scale types with the right super class", {
  expect_s3_class(scale_capped(type = "continuous"), "ScaleContinuousCapped")
  expect_s3_class(scale_capped(type = "binned"), "ScaleBinnedCapped")
  expect_s3_class(scale_capped(type = "discrete"), "ScaleDiscrete")
  expect_error(scale_capped(type = "smooth"))
})

test_that("the wrappers differ only in aesthetic and type", {
  expect_equal(scale_fill_capped()$aesthetics, "fill")
  expect_equal(scale_colour_capped()$aesthetics, "colour")
  expect_equal(scale_color_capped()$aesthetics, "colour")
  expect_s3_class(scale_fill_capped_b(), "ScaleBinnedCapped")
  expect_s3_class(scale_colour_capped_d(), "ScaleDiscrete")

  # American spelling in, British spelling out -- one aesthetic name inside
  expect_equal(scale_capped(aesthetics = "color")$aesthetics, "colour")
  expect_equal(scale_capped(aesthetics = c("colour", "fill"))$aesthetics,
               c("colour", "fill"))
})

test_that("limits and probs are mutually exclusive", {
  expect_error(scale_fill_capped(limits = c(0, 1), probs = 0.02), "mutually exclusive")
  expect_warning(scale_colour_capped_d(probs = 0.02), "discrete")
})

test_that("probs is normalised and checked", {
  expect_equal(scale_fill_capped(probs = 0.02)$qprobs, c(0.02, 0.98))
  expect_equal(scale_fill_capped(probs = c(NA, 0.9))$qprobs, c(NA, 0.9))
  expect_null(scale_fill_capped()$qprobs)

  expect_equal(.capped_check_probs(0.05), c(0.05, 0.95))
  expect_error(.capped_check_probs(c(0.1, 0.5, 0.9)), "length")
  expect_error(.capped_check_probs(c(-1, 0.5)), "between 0 and 1")
  expect_error(.capped_check_probs(c(0.5, 2)), "between 0 and 1")
})

test_that("probs turns the trained data into limits", {
  z <- 1:100
  p <- tile_plot(z, probs = c(0.1, 0.9))
  b <- ggplot2::ggplot_build(p)
  sc <- b$plot$scales$get_scales("fill")

  expect_equal(sc$get_limits(),
               unname(stats::quantile(z, c(0.1, 0.9), names = FALSE)))

  # a one-sided quantile leaves the other end at the data edge
  sc2 <- ggplot2::ggplot_build(tile_plot(z, probs = c(NA, 0.9)))$plot$scales$get_scales("fill")
  expect_equal(sc2$get_limits()[1], 1)
  expect_equal(sc2$get_limits()[2], unname(stats::quantile(z, 0.9, names = FALSE)))
})

test_that("training the same data twice does not move the quantiles", {
  z  <- 1:100
  d  <- data.frame(x = z, y = 1, z = z)
  # two layers on the same scale: the buffer is keyed per chunk, so the
  # second copy must not be counted twice
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, fill = z)) +
    ggplot2::geom_tile() + ggplot2::geom_tile() +
    scale_fill_capped(probs = c(0.1, 0.9))

  sc <- ggplot2::ggplot_build(p)$plot$scales$get_scales("fill")
  expect_equal(sc$get_limits(),
               unname(stats::quantile(z, c(0.1, 0.9), names = FALSE)))
})

test_that("values outside the limits are squished to the edge, not censored", {
  ld <- ggplot2::layer_data(tile_plot(c(0, 5, 100), limits = c(0, 5)))

  expect_equal(ld$fill[3], ld$fill[2])       # 100 gets the colour of 5
  expect_false(anyNA(ld$fill))
})

test_that("oob = censor turns the scale back into an ordinary one", {
  ld <- ggplot2::layer_data(
    tile_plot(c(0, 5, 100), limits = c(0, 5), oob = scales::oob_censor))

  expect_equal(ld$fill[3], "grey80")         # na.value
  expect_equal(ggplot2::layer_data(
    tile_plot(c(0, 5, 100), limits = c(0, 5), oob = scales::oob_censor,
              na.value = "red"))$fill[3], "red")
})

test_that("the palette argument reaches the drawn colours", {
  ld <- ggplot2::layer_data(tile_plot(c(0, 1), limits = c(0, 1), palette = "Viridis"))
  expect_equal(ld$fill, substr(scales::pal_viridis(option = "D")(256)[c(1, 256)], 1, 7))

  rev <- ggplot2::layer_data(
    tile_plot(c(0, 1), limits = c(0, 1), palette = "Viridis", direction = -1))
  expect_equal(rev$fill, base::rev(ld$fill))
})

test_that("a binned scale asks the palette for exactly one colour per class", {
  d <- data.frame(x = 1:100, y = 1, z = 1:100)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, fill = z)) +
    ggplot2::geom_tile() +
    scale_fill_capped_b(limits = c(0, 100), breaks = c(25, 50, 75))

  ld <- ggplot2::layer_data(p)
  expect_equal(length(unique(ld$fill)), 4L)      # four classes, four colours
})

test_that("a discrete scale colours the levels and lets limits select them", {
  d <- data.frame(x = 1:3, y = 1, g = c("a", "b", "c"))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, fill = g)) +
    ggplot2::geom_tile() +
    scale_fill_capped_d(limits = c("a", "b"))

  ld <- ggplot2::layer_data(p)
  expect_equal(ld$fill[1:2], .capped_palette()$n_pal(2))
  expect_equal(ld$fill[3], "grey80")            # not in limits -> na.value
})

test_that("... reaches the underlying ggplot2 scale", {
  sc <- scale_fill_capped(name = "NOx", breaks = c(0, 50), limits = c(0, 100))
  expect_equal(sc$name, "NOx")
  expect_equal(sc$breaks, c(0, 50))
  expect_equal(sc$limits, c(0, 100))
})

test_that("the default guide is the capped one, and can be swapped out", {
  expect_s3_class(scale_fill_capped()$guide, "GuideColourbarCapped")
  expect_s3_class(scale_fill_capped_b()$guide, "GuideColourstepsCapped")
  expect_equal(scale_fill_capped(guide = "none")$guide, "none")
  expect_equal(scale_fill_capped(guide = "colourbar")$guide, "colourbar")
  expect_equal(scale_colour_capped_d()$guide, "legend")
})

test_that("a capped scale renders in a plot without warnings", {
  expect_renders_clean(tile_plot(c(0, 5, 100), limits = c(0, 50)))
  expect_renders_clean(tile_plot(1:100, probs = 0.05))
})

test_that("a scale without probs keeps no copy of the trained data", {
  # The quantile buffer exists only to answer probs; filling it for every
  # capped scale would hold a second copy of the fill column for the life of
  # the plot.
  p  <- tile_plot(1:100, limits = c(0, 50))
  sc <- ggplot2::ggplot_build(p)$plot$scales$get_scales("fill")
  expect_null(sc$qprobs_buffer)

  with_probs <- ggplot2::ggplot_build(tile_plot(1:100, probs = 0.1))$plot$scales$get_scales("fill")
  expect_false(is.null(with_probs$qprobs_buffer))
})
