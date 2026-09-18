# The guide only exists once the plot is drawn, so everything here goes
# through the gtable: the cap symbols are text grobs in the legend, the
# whiskers are named grobs beside the bar. Testing the drawn object is also
# what makes these tests meaningful -- a cap symbol that is computed but
# never placed would pass any test of the params alone.

capped_plot <- function(z, ...) {
  d <- data.frame(x = seq_along(z), y = 1, z = z)
  ggplot2::ggplot(d, ggplot2::aes(x, y, fill = z)) +
    ggplot2::geom_tile() +
    scale_fill_capped(...)
}

# Escaped rather than literal, so the file stays ASCII whatever encoding the
# checking machine runs in.
LE <- "\u2264"
GE <- "\u2265"

test_that("the legend flags the ends where data lie outside the limits", {
  lab <- grob_labels(plot_gtable(capped_plot(c(-50, 20, 500), limits = c(0, 100))))

  expect_true(any(grepl(LE, lab, fixed = TRUE)))
  expect_true(any(grepl(GE, lab, fixed = TRUE)))
})

test_that("an end that is not exceeded gets no cap symbol", {
  lab <- grob_labels(plot_gtable(capped_plot(c(10, 20, 500), limits = c(0, 100))))

  expect_false(any(grepl(LE, lab, fixed = TRUE)))    # nothing below 0
  expect_true(any(grepl(GE, lab, fixed = TRUE)))
})

test_that("no capping at all means an ordinary-looking legend", {
  lab <- grob_labels(plot_gtable(capped_plot(c(10, 20, 90), limits = c(0, 100))))

  expect_false(any(grepl(LE, lab, fixed = TRUE)))
  expect_false(any(grepl(GE, lab, fixed = TRUE)))
})

test_that("oob = censor removes the cap symbols, because nothing is capped", {
  lab <- grob_labels(plot_gtable(
    capped_plot(c(-50, 20, 500), limits = c(0, 100), oob = scales::oob_censor)))

  expect_false(any(grepl(LE, lab, fixed = TRUE)))
  expect_false(any(grepl(GE, lab, fixed = TRUE)))
})

test_that("cap = FALSE keeps the squishing but drops the annotation", {
  lab <- grob_labels(plot_gtable(
    capped_plot(c(-50, 20, 500), limits = c(0, 100), cap = FALSE)))

  expect_false(any(grepl(LE, lab, fixed = TRUE)))
  expect_false(any(grepl(GE, lab, fixed = TRUE)))
})

test_that("the whiskers are drawn as their own grobs, and can be switched off", {
  with_w <- grob_names(plot_gtable(capped_plot(c(-50, 20, 500), limits = c(0, 100))))
  no_w   <- grob_names(plot_gtable(
    capped_plot(c(-50, 20, 500), limits = c(0, 100), whiskers = FALSE)))

  expect_true(any(grepl("^whisker-", with_w)))
  expect_false(any(grepl("^whisker-", no_w)))
})

test_that("the whisker labels carry the true data extremes", {
  lab <- grob_labels(plot_gtable(capped_plot(c(-50, 20, 500), limits = c(0, 100))))

  expect_true(any(grepl("-50", lab, fixed = TRUE)))
  expect_true(any(grepl("500", lab, fixed = TRUE)))

  # ... and whisker_labels = FALSE leaves the whisker but drops the number
  lab2 <- grob_labels(plot_gtable(
    capped_plot(c(-50, 20, 500), limits = c(0, 100), whisker_labels = FALSE)))
  expect_false(any(grepl("500", lab2, fixed = TRUE)))
})

test_that("a whisker inside the limits is not drawn -- there is nothing to show", {
  nm <- grob_names(plot_gtable(capped_plot(c(10, 20, 90), limits = c(0, 100))))
  expect_false(any(grepl("^whisker-", nm)))
})

test_that("the binned guide caps and whiskers the same way", {
  d <- data.frame(x = 1:100, y = 1, z = c(-50, 2:99, 500))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, fill = z)) +
    ggplot2::geom_tile() +
    scale_fill_capped_b(limits = c(0, 100), breaks = c(25, 50, 75))
  gt <- plot_gtable(p)

  expect_true(any(grepl(GE, grob_labels(gt), fixed = TRUE)))
  expect_true(any(grepl("^whisker-", grob_names(gt))))
})

test_that("a horizontal legend gets the same treatment as a vertical one", {
  p <- capped_plot(c(-50, 20, 500), limits = c(0, 100)) +
    ggplot2::theme(legend.position = "bottom")
  gt <- plot_gtable(p)

  expect_true(any(grepl(GE, grob_labels(gt), fixed = TRUE)))
  expect_true(any(grepl("^whisker-", grob_names(gt))))
})

test_that("the guide constructors return guides that can stand alone", {
  expect_s3_class(guide_colourbar_capped(), "GuideColourbarCapped")
  expect_s3_class(guide_coloursteps_capped(), "GuideColourstepsCapped")
  expect_s3_class(guide_colorbar_capped(), "GuideColourbarCapped")

  # passed to a scale directly, rather than through scale_capped()'s arguments
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1, z = c(-50, 20, 500)),
                       ggplot2::aes(x, y, fill = z)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(limits = c(0, 100), oob = scales::oob_squish,
                                  guide = guide_colourbar_capped())
  expect_true(any(grepl(GE, grob_labels(plot_gtable(p)), fixed = TRUE)))
})

test_that(".capped_format falls back through the label routes in order", {
  sc <- scale_fill_capped(limits = c(0, 100))
  expect_equal(.capped_format(sc, 50, fun = function(x) paste0(x, " ppb")), "50 ppb")
  expect_equal(.capped_format(sc, 50, fun = TRUE), "50")
  # a labelling function that errors must not take the legend down with it
  expect_equal(.capped_format(sc, 50, fun = function(x) stop("nope")), "50")
})

test_that("capped legends render without warnings, in both orientations", {
  expect_renders_clean(capped_plot(c(-50, 20, 500), limits = c(0, 100)))
  expect_renders_clean(capped_plot(c(-50, 20, 500), limits = c(0, 100)) +
                         ggplot2::theme(legend.position = "bottom"))
})
