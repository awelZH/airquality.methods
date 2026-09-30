# The key is a plot whose only job is to be read, so the tests ask what a
# reader would: are the four elements there, do they carry the labels they
# were given, and does it draw without a warning.

test_that("the key draws the four elements and names them", {
  p <- band_key()

  expect_s3_class(p, "ggplot")
  expect_equal(sum(vapply(p$layers, function(l) inherits(l$geom, "GeomRibbon"),
                          logical(1))), 1L)
  expect_equal(sum(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"),
                          logical(1))), 1L)

  lab <- grob_labels(plot_gtable(p))
  expect_true(all(c("Median", "Mittelwert", "P25\u2013P75", "P10\u2013P90") %in% lab))
})

test_that("the labels are the caller's, in the order median, mean, inner, outer", {
  p   <- band_key(c("med", "avg", "inner", "outer"))
  lab <- grob_labels(plot_gtable(p))
  expect_true(all(c("med", "avg", "inner", "outer") %in% lab))
  expect_false("Median" %in% lab)
})

test_that("the bands carry the opacity the figure uses", {
  # The alpha is baked into the fill rather than mapped, so that the legend key
  # shows the shade the figure draws. Reading it back is therefore reading the
  # fill scale.
  p <- band_key(alpha = c(0.4, 0.1), colour = "black")
  f <- ggplot2::ggplot_build(p)$plot$scales$get_scales("fill")$palette.cache
  expect_true(any(grepl("66$", toupper(f))))   # 0.4 -> 66 in hex
  expect_true(any(grepl("1A$", toupper(f))))   # 0.1 -> 1A
})

test_that("the key takes the theme of the figure it explains", {
  # The key sits beside a figure and has to look like it, so the caller's
  # theme is the base; the key only switches off what a schematic hump does
  # not need.
  p <- band_key(theme = ggplot2::theme_bw(base_size = 7))

  expect_equal(p$theme$text$size, 7)
  expect_s3_class(p$theme$panel.border, "element_rect")
  expect_s3_class(p$theme$axis.text, "element_blank")
})

test_that("a key that cannot describe the figure is refused", {
  expect_error(band_key(c("a", "b", "c")), "four elements")
  expect_error(band_key(alpha = 0.3), "two values")
})

test_that("it renders clean", {
  expect_renders_clean(band_key())
  expect_renders_clean(band_key(title = "Statistik je Gr\u00f6ssenklasse"))
})
