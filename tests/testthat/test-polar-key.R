# The key rose exists to describe the roses beside it, so almost every test
# here is a comparison against what polar_raster() actually draws: rings that
# are not the figure's rings would make the key worse than no key at all.

ring_radii <- function(p) {
  # The ring layer is the one whose data carry a .r column, one row per
  # vertex; the radii are what the key claims the rings are.
  for (l in p$layers) if (!is.null(l$data$.r)) return(sort(unique(l$data$.r)))
  numeric(0)
}

test_that("the key's rings are the rings polar_raster draws", {
  d <- fake_polar()
  p <- polar_raster(d, facet = "site")
  k <- polar_key(d)

  expect_equal(ring_radii(k), ring_radii(p))
  expect_gt(length(ring_radii(k)), 0)
})

test_that("a number and the data it came from give the same key", {
  d <- fake_polar()
  expect_equal(ring_radii(polar_key(max(sqrt(d$u^2 + d$v^2)))), ring_radii(polar_key(d)))
})

test_that("explicit rings win, here as in polar_raster", {
  k <- polar_key(8, grid = polar_grid(rings = c(2, 5)))
  expect_equal(ring_radii(k), c(2, 5))
})

test_that("the key names the directions and the rings", {
  lab <- grob_labels(plot_gtable(polar_key(6, axis_unit = "m/s")))

  expect_true(all(c("N", "E", "S", "W") %in% lab))
  expect_true(any(grepl("m/s", lab)))            # a ring carries its unit
  expect_false(any(c("NE", "SW") %in% lab))      # four points unless asked

  lab8 <- grob_labels(plot_gtable(polar_key(6, compass = "8")))
  expect_true(all(c("NE", "SE", "SW", "NW") %in% lab8))
})

test_that("the compass is on even when the roses have it off", {
  # The whole point of the key: the panels go without compass letters, and
  # the key says them once. So grid$compass must not be able to silence it.
  lab <- grob_labels(plot_gtable(polar_key(6, grid = polar_grid(compass = FALSE))))
  expect_true("N" %in% lab)

  lab_off <- grob_labels(plot_gtable(polar_key(6, compass = FALSE)))
  expect_false("N" %in% lab_off)
})

test_that("the key carries a title only when asked", {
  expect_false("Wind" %in% grob_labels(plot_gtable(polar_key(6))))
  expect_true("Wind" %in% grob_labels(plot_gtable(polar_key(6, title = "Wind"))))
})

test_that("the key holds no data of its own", {
  k <- polar_key(6)
  expect_s3_class(k$data, "waiver")   # ggplot() with no data at all
  expect_renders_clean(k)
})

test_that("a radius that cannot be drawn is refused, not guessed", {
  expect_error(polar_key(0), "positive")
  expect_error(polar_key(-1), "positive")
  expect_error(polar_key(fake_polar(), u = "east"), "not found")
})

test_that("the key labels at most three rings, as the map's key does", {
  lab <- function(...) grep("m/s", grob_labels(plot_gtable(polar_key(...))), value = TRUE)

  # four rings and a cap of three: the thinning keeps every second one, so
  # two labels rather than three -- even spacing wins over filling the quota
  expect_length(lab(9), 2L)
  expect_length(lab(9, n_labels = 2), 2L)
  expect_length(lab(9, n_labels = Inf), 4L)
  expect_true("8 m/s" %in% lab(9))             # counted from the rim inwards
})
