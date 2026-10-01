# polar_raster() makes four decisions that a picture alone will not reveal:
# which geom the cells are drawn with, how far the panel reaches, which
# columns become facets, and which colour scale is in force. Each is checked
# on the object, and the whole thing is then rendered to catch the warnings
# ggplot2 only raises at draw time.

fake_grid <- function(r = 8, res = 1) {
  g <- expand.grid(u = seq(-r, r, res), v = seq(-r, r, res))
  g <- g[sqrt(g$u^2 + g$v^2) <= r, ]
  g$z <- 100 - 8 * sqrt(g$u^2 + g$v^2) + g$u
  g
}

test_that("missing columns are reported by name", {
  d <- fake_grid()
  expect_error(polar_raster(d, z = "value"), "value")
  expect_error(polar_raster(data.frame(a = 1, b = 2)), "Available")
})

test_that("the panel reaches exactly r_out plus the compass margin", {
  d <- fake_grid()
  p <- polar_raster(d, grid = polar_grid(expand = 0.2))

  r_out <- max(sqrt(d$u^2 + d$v^2))
  expect_equal(p$coordinates$limits$x, c(-1, 1) * r_out * 1.2)
  expect_equal(p$coordinates$limits$y, c(-1, 1) * r_out * 1.2)
  expect_equal(p$coordinates$ratio, 1)      # a rose is round, or it is a lie
})

test_that("rings default to pretty values inside the data radius", {
  p <- polar_raster(fake_grid(r = 8))
  rings <- layer_data_of(p, "GeomPath")$.r

  expect_setequal(unique(rings), c(2, 4, 6, 8))
  expect_true(all(unique(rings) > 0))

  # ... and an explicit set wins, even beyond the data
  p2 <- polar_raster(fake_grid(r = 8), grid = polar_grid(rings = c(5, 10)))
  expect_setequal(unique(layer_data_of(p2, "GeomPath")$.r), c(5, 10))
  expect_equal(p2$coordinates$limits$x[2], 10 * 1.17)   # r_out follows the ring
})

test_that("an even grid is drawn with geom_raster, a gappy one with geom_tile", {
  even <- fake_grid()
  expect_equal(layer_geoms(polar_raster(even))[1], "GeomRaster")

  # gaps at some columns only: still on the grid, but no longer evenly spaced
  lattice <- even[!even$u %in% c(1, 5), ]
  expect_equal(layer_geoms(polar_raster(lattice))[1], "GeomTile")
  expect_silent(polar_raster(lattice))       # a lattice is normal, not news
})

test_that("an uneven grid falls back to geom_tile and says why", {
  d <- fake_grid()
  d$u[d$u == 3] <- 3.3                       # no longer on the grid

  expect_message(p <- polar_raster(d), "irregularly spaced")
  expect_equal(layer_geoms(p)[1], "GeomTile")
})

test_that("geom = raster/tile can be forced through the data attribute path", {
  # polar_sector() marks its output as polygons; polar_raster() must then use
  # the ready-made vertices instead of trying to grid them.
  sec <- polar_sector(fake_wind(), "nox", wd_res = 30, ws_res = 2)
  p   <- polar_raster(sec)

  expect_equal(layer_geoms(p)[1], "GeomPolygon")
  expect_renders_clean(p)
})

test_that("polygon data without .cell is refused", {
  sec <- polar_sector(fake_wind(), "nox", wd_res = 30, ws_res = 2)
  sec$.cell <- NULL
  expect_error(polar_raster(sec), "\\.cell")
})

test_that("facets follow what polar_bin recorded", {
  d <- fake_wind()
  d$site <- rep(c("A", "B"), length.out = nrow(d))
  b <- polar_bin(d, "nox", res = 1, type = "site")

  p <- polar_raster(b)
  expect_s3_class(p$facet, "FacetWrap")
  expect_equal(names(p$facet$params$facets), "site")

  # facet = NA switches faceting off without touching the data
  expect_s3_class(polar_raster(b, facet = NA)$facet, "FacetNull")
})

test_that("the default fill scale is magma, and an explicit scale replaces it", {
  p  <- polar_raster(fake_grid())
  sc <- p$scales$get_scales("fill")
  expect_equal(sc$palette(c(0, 0.5, 1)),
               ggplot2::scale_fill_viridis_c(option = "A")$palette(c(0, 0.5, 1)))

  # ... passed through to the default scale
  expect_equal(polar_raster(fake_grid(), name = "NOx")$scales$get_scales("fill")$name,
               "NOx")

  # `scale =` takes over entirely
  p2 <- polar_raster(fake_grid(), scale = scale_fill_capped(limits = c(0, 50)))
  expect_equal(p2$scales$get_scales("fill")$limits, c(0, 50))
})

test_that("grid = below puts the rings under the surface, not over it", {
  above <- layer_geoms(polar_raster(fake_grid()))
  below <- layer_geoms(polar_raster(fake_grid(), grid = polar_grid(below = TRUE)))

  expect_equal(above[1], "GeomRaster")
  expect_equal(below[length(below)], "GeomRaster")
})

test_that("the y scale carries the rings as breaks and the unit as labels", {
  p  <- polar_raster(fake_grid(), axis_unit = "m/s")
  sc <- p$scales$get_scales("y")

  expect_equal(sc$breaks, c(0, 2, 4, 6, 8))
  expect_equal(sc$labels(c(0, 2)), c("0 m/s", "2 m/s"))
  expect_equal(p$labels$y, NULL)                    # no axis title by default
  expect_equal(polar_raster(fake_grid(), axis_title = "Wind")$labels$y, "Wind")
})

test_that("a whole rose renders without warnings", {
  expect_renders_clean(polar_raster(fake_grid()))
  expect_renders_clean(polar_raster(polar_bin(fake_wind(), "nox", res = 1)))
  expect_renders_clean(polar_raster(fake_grid(), axis_labels = "centre"))
})


# --- polar_plot() --------------------------------------------------------

test_that("polar_plot checks args before it spends a minute smoothing", {
  d <- fake_wind()
  expect_error(polar_plot(d, pollutant = "nox", args = c(1, 2)), "named list")
  expect_error(polar_plot(d, pollutant = "nox", args = list(1)), "named")
  expect_error(polar_plot(d, pollutant = "nox", args = list(limitz = c(0, 1))),
               "limitz")
})

test_that("polar_plot draws openair's surface and keeps its data", {
  skip_if_not_installed("openair")
  p <- polar_plot(fake_wind(), pollutant = "nox")

  expect_s3_class(p, "ggplot")
  expect_s3_class(polar_data(p), "data.frame")
  expect_true(all(c("u", "v", "z") %in% names(polar_data(p))))

  # the recorded grid goes straight back in, without recomputation
  expect_s3_class(polar_raster(polar_data(p)), "ggplot")
  expect_null(polar_data(polar_raster(fake_grid())))
})
