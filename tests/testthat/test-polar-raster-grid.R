# polar_grid() is a settings object, so the tests are about what a setting
# turns into on the plot -- and about the themes, whose whole job is to be
# read by something else later.

test_that("polar_grid keeps every setting it was given", {
  g <- polar_grid(rings = c(2, 4), n = 3, spokes = c(0, 180), compass = FALSE,
                  outer = FALSE, below = TRUE, expand = 0.3)

  expect_equal(g$rings, c(2, 4))
  expect_equal(g$n, 3)
  expect_equal(g$spokes, c(0, 180))
  expect_false(g$compass)
  expect_false(g$outer)
  expect_true(g$below)
  expect_equal(g$expand, 0.3)

  # the appearance fields stay empty until a theme fills them in
  expect_null(g$colour)
  expect_null(g$linewidth)
  expect_null(g$linetype)
})

test_that("polar_grid defaults are the ones the examples describe", {
  g <- polar_grid()
  expect_equal(g$spokes, seq(0, 315, by = 45))
  expect_true(g$compass)
  expect_equal(g$compass_fontface, "plain")     # not bold: it is a label, not a title
  expect_equal(g$expand, 0.17)
  expect_equal(g$outer_linetype, 1)
})

test_that(".polar_compass resolves the four spellings of a compass", {
  expect_equal(.polar_compass(TRUE), c(N = 0, E = 90, S = 180, W = 270))
  expect_equal(.polar_compass("4"),  c(N = 0, E = 90, S = 180, W = 270))
  expect_equal(.polar_compass("8"),
               c(N = 0, NE = 45, E = 90, SE = 135, S = 180, SW = 225,
                 W = 270, NW = 315))
  expect_equal(.polar_compass(FALSE), numeric(0))
  expect_equal(.polar_compass(NULL),  numeric(0))
  expect_equal(.polar_compass(c(0, 120, 240)), c(0, 120, 240))
})

test_that("the compass is drawn outside the outer ring, in the expand margin", {
  d <- expand.grid(u = seq(-4, 4, 1), v = seq(-4, 4, 1))
  d <- d[sqrt(d$u^2 + d$v^2) <= 4, ]
  d$z <- 1

  p  <- polar_raster(d, grid = polar_grid(compass = "8", expand = 0.2))
  cd <- layer_data_of(p, "GeomText")

  expect_equal(nrow(cd), 8L)
  expect_equal(unique(round(sqrt(cd$.u^2 + cd$.v^2), 8)), 4 * 1.1)  # r_out * (1 + expand/2)
  expect_setequal(cd$.lab, c("N", "NE", "E", "SE", "S", "SW", "W", "NW"))
})

test_that(".polar_circle closes and has the requested radius", {
  c1 <- .polar_circle(3)
  expect_equal(unique(round(sqrt(c1$.u^2 + c1$.v^2), 10)), 3)
  expect_equal(c1[1, c(".u", ".v")], c1[nrow(c1), c(".u", ".v")], ignore_attr = TRUE)
  expect_equal(unique(c1$.r), 3)
  expect_equal(nrow(.polar_circle(1, n = 5L)), 5L)
})

test_that(".polar_theme completes a partial theme instead of failing on it", {
  bare <- ggplot2::theme(polar.grid = ggplot2::element_line(colour = "red"))

  expect_true(isTRUE(attr(.polar_theme(bare), "complete")))
  expect_equal(ggplot2::calc_element("polar.grid", .polar_theme(bare))$colour, "red")
  expect_true(isTRUE(attr(.polar_theme(NULL), "complete")))
})


# --- themes ---------------------------------------------------------------

test_that("theme_polar carries the polar grid and the radial axis", {
  th <- theme_polar(axis_colour = "firebrick", grid_colour = "steelblue",
                    grid_linewidth = 0.5, grid_linetype = 3)

  expect_true(isTRUE(attr(th, "complete")))
  grid_el <- ggplot2::calc_element("polar.grid", th)
  expect_equal(grid_el$colour, "steelblue")
  expect_equal(grid_el$linewidth, 0.5)
  expect_equal(grid_el$linetype, 3)

  expect_equal(ggplot2::calc_element("axis.line.y", th)$colour, "firebrick")
  expect_equal(ggplot2::calc_element("axis.text.y", th)$colour, "firebrick")
  # grid_colour defaults to axis_colour, so one argument restyles both
  expect_equal(ggplot2::calc_element("polar.grid", theme_polar(axis_colour = "red"))$colour,
               "red")
})

test_that("theme_polar_map hides the coordinate axes unless asked", {
  off <- theme_polar_map()
  on  <- theme_polar_map(axes = TRUE)

  expect_s3_class(ggplot2::calc_element("axis.text.x", off), "element_blank")
  expect_s3_class(ggplot2::calc_element("axis.text.x", on), "element_text")
  expect_equal(ggplot2::calc_element("axis.title.y", on)$angle, 90)

  # the grid is darker than on a plain background, because it lies on a basemap
  expect_equal(ggplot2::calc_element("polar.grid", off)$colour, "grey20")
  expect_gt(ggplot2::calc_element("polar.grid", off)$linewidth,
            ggplot2::calc_element("polar.grid", theme_polar())$linewidth)
})

test_that("the caption element is set on both themes -- it carries the attribution", {
  expect_equal(ggplot2::calc_element("plot.caption", theme_polar_map())$hjust, 1)
  expect_s3_class(ggplot2::calc_element("plot.caption", theme_polar()), "element_text")
})
