# The radial axis and the themed polar grid: both are places where the drawn
# result and the theme can drift apart silently, so both are checked against
# the theme rather than against a picture.

fake_grid <- function(r = 8, res = 1) {
  g <- expand.grid(u = seq(-r, r, res), v = seq(-r, r, res))
  g <- g[sqrt(g$u^2 + g$v^2) <= r, ]
  g$z <- 100 - 8 * sqrt(g$u^2 + g$v^2) + g$u
  g
}

geoms <- function(p) vapply(p$layers, function(l) class(l$geom)[1], "")

test_that("axis_labels accepts TRUE/FALSE and the position strings", {
  expect_equal(.polar_axis_position(TRUE),     "left")
  expect_equal(.polar_axis_position(FALSE),    "none")
  expect_equal(.polar_axis_position("left"),   "left")
  expect_equal(.polar_axis_position("centre"), "centre")
  expect_equal(.polar_axis_position("center"), "centre")
  expect_equal(.polar_axis_position("none"),   "none")
  expect_error(.polar_axis_position("middle"), "axis_labels")
})

test_that("the left axis stays the plot's real y-axis", {
  p <- polar_raster(fake_grid())
  b <- ggplot2::ggplot_build(p)

  expect_s3_class(b$plot$scales$get_scales("y")$guide, "Guide")
  expect_false("GeomLabel" %in% geoms(p))
})

test_that("axis_labels = 'centre' moves the axis into the panel", {
  p <- polar_raster(fake_grid(), axis_labels = "centre")

  # exactly one added layer: the ring labels. No axis line, no ticks -- the
  # rings are the ticks, so drawing both would be drawing it twice.
  expect_equal(setdiff(geoms(p), geoms(polar_raster(fake_grid()))), "GeomLabel")
  expect_length(geoms(p), length(geoms(polar_raster(fake_grid()))) + 1L)
  expect_equal(ggplot2::ggplot_build(p)$plot$scales$get_scales("y")$guide, "none")

  # it labels the same rings the grid draws, on the ring itself
  lab <- p$layers[[which(geoms(p) == "GeomLabel")]]$data
  expect_equal(lab$.lab, .polar_axis_labels(c(2, 4, 6, 8), "m/s"))
  expect_equal(sqrt(lab$.u^2 + lab$.v^2), c(2, 4, 6, 8))
  expect_equal(lab$.u, rep(0, 4))                      # bearing 0 = up the N spoke
})

test_that("axis_labels = FALSE draws no axis at all", {
  p <- polar_raster(fake_grid(), axis_labels = FALSE)
  expect_false("GeomLabel" %in% geoms(p))
  expect_equal(ggplot2::ggplot_build(p)$plot$scales$get_scales("y")$guide, "none")
})

test_that("the centred axis takes its styling from the theme's axis elements", {
  th <- theme_polar(axis_colour = "firebrick")
  p  <- polar_raster(fake_grid(), axis_labels = "centre", theme = th)
  lab <- p$layers[[which(geoms(p) == "GeomLabel")]]

  expect_equal(lab$aes_params$colour, "firebrick")
  # the transparent plot.background of theme_void() must not become the fill
  expect_equal(lab$aes_params$fill, "white")
})

test_that("the grid reads polar.grid, and an explicit setting still wins", {
  th <- theme_polar(grid_colour = "white", grid_linewidth = 0.9, grid_linetype = 1)

  g1 <- .polar_grid_style(polar_grid(), th)
  expect_equal(g1[c("colour", "linewidth", "linetype")],
               list(colour = "white", linewidth = 0.9, linetype = 1))

  g2 <- .polar_grid_style(polar_grid(colour = "grey20"), th)
  expect_equal(g2$colour, "grey20")           # explicit beats the theme ...
  expect_equal(g2$linewidth, 0.9)             # ... field by field

  # theme(polar.grid = ) is the other route to the same place
  g3 <- .polar_grid_style(
    polar_grid(),
    theme_polar() + ggplot2::theme(polar.grid = ggplot2::element_line(colour = "grey5")))
  expect_equal(g3$colour, "grey5")
})

test_that("a theme without polar.grid falls back instead of drawing nothing", {
  g <- .polar_grid_style(polar_grid(), ggplot2::theme_void())
  expect_equal(g[c("colour", "linewidth", "linetype")],
               list(colour = "grey35", linewidth = 0.25, linetype = 2))
})

test_that("polar_map's roses inherit the map theme's grid", {
  s <- fake_sites()
  p <- polar_map(fake_polar(s), s, radius = 300, quiet = TRUE,
                 decor = plain_decor(), grid = polar_grid(compass = FALSE))

  rings <- p$layers[[which(geoms(p) == "GeomPath")[1]]]
  expect_equal(rings$aes_params$colour, "grey20")     # theme_polar_map()'s value
})
