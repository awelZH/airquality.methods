# The preparation helpers decide three things that are invisible in the
# finished picture but wrong-or-right in the data: where the data frame comes
# from, which columns are facets, and whether the cells are evenly enough
# spaced for geom_raster(). All three fail silently if they fail at all, so
# they are tested directly rather than through a plot.

test_that(".polar_data unwraps openair objects, data frames, and lists", {
  df <- data.frame(u = 1, v = 2, z = 3)

  expect_equal(.polar_data(df), df)
  expect_equal(.polar_data(structure(list(data = df), class = "openair")), df)
  expect_equal(.polar_data(list(df, df)), df)          # first element of a list

  expect_error(.polar_data(list()), "no data")
  expect_error(.polar_data(1:10), "data frame")
  expect_error(.polar_data(structure(list(data = 1:3), class = "openair")),
               "data frame")
})

test_that(".polar_data returns a plain data frame, not a tibble or a grouped one", {
  skip_if_not_installed("dplyr")
  tb <- dplyr::group_by(dplyr::tibble(u = 1:2, v = 1:2, z = 1:2), u)

  out <- .polar_data(tb)
  expect_s3_class(out, "data.frame", exact = TRUE)
})

test_that(".polar_facets takes an explicit answer over guessing", {
  d <- data.frame(u = 1:4, v = 1:4, z = 1:4, season = c("a", "a", "b", "b"))

  expect_equal(.polar_facets(d, ~season, c("u", "v", "z")), "season")
  expect_equal(.polar_facets(d, "season", c("u", "v", "z")), "season")
  expect_equal(.polar_facets(d, NA, c("u", "v", "z")), character(0))
})

test_that(".polar_facets uses what polar_bin recorded, not what it can see", {
  d <- data.frame(u = 1:4, v = 1:4, z = 1:4,
                  season = c("a", "a", "b", "b"),
                  label  = c("x", "x", "y", "y"))
  attr(d, "polar_facets") <- "season"

  expect_equal(.polar_facets(d, NULL, c("u", "v", "z")), "season")

  # a recorded column that is no longer there must not become a facet
  attr(d, "polar_facets") <- c("season", "gone")
  expect_equal(.polar_facets(d, NULL, c("u", "v", "z")), "season")
})

test_that(".polar_facets guesses only when nothing was recorded", {
  # An empty recording means "nothing recorded" -- bind_rows() carries the
  # attribute of its first input along, so a hand-added label column has to
  # survive as a facet.
  d <- data.frame(u = 1:4, v = 1:4, z = 1:4,
                  label = c("x", "x", "y", "y"),
                  const = "same", n = 1:4)
  attr(d, "polar_facets") <- character(0)

  expect_equal(.polar_facets(d, NULL, c("u", "v", "z")), "label")   # not `const`, not `n`
})

test_that(".polar_grid_kind separates even spacing from gaps from noise", {
  expect_equal(.polar_grid_kind(seq(0, 10, by = 1)), "regular")
  expect_equal(.polar_grid_kind(c(0, 1, 2, 4, 5)), "lattice")   # gaps, still on a grid
  expect_equal(.polar_grid_kind(c(0, 1, 2.5)), "irregular")
  expect_equal(.polar_grid_kind(c(1, 2)), "regular")            # too short to judge
  expect_equal(.polar_grid_kind(c(0, 1, 2, NA, Inf)), "regular")
})

test_that(".polar_kind reports the worst group, not the first", {
  d <- data.frame(
    u   = c(0, 1, 2, 3,   0, 1, 2, 4),      # second group has a gap
    v   = rep(c(0, 1, 2, 3), 2),
    grp = rep(c("a", "b"), each = 4)
  )
  expect_equal(.polar_kind(d, "u", "v", factor(d$grp)), "lattice")

  d$u[8] <- 4.5                              # ... and now uneven
  expect_equal(.polar_kind(d, "u", "v", factor(d$grp)), "irregular")
})

test_that(".polar_axis_labels reads the lower half as a positive radius", {
  expect_equal(.polar_axis_labels(c(-4, 0, 4)), c("4", "0", "4"))
  expect_equal(.polar_axis_labels(c(2, 4), "m/s"), c("2 m/s", "4 m/s"))
  expect_equal(.polar_axis_labels(2, NULL), "2")
})
