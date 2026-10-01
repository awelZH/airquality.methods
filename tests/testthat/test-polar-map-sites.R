test_that(".nice_down rounds down to a readable step", {
  expect_equal(.nice_down(239), 200)
  expect_equal(.nice_down(204.5), 200)
  expect_equal(.nice_down(825), 750)
  expect_equal(.nice_down(1.2), 1)
  expect_equal(.nice_down(9.9), 7.5)
  expect_equal(.nice_down(1000), 1000)
})

test_that(".polar_map_radius halves the smallest spacing and allows the margin", {
  e <- c(0, 1000, 500)
  n <- c(0, 0, 1000)
  expect_equal(.polar_map_radius(e, n, expand = 0), 500)
  expect_equal(.polar_map_radius(e, n, expand = 0.25), 400)
  # A single site has no neighbour distance to work from.
  expect_null(.polar_map_radius(0, 0))
})

