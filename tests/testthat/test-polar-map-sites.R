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


test_that("transposed coordinates are an error, not a wrong map", {
  s <- fake_sites()
  expect_error(
    .polar_map_centres(s, "site", "y_lv95", "x_lv95"),
    "transposed"
  )
})

test_that("coordinates outside Switzerland warn", {
  # LV03 rather than LV95: the false easting/northing offsets are missing, so
  # the ordering is still right but the values land nowhere near Switzerland.
  s <- data.frame(site = c("A", "B"),
                  x_lv95 = c(686000, 687000),
                  y_lv95 = c(256000, 257000))
  expect_warning(.polar_map_centres(s, "site", "x_lv95", "y_lv95"),
                 "outside Switzerland")
})

test_that("missing and duplicated sites are rejected", {
  s <- fake_sites()
  expect_error(.polar_map_centres(s, "site", "nope", "y_lv95"), "not found")

  s2 <- s; s2$x_lv95[2] <- NA
  expect_error(.polar_map_centres(s2, "site", "x_lv95", "y_lv95"), "Missing coordinates")

  s3 <- rbind(s, s[1, ])
  expect_error(.polar_map_centres(s3, "site", "x_lv95", "y_lv95"), "Duplicated")
})

test_that("radius resolution reports, warns, and aborts appropriately", {
  ce <- .polar_map_centres(fake_sites(), "site", "x_lv95", "y_lv95")

  expect_message(r <- .polar_map_resolve_radius(NULL, ce, 0.17), "Using")
  expect_equal(r, .nice_down(.polar_map_radius(ce$e, ce$n, 0.17)))

  expect_silent(.polar_map_resolve_radius(NULL, ce, 0.17, quiet = TRUE))

  # A user-supplied radius larger than the fit warns but is honoured.
  expect_warning(out <- .polar_map_resolve_radius(5000, ce, 0.17), "overlap")
  expect_equal(out, 5000)

  expect_error(.polar_map_resolve_radius(NULL, ce[1, ], 0.17), "only one row")
  expect_error(.polar_map_resolve_radius(-1, ce, 0.17), "positive")
})

test_that("the join checks both directions", {
  ce  <- .polar_map_centres(fake_sites(), "site", "x_lv95", "y_lv95")
  dat <- fake_polar()

  expect_silent(.polar_map_join(dat, ce, "site", quiet = TRUE))

  # A site with data but no coordinates cannot be placed.
  bad <- dat; bad$site[1] <- "Z"
  expect_error(.polar_map_join(bad, ce, "site"), "No coordinates")

  # A site with coordinates but no data is fine — mapping a subset is normal.
  sub <- dat[dat$site != "C", ]
  expect_message(j <- .polar_map_join(sub, ce, "site"), "not drawn")
  expect_equal(nrow(j$centres), 2L)

  expect_error(.polar_map_join(dat[, c("u", "v", "z")], ce, "site"), "not found")
})

test_that("named lists bind into the long form", {
  d <- fake_polar()
  lst <- split(d[, c("u", "v", "z")], d$site)
  out <- .polar_map_bind(lst, "site")
  expect_true("site" %in% names(out))
  expect_setequal(unique(out$site), c("A", "B", "C"))

  expect_error(.polar_map_bind(stats::setNames(lst, NULL), "site"), "must be named")
  expect_error(.polar_map_bind(1:3, "site"), "must be a data frame")
})

test_that("binding a named list keeps the recorded facets, not just the geom", {
  # bind_rows() happens to carry the first element's attributes along, but
  # that is undocumented dplyr behaviour: polar_facets decides what the plot
  # facets by, so it is re-set explicitly rather than inherited by luck.
  one <- polar_bin(transform(fake_wind(), season = "summer"), "nox",
                   res = 1, type = "season")
  two <- polar_bin(transform(fake_wind(), season = "winter"), "nox",
                   res = 1, type = "season")

  out <- .polar_map_bind(list(A = one, B = two), "site")

  expect_equal(attr(out, "polar_facets"), "season")
  expect_setequal(unique(out$site), c("A", "B"))
})

test_that("binding sector data keeps the polygon marker", {
  sec <- polar_sector(fake_wind(), "nox", wd_res = 30, ws_res = 2)
  out <- .polar_map_bind(list(A = sec, B = sec), "site")

  expect_equal(attr(out, "polar_geom"), "polygon")
  expect_false(attr(out, "polar_interpolate"))
})
