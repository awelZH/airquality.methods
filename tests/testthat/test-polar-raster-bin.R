# Binning is the statistical core of the package: everything drawn later is
# only as true as the cell means computed here. So these tests state the
# expected numbers in closed form rather than re-implementing the binning and
# comparing it with itself.

test_that("polar_bin puts each measurement in the cell its wind vector points to", {
  # wd is the direction the wind blows FROM, in compass degrees, so
  # wd = 90 (east) is u = +ws, v = 0.
  d <- data.frame(ws = c(4, 4, 4, 2),
                  wd = c(90, 90, 270, 0),
                  nox = c(10, 20, 5, 7))

  out <- polar_bin(d, "nox", res = 1)

  # ordered by v, then u -- the documented order
  expect_equal(out, data.frame(u = c(-4, 4, 0), v = c(0, 0, 2),
                               z = c(5, 15, 7), n = c(1L, 2L, 1L)),
               ignore_attr = TRUE)
  expect_equal(attr(out, "polar_facets"), character(0))
  expect_false(attr(out, "polar_interpolate"))
})

test_that("cell centres are exact multiples of res, whatever the data", {
  for (res in c(0.25, 0.5, 1, 2)) {
    out <- polar_bin(fake_wind(), "nox", res = res)
    expect_equal(out$u %% res, rep(0, nrow(out)))
    expect_equal(out$v %% res, rep(0, nrow(out)))
  }
})

test_that("every complete row lands in exactly one cell", {
  d   <- fake_wind()
  out <- polar_bin(d, "nox", res = 1)
  expect_equal(sum(out$n), nrow(d))
})

test_that("the statistic argument selects the summary actually applied", {
  d <- data.frame(ws = rep(4, 4), wd = rep(90, 4), nox = c(1, 2, 3, 10))

  expect_equal(polar_bin(d, "nox", res = 1)$z, 4)                       # mean
  expect_equal(polar_bin(d, "nox", res = 1, statistic = "median")$z, 2.5)
  expect_equal(polar_bin(d, "nox", res = 1, statistic = "min")$z, 1)
  expect_equal(polar_bin(d, "nox", res = 1, statistic = "max")$z, 10)
  expect_equal(polar_bin(d, "nox", res = 1, statistic = "sd")$z,
               stats::sd(c(1, 2, 3, 10)))
  expect_equal(polar_bin(d, "nox", res = 1, statistic = "n")$z, 4)
  expect_equal(polar_bin(d, "nox", res = 1, statistic = "frequency")$z, 4)
  expect_equal(polar_bin(d, "nox", res = 1,
                         statistic = function(x) unname(stats::quantile(x, 0.75)))$z,
               unname(stats::quantile(c(1, 2, 3, 10), 0.75)))
})

test_that("statistic = 'n' fills z with the same count as n", {
  out <- polar_bin(fake_wind(), "nox", res = 1, statistic = "n")
  expect_equal(out$z, as.numeric(out$n))
})

test_that("an unknown statistic is named in the error", {
  expect_error(polar_bin(fake_wind(), "nox", statistic = "modus"), "modus")
  expect_error(polar_statfun("modus"), "Unknown")
  expect_type(polar_statfun(function(x) 1), "closure")
})

test_that("min_n drops thin cells and says so when nothing survives", {
  out <- polar_bin(fake_wind(reps = 3), "nox", res = 1, min_n = 5)
  expect_true(all(out$n >= 5))
  expect_lt(nrow(out), nrow(polar_bin(fake_wind(reps = 3), "nox", res = 1)))

  expect_error(polar_bin(fake_wind(reps = 1), "nox", res = 0.1, min_n = 1000),
               "min_n")
})

test_that("ws_max removes the measurements, not just the cells", {
  d   <- fake_wind()
  out <- polar_bin(d, "nox", res = 1, ws_max = 4)

  expect_equal(sum(out$n), sum(d$ws <= 4))
  # the cell centre may round outward by half a cell, but no further
  expect_lte(max(sqrt(out$u^2 + out$v^2)), 4 + sqrt(2) * 0.5)
})

test_that("incomplete rows are dropped, and an empty result is an error", {
  d <- data.frame(ws = c(4, NA, 4, 4), wd = c(90, 90, NA, 90),
                  nox = c(10, 10, 10, NA))
  out <- polar_bin(d, "nox", res = 1)
  expect_equal(out$n, 1L)

  expect_error(polar_bin(data.frame(ws = NA_real_, wd = NA_real_, nox = NA_real_),
                         "nox"), "Nothing left")
})

test_that("missing columns are reported by name, with the available ones", {
  d <- fake_wind()
  expect_error(polar_bin(d, "no2"), "no2")
  expect_error(polar_bin(d, "nox", ws = "speed"), "speed")
  expect_error(polar_bin(d, "nox"), NA)          # sanity: the fixture is fine
  expect_error(polar_bin(as.list(d), "nox"), "data frame")
})

test_that("res must be a single positive number", {
  d <- fake_wind()
  expect_error(polar_bin(d, "nox", res = 0), "res")
  expect_error(polar_bin(d, "nox", res = -1), "res")
  expect_error(polar_bin(d, "nox", res = c(1, 2)), "res")
  expect_error(polar_bin(d, "nox", res = "1"), "res")
})

test_that("type groups the result and records the grouping for the facets", {
  d <- fake_wind()
  d$site <- rep(c("A", "B"), length.out = nrow(d))

  out <- polar_bin(d, "nox", res = 1, type = "site")

  expect_true("site" %in% names(out))
  expect_setequal(unique(out$site), c("A", "B"))
  expect_equal(attr(out, "polar_facets"), "site")
  expect_equal(sum(out$n), nrow(d))
})

test_that("a derived type is cut by openair rather than refused", {
  skip_if_not_installed("openair")
  out <- polar_bin(fake_wind(), "nox", res = 1, type = "daylight")

  expect_true("daylight" %in% names(out))
  expect_equal(attr(out, "polar_facets"), "daylight")
})

test_that("wind directions wrap: 0 and 360 are the same direction", {
  d <- data.frame(ws = c(3, 3), wd = c(0, 360), nox = c(10, 20))
  out <- polar_bin(d, "nox", res = 1)

  expect_equal(nrow(out), 1L)
  expect_equal(out$z, 15)
})


# --- polar_sector ------------------------------------------------------------

test_that("polar_sector returns closed ring segments with a cell id", {
  out <- polar_sector(fake_wind(), "nox", wd_res = 30, ws_res = 2)

  n_arc <- max(2L, ceiling(30 / 5) + 1L)
  expect_equal(nrow(out), length(unique(out$.cell)) * 2L * n_arc)
  expect_equal(sort(unique(out$.cell)), seq_len(length(unique(out$.cell))))
  expect_equal(attr(out, "polar_geom"), "polygon")
  expect_false(attr(out, "polar_interpolate"))

  # z and n are properties of the cell, so they must not vary within one
  expect_true(all(vapply(split(out$z, out$.cell),
                         function(x) length(unique(x)) == 1L, logical(1))))
})

test_that("every vertex lies inside its own speed class and direction sector", {
  ws_res <- 2
  wd_res <- 30
  out <- polar_sector(fake_wind(), "nox", wd_res = wd_res, ws_res = ws_res)

  r <- sqrt(out$u^2 + out$v^2)
  # each cell spans exactly one speed class, so its vertex radii take two
  # values ws_res apart (for the innermost band the inner arc is the origin)
  spans <- vapply(split(round(r, 8), out$.cell), function(x) diff(range(x)),
                  numeric(1))
  expect_equal(unname(spans), rep(ws_res, length(spans)))
  expect_equal(min(r), 0)

  # the drawn sector is exactly wd_res wide
  width <- vapply(split(seq_len(nrow(out)), out$.cell), function(i) {
    keep <- sqrt(out$u[i]^2 + out$v[i]^2) > 1e-8
    a <- atan2(out$u[i][keep], out$v[i][keep]) * 180 / pi
    a <- (a - a[1] + 540) %% 360 - 180          # unwrap around the first vertex
    diff(range(a))
  }, numeric(1))
  expect_equal(unname(round(width, 6)), rep(wd_res, length(width)))
})

test_that("n_arc controls how finely the arcs are drawn", {
  a <- polar_sector(fake_wind(), "nox", wd_res = 30, ws_res = 2, n_arc = 2)
  b <- polar_sector(fake_wind(), "nox", wd_res = 30, ws_res = 2, n_arc = 20)

  expect_equal(nrow(a) / length(unique(a$.cell)), 4)     # 2 * n_arc
  expect_equal(nrow(b) / length(unique(b$.cell)), 40)
})

test_that("sector cells summarise the same measurements the bins do", {
  d <- data.frame(ws = c(1.5, 1.5, 1.5), wd = c(0, 5, 355), nox = c(10, 20, 30))
  out <- polar_sector(d, "nox", wd_res = 30, ws_res = 1)

  expect_equal(length(unique(out$.cell)), 1L)   # all three in the north sector
  expect_equal(unique(out$z), 20)               # mean of 10, 20, 30
  expect_equal(unique(out$n), 3L)
})

test_that("wd_res and ws_res are checked before anything is computed", {
  d <- fake_wind()
  expect_error(polar_sector(d, "nox", wd_res = 7), "divide 360")
  expect_error(polar_sector(d, "nox", wd_res = 200), "between 0 and 180")
  expect_error(polar_sector(d, "nox", wd_res = 0), "between 0 and 180")
  expect_error(polar_sector(d, "nox", ws_res = 0), "ws_res")
  expect_error(polar_sector(d, "nox", ws_res = c(1, 2)), "ws_res")
  expect_error(polar_sector(d, "nox", wd_res = 22.5), NA)   # divides 360
})

test_that("min_n and type behave as they do for polar_bin", {
  d <- fake_wind()
  d$site <- rep(c("A", "B"), length.out = nrow(d))

  out <- polar_sector(d, "nox", wd_res = 30, ws_res = 2, min_n = 10, type = "site")

  expect_true(all(out$n >= 10))
  expect_equal(attr(out, "polar_facets"), "site")
  expect_setequal(unique(out$site), c("A", "B"))
})


# --- tie handling and argument robustness ------------------------------------
# Both regressions found by review: a value sitting exactly on a cell boundary
# is common here (wind directions come in fixed steps), so the tie rule is not
# a detail.

test_that("sector binning treats every sector alike when directions tie", {
  # wd_res as a multiple of the data step puts every second direction exactly
  # on a sector boundary. Round-half-to-even sends those ties alternately left
  # and right, which leaves neighbouring sectors with a 3:1 count ratio on
  # perfectly uniform data.
  d <- expand.grid(wd = seq(0, 350, by = 10), rep = 1:10)
  d$ws <- 3
  d$z  <- 1

  cell <- unique(polar_sector(d, "z", wd_res = 20, ws_res = 1)[, c(".cell", "n")])

  expect_length(unique(cell$n), 1L)
  expect_equal(sum(cell$n), nrow(d))
})

test_that("bin cells share the boundary values evenly", {
  # ws on a half-cell ladder: every value sits exactly between two cell
  # centres, so every cell must end up with the same count.
  d <- data.frame(ws = seq(0.25, 4, by = 0.25), wd = 90, z = 1)

  out <- polar_bin(d, "z", res = 0.5)

  expect_length(unique(out$n), 1L)
  expect_equal(sum(out$n), nrow(d))
})

test_that("a tie is resolved the same way on both sides of the origin", {
  d <- data.frame(ws = rep(1, 4), wd = c(90, 270, 0, 180), z = 1)
  out <- polar_bin(d, "z", res = 2)

  expect_setequal(out$u, c(2, -2, 0, 0))
  expect_setequal(out$v, c(0, 0, 2, -2))
})

test_that("ws_max accepts NULL as no limit and refuses nonsense", {
  d <- fake_wind()

  expect_equal(polar_bin(d, "nox", res = 1, ws_max = NULL),
               polar_bin(d, "nox", res = 1), ignore_attr = TRUE)
  expect_equal(polar_sector(d, "nox", wd_res = 30, ws_max = NULL),
               polar_sector(d, "nox", wd_res = 30), ignore_attr = TRUE)

  expect_error(polar_bin(d, "nox", ws_max = "4"), "ws_max")
  expect_error(polar_bin(d, "nox", ws_max = c(4, 8)), "ws_max")
})

test_that("polar_statfun() is the statistic the binning uses, for other callers too", {
  # Exported so that analyses condensing values per direction (e.g. ufp25's
  # wd_extreme()) mean by "median" exactly what polar_bin() means by it.
  expect_true("polar_statfun" %in% getNamespaceExports("airquality.methods"))
  expect_equal(polar_statfun("median")(c(1, NA, 3, 10)), 3)
  expect_equal(polar_statfun("mean")(c(1, NA, 3)), 2)
  expect_equal(polar_statfun("n")(c(1, NA, 3)), 3L)
})
