# Test data and accessors for the polar plots and maps (moved from ufp25 with them).
#
# Every fixture here is deterministic and computed, not sampled: a test that asserts a cell mean
# has to be able to state that mean in closed form, and a seeded RNG only pushes the arithmetic
# somewhere the reader cannot see.

#' Three sites 1 km apart -- far enough that the default radius is comfortable.
fake_sites <- function() {
  data.frame(
    site   = c("A", "B", "C"),
    x_lv95 = c(2686000, 2687000, 2686500),
    y_lv95 = c(1256000, 1256000, 1257000),
    stringsAsFactors = FALSE
  )
}

#' A round u/v/z grid per site.
fake_polar <- function(sites = fake_sites(), res = 1, r = 8) {
  g <- expand.grid(u = seq(-r, r, res), v = seq(-r, r, res))
  g <- g[sqrt(g$u^2 + g$v^2) <= r, ]
  do.call(rbind, lapply(sites$site, function(k) {
    d <- g
    d$z <- 100 - 8 * sqrt(d$u^2 + d$v^2) + d$u
    d$site <- k
    d
  }))
}

#' Raw wind data on an exact wd x ws lattice.
#'
#' `reps` identical repetitions per (wd, ws) combination, with the value
#' offset by the repetition index. So the mean of any cell holding whole
#' repetitions is the base value plus `mean(seq_len(reps))`, and the count is
#' `reps` times the number of (wd, ws) pairs falling into it. Both are
#' checkable by hand, which is the point.
#'
#' The signal is a clean cosine peaking at wd = 90 (east), so a direction
#' effect that survives binning is visible as an asymmetry in u.
fake_wind <- function(reps = 3, wd_step = 10, ws_step = 0.5, ws_max = 8,
                      pollutant = "nox") {
  g <- expand.grid(rep = seq_len(reps),
                   ws  = seq(ws_step, ws_max, by = ws_step),
                   wd  = seq(0, 360 - wd_step, by = wd_step))
  out <- data.frame(
    date = as.POSIXct("2025-03-08 00:00:00", tz = "UTC") + 3600 * seq_len(nrow(g)),
    ws   = g$ws,
    wd   = g$wd
  )
  out[[pollutant]] <- 100 + 50 * cos((g$wd - 90) * pi / 180) + 5 * g$ws + g$rep
  out
}

#' Class names of a plot's layer geoms.
#'
#' Unnamed on purpose: ggplot2 names the layers, and a name that differs from
#' the geom ("geom_raster" vs "GeomRaster") turns every comparison into a
#' comparison of names as well.
layer_geoms <- function(p) {
  unname(vapply(p$layers, function(l) class(l$geom)[1], ""))
}

#' The data frame of the first layer whose geom matches.
layer_data_of <- function(p, geom, which = 1L) {
  i <- which(layer_geoms(p) == geom)
  expect_gte(length(i), which)
  p$layers[[i[which]]]$data
}

#' Decor with the corner furniture off.
#'
#' The fixture sites sit close together relative to the test radius, so the
#' key rose legitimately collides with them and says so. Tests that are not
#' about the key use this to keep that (correct) warning out of the way.
plain_decor <- function(...) polar_map_decor(key = FALSE, scalebar = FALSE, ...)
