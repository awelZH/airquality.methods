test_that("polar_map builds one raster layer per site and renders clean", {
  s <- fake_sites()
  p <- polar_map(fake_polar(s), s, radius = 300, quiet = TRUE, decor = plain_decor())

  expect_s3_class(p, "ggplot")
  expect_equal(sum(layer_geoms(p) == "GeomRaster"), 3L)
  expect_renders_clean(p)
})

test_that("all sites share exactly one fill scale", {
  s <- fake_sites()
  d <- fake_polar(s)
  d$z[d$site == "B"] <- d$z[d$site == "B"] + 500      # push one site's range out
  p <- polar_map(d, s, radius = 300, quiet = TRUE, decor = plain_decor())

  fills <- Filter(function(sc) "fill" %in% sc$aesthetics, p$scales$scales)
  expect_length(fills, 1L)

  b   <- ggplot2::ggplot_build(p)
  lim <- b$plot$scales$get_scales("fill")$get_limits()
  expect_gte(lim[2], max(d$z))
  expect_lte(lim[1], min(d$z))
})

test_that("the wind-speed scale is recorded and geometrically exact", {
  s <- fake_sites()
  p <- polar_map(fake_polar(s, r = 8), s, radius = 300, quiet = TRUE, decor = plain_decor())

  k <- attr(p, "polar_map_scale")
  expect_equal(k[["radius"]], 300)
  expect_equal(k[["r_out"]], 8)
  expect_equal(k[["k"]], 300 / 8)

  # The outer circle must sit exactly `radius` from each site centre.
  b  <- ggplot2::ggplot_build(p)
  oc <- which(layer_geoms(p) == "GeomPath")[2]        # rings, then outer circle
  per <- split(b$data[[oc]], b$data[[oc]]$group)
  for (q in per) {
    expect_equal((max(q$x) - min(q$x)) / 2, 300, tolerance = 1e-6)
    expect_equal((max(q$y) - min(q$y)) / 2, 300, tolerance = 1e-6)
  }
  centres <- sort(vapply(per, function(q) mean(range(q$x)), 0))
  expect_equal(unname(centres), sort(s$x_lv95), tolerance = 1e-6)
})

test_that("one shared k means one shared ruler, not per-site normalisation", {
  s <- fake_sites()
  d <- fake_polar(s)
  # Shrink one site's wind extent: its rose must get *smaller*, not rescale.
  d <- d[!(d$site == "C" & sqrt(d$u^2 + d$v^2) > 4), ]
  p <- polar_map(d, s, radius = 300, quiet = TRUE, decor = plain_decor())
  b <- ggplot2::ggplot_build(p)

  ri     <- which(layer_geoms(p) == "GeomRaster")
  widths <- vapply(ri, function(i) diff(range(b$data[[i]]$x)), 0)
  expect_lt(min(widths), max(widths))
})

test_that("r_max clips rather than rescales", {
  s <- fake_sites()
  p <- polar_map(fake_polar(s, r = 8), s, radius = 300, r_max = 4, quiet = TRUE, decor = plain_decor())

  expect_equal(attr(p, "polar_map_scale")[["r_out"]], 4)
  b  <- ggplot2::ggplot_build(p)
  ri <- which(layer_geoms(p) == "GeomRaster")
  for (i in ri) {
    d <- b$data[[i]]
    expect_lte(diff(range(d$x)) / 2, 300 + 1e-6)
  }
  # Every cell removed: needs data with no cell at the origin, otherwise the
  # centre cell (radius 0) always survives whatever r_max is set to.
  ring_only <- fake_polar(s)
  ring_only <- ring_only[sqrt(ring_only$u^2 + ring_only$v^2) >= 5, ]
  expect_error(polar_map(ring_only, s, radius = 300, r_max = 1, quiet = TRUE, decor = plain_decor()),
               "removes every cell")
})

test_that("polygon input becomes a single grouped layer", {
  s <- fake_sites()
  cell <- data.frame(
    .cell = rep(1:2, each = 4),
    u = c(0, 1, 1, 0, 1, 2, 2, 1),
    v = c(0, 0, 1, 1, 0, 0, 1, 1),
    z = rep(c(10, 20), each = 4)
  )
  d <- do.call(rbind, lapply(s$site, function(k) transform(cell, site = k)))
  attr(d, "polar_geom") <- "polygon"

  p <- polar_map(d, s, radius = 300, quiet = TRUE,
                 decor = polar_map_decor(key = FALSE, scalebar = FALSE))
  expect_equal(sum(layer_geoms(p) == "GeomPolygon"), 1L)

  b <- ggplot2::ggplot_build(p)
  expect_equal(length(unique(b$data[[1]]$group)), 6L)   # 3 sites x 2 cells
  expect_renders_clean(p)
})

test_that("polygon data without .cell is refused", {
  s <- fake_sites()
  d <- fake_polar(s)
  expect_error(polar_map(d, s, radius = 300, geom = "polygon", quiet = TRUE, decor = plain_decor()),
               "\\.cell")
})

test_that("na.rm drops missing cells so no grey square lands on the basemap", {
  s <- fake_sites()
  d <- fake_polar(s)
  d$z[1:20] <- NA

  # The fill column holds resolved colours, so a dropped cell shows up as a
  # missing *row*, and a kept one as the scale's opaque na.value — which is
  # exactly the grey square that would sit on the basemap.
  na_col <- ggplot2::scale_fill_viridis_c()$na.value

  p  <- polar_map(d, s, radius = 300, quiet = TRUE, decor = plain_decor())
  b  <- ggplot2::ggplot_build(p)
  ri <- which(layer_geoms(p) == "GeomRaster")
  expect_false(any(vapply(ri, function(i) any(b$data[[i]]$fill == na_col), TRUE)))

  keep <- polar_map(d, s, radius = 300, na.rm = FALSE, quiet = TRUE, decor = plain_decor())
  bk   <- ggplot2::ggplot_build(keep)
  rk   <- which(layer_geoms(keep) == "GeomRaster")
  expect_true(any(vapply(rk, function(i) any(bk$data[[i]]$fill == na_col), TRUE)))
})

test_that("a lattice grid falls back to geom_tile", {
  s <- fake_sites()
  d <- fake_polar(s, res = 1)
  d <- d[d$u %% 2 == 0, ]                    # every other column -> gaps

  p <- polar_map(d, s, radius = 300, quiet = TRUE, decor = plain_decor())
  expect_renders_clean(p)

  # Forcing raster is allowed but is the caller's problem, not a silent choice.
  expect_equal(sum(layer_geoms(
    polar_map(d, s, radius = 300, geom = "raster", quiet = TRUE, decor = plain_decor())) == "GeomRaster"), 3L)
})

test_that("the key rose warns when it lands on a site", {
  s <- fake_sites()
  d <- fake_polar(s)
  # A radius large enough that every corner is crowded. It also trips the
  # rose-overlap warning, so collect both rather than letting one leak.
  seen <- character(0)
  withCallingHandlers(
    polar_map(d, s, radius = 700, quiet = TRUE,
              decor = polar_map_decor(key = "bottomleft")),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("key rose overlaps", seen)))
  expect_true(any(grepl("Wind roses overlap", seen)))
  expect_silent(
    polar_map(d, s, radius = 300, quiet = TRUE,
              decor = polar_map_decor(key = FALSE, scalebar = FALSE))
  )
})

test_that("decor can be switched off entirely", {
  s <- fake_sites()
  p <- polar_map(fake_polar(s), s, radius = 300, quiet = TRUE,
                 decor = polar_map_decor(key = FALSE, scalebar = FALSE,
                                         labels = FALSE, marker = FALSE))
  g <- layer_geoms(p)
  expect_false("GeomLabel" %in% g)
  expect_false("GeomPoint" %in% g)
  expect_false("GeomRect" %in% g)
  expect_renders_clean(p)
})

test_that("missing columns are reported by name", {
  s <- fake_sites()
  d <- fake_polar(s)
  expect_error(polar_map(d[, c("u", "site")], s, radius = 300, quiet = TRUE, decor = plain_decor()),
               "\"v\"")
})

test_that("the roses carry no compass by default, the key does", {
  s <- fake_sites()
  d <- fake_polar(s)

  # key off: the compass letters would have to come from the roses, and there
  # are none
  p <- polar_map(d, s, radius = 300, quiet = TRUE, decor = plain_decor())
  expect_false("GeomText" %in% layer_geoms(p))

  # key on: exactly the four letters, once
  q <- polar_map(d, s, radius = 300, quiet = TRUE,
                 decor = polar_map_decor(key = c(2686000, 1254800),
                                         scalebar = FALSE, labels = FALSE))
  # The key draws its own text as geom_label -- the labels sit partly off
  # the plaque and need a backing over a basemap -- so look at both geoms.
  txt  <- Filter(function(l) inherits(l$geom, "GeomText") ||
                             inherits(l$geom, "GeomLabel"), q$layers)
  labs <- unlist(lapply(txt, function(l) l$data$.lab))
  # the key also carries its ring labels, so count the letters rather than
  # demanding they are all there is
  expect_equal(sum(labs %in% c("N", "E", "S", "W")), 4L)

  # and the two settings are independent of each other
  r <- polar_map(d, s, radius = 300, quiet = TRUE,
                 grid  = polar_grid(compass = "8"),
                 decor = polar_map_decor(key = FALSE, scalebar = FALSE,
                                         labels = FALSE))
  labs_r <- unlist(lapply(Filter(function(l) inherits(l$geom, "GeomText"), r$layers),
                          function(l) l$data$.lab))
  expect_equal(length(labs_r), 8L * nrow(s))
})

test_that("the swisstopo caption follows the basemap, not the decor", {
  s <- fake_sites()
  p <- polar_map(fake_polar(s), s, radius = 300, quiet = TRUE, decor = plain_decor())

  expect_null(p$labels$caption)                       # no basemap, nothing to cite
  expect_equal(polar_map_decor()$attribution, NULL)   # the decor stays neutral
})

test_that("the key carries no heading unless one is asked for", {
  s <- fake_sites()
  d <- fake_polar(s)
  key <- polar_map_decor(key = c(2686000, 1254800), scalebar = FALSE,
                         labels = FALSE)

  # The key\x27s ring labels and compass letters are geom_labels too, so the
  # heading is identified by its text rather than by its geom.
  labels_of <- function(p) {
    unname(unlist(lapply(Filter(function(l) inherits(l$geom, "GeomLabel"), p$layers),
                         function(l) l$data$.lab)))
  }
  heading_of <- function(p) setdiff(labels_of(p),
                                    c("N", "E", "S", "W",
                                      grep("m/s", labels_of(p), value = TRUE)))

  p <- polar_map(d, s, radius = 300, quiet = TRUE, decor = key)
  expect_length(heading_of(p), 0L)

  q <- polar_map(d, s, radius = 300, quiet = TRUE,
                 decor = polar_map_decor(key = c(2686000, 1254800),
                                         scalebar = FALSE, labels = FALSE,
                                         key_title = "wind speed"))
  expect_equal(heading_of(q), "wind speed")
})

test_that("r_max drops whole sector cells, never single vertices", {
  # A polygon row is a vertex, not a cell. Clipping row-wise leaves a cell
  # straddling r_max with its inner arc and no outer arc, which geom_polygon()
  # draws as a sliver -- a shape that is in the data nowhere.
  raw <- data.frame(ws = rep(seq(0.5, 8, by = 0.5), each = 40),
                    wd = rep(seq(0, 351, by = 9), 16), z = 1)
  s   <- fake_sites()[1, ]
  sec <- transform(polar_sector(raw, "z", wd_res = 30, ws_res = 1), site = s$site)

  p  <- polar_map(sec, s, radius = 300, r_max = 4, quiet = TRUE, decor = plain_decor())
  ld <- p$layers[[1]]$data

  expect_equal(unname(unique(table(ld$.cell))), 14L)   # every drawn cell complete
  expect_lte(max(sqrt(ld$u^2 + ld$v^2)), 4)
  expect_renders_clean(p)
})

test_that("r_max on raster input still clips cell by cell", {
  s   <- fake_sites()
  dat <- fake_polar(s)

  p  <- polar_map(dat, s, radius = 300, r_max = 4, quiet = TRUE, decor = plain_decor())
  ld <- p$layers[[1]]$data
  expect_lte(max(sqrt(ld$u^2 + ld$v^2)), 4)
})
