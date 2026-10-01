# The decor is everything on the map that is not a rose: site names, the key
# rose, the scale bar, the attribution. It is furniture, so the tests are
# about placement and about the defaults -- the two things a reader notices
# immediately and no error message ever reports.

decor_layers <- function(p, geom) {
  i <- which(layer_geoms(p) == geom)
  lapply(i, function(k) p$layers[[k]])
}

# Both geoms: the key draws its own text as geom_label (it needs a backing
# where it runs off the plaque), the roses' compass as geom_text.
text_labels <- function(p) {
  unlist(lapply(c(decor_layers(p, "GeomText"), decor_layers(p, "GeomLabel")),
                function(l) c(l$aes_params$label, l$data$label, l$data$.lab)))
}

map_plot <- function(..., radius = 300) {
  s <- fake_sites()
  polar_map(fake_polar(s), s, radius = radius, quiet = TRUE, ...)
}

test_that("the defaults are the ones the figures are drawn with", {
  d <- polar_map_decor()

  expect_equal(d$key, "bottomleft")
  expect_null(d$key_title)                 # the rings carry the unit already
  expect_true(d$key_compass)
  expect_equal(d$scalebar, "bottomright")
  expect_true(d$labels)                    # = above
  expect_equal(d$label_gap, 0.06)
  expect_equal(d$axis_unit, "m/s")
  expect_null(d$attribution)               # taken from the basemap when there is one
})

test_that("labels default to sitting above the rose", {
  expect_equal(.polar_map_label_position(TRUE),    "above")
  expect_equal(.polar_map_label_position("above"), "above")
  expect_equal(.polar_map_label_position("below"), "below")
  expect_equal(.polar_map_label_position(FALSE),   "none")
  expect_equal(.polar_map_label_position("none"),  "none")
  expect_error(.polar_map_label_position("left"), "labels")
  expect_error(.polar_map_label_position(c(TRUE, TRUE)), "labels")
})

test_that("the site name is offset from the outer ring by label_gap", {
  s <- fake_sites()
  radius <- 300

  above <- map_plot(decor = polar_map_decor(key = FALSE, scalebar = FALSE))
  lab   <- decor_layers(above, "GeomLabel")[[1]]$data
  expect_equal(lab$.N - lab$n, rep(radius * 1.06, nrow(s)))

  below <- map_plot(decor = polar_map_decor(key = FALSE, scalebar = FALSE,
                                            labels = "below"))
  lab2  <- decor_layers(below, "GeomLabel")[[1]]$data
  expect_equal(lab2$n - lab2$.N, rep(radius * 1.06, nrow(s)))

  # a bigger gap moves it further out, in the same direction
  wide <- map_plot(decor = polar_map_decor(key = FALSE, scalebar = FALSE,
                                           label_gap = 0.5))
  lab3 <- decor_layers(wide, "GeomLabel")[[1]]$data
  expect_gt(lab3$.N[1] - lab3$n[1], lab$.N[1] - lab$n[1])
})

test_that("labels and markers can be switched off separately", {
  no_lab <- map_plot(decor = polar_map_decor(key = FALSE, scalebar = FALSE,
                                             labels = FALSE))
  expect_false("GeomLabel" %in% layer_geoms(no_lab))
  expect_true("GeomPoint" %in% layer_geoms(no_lab))

  no_mark <- map_plot(decor = polar_map_decor(key = FALSE, scalebar = FALSE,
                                              marker = FALSE))
  expect_false("GeomPoint" %in% layer_geoms(no_mark))
  expect_true("GeomLabel" %in% layer_geoms(no_mark))
})


# --- the key rose ---------------------------------------------------------

test_that("the key labels only as many rings as it has room for", {
  # A key is a small disc with text of a fixed point size in it: with five
  # rings the labels overprint one another. Every ring is still drawn.
  ring_labels <- function(...) {
    p <- map_plot(radius = 100,
                  decor = polar_map_decor(key = "topright", scalebar = FALSE,
                                          ...),
                  grid = polar_grid(rings = c(2, 4, 6, 8, 10)))
    grep("m/s", text_labels(p), value = TRUE)
  }

  expect_length(ring_labels(), 3L)                 # the default, key_labels = 3
  expect_true("10 m/s" %in% ring_labels())         # counted from the rim inwards
  expect_length(ring_labels(key_labels = 2), 2L)
  expect_length(ring_labels(key_labels = Inf), 5L)
})

test_that("a key drawn smaller keeps every part of its geometry in step", {
  # key_scale makes the key a legend rather than a ruler, which is a
  # deliberate trade on a map of one rose -- but the rings, spokes, ticks
  # and compass letters must all shrink by the same number.
  radii <- function(scale) {
    p <- map_plot(radius = 100,
                  decor = polar_map_decor(key = "topright", scalebar = FALSE,
                                          key_scale = scale))
    seg <- Filter(function(l) all(c(".E", ".N", ".E1", ".N1") %in% names(l$data)),
                  decor_layers(p, "GeomSegment"))
    sp  <- seg[[which(vapply(seg, function(l) nrow(l$data), integer(1)) == 8L)[1]]]$data
    max(sqrt((sp$.E1 - sp$.E)^2 + (sp$.N1 - sp$.N)^2))
  }

  expect_equal(radii(0.5) / radii(1), 0.5, tolerance = 1e-6)
})


test_that("the key rose carries the same direction spokes as the data roses", {
  p  <- map_plot(radius = 100, decor = polar_map_decor(key = "topright", scalebar = FALSE),
                 grid = polar_grid(spokes = c(0, 90, 180, 270)))
  seg <- decor_layers(p, "GeomSegment")
  seg <- Filter(function(l) all(c(".E", ".N", ".E1", ".N1") %in% names(l$data)), seg)
  rows <- vapply(seg, function(l) nrow(l$data), integer(1))

  # Four spokes per data rose in one layer, and four more for the key: the
  # key is a rose and not a plaque, and it is the same four bearings.
  expect_true(any(rows == 4L * nrow(fake_sites())))
  expect_true(any(rows == 4L))

  # with a north spoke present, the key draws no separate axis line: the
  # spoke is that line. Without one, it draws its own.
  n_seg <- function(spokes) {
    q <- map_plot(radius = 100, decor = polar_map_decor(key = "topright", scalebar = FALSE),
                  grid = polar_grid(spokes = spokes))
    length(decor_layers(q, "GeomSegment"))
  }
  expect_equal(n_seg(c(45, 135, 225, 315)), n_seg(c(0, 90, 180, 270)) + 1L)
})

test_that("the key plaque is a fill without an outline", {
  p    <- map_plot(radius = 100, decor = polar_map_decor(key = "topright", scalebar = FALSE))
  disc <- decor_layers(p, "GeomPolygon")

  expect_length(disc, 1L)
  expect_equal(disc[[1]]$aes_params$fill, "white")
  expect_true(is.na(disc[[1]]$aes_params$colour))   # one circle, not two
})

test_that("the key labels its rings in the axis unit", {
  p   <- map_plot(radius = 100,
                  decor = polar_map_decor(key = "topright", scalebar = FALSE,
                                          axis_unit = "km/h"),
                  grid = polar_grid(rings = c(2, 4)))
  expect_true(all(c("2 km/h", "4 km/h") %in% text_labels(p)))
})

test_that("the key can be placed by hand or dropped", {
  by_hand <- map_plot(radius = 100,
                      decor = polar_map_decor(key = c(2686200, 1256800),
                                              scalebar = FALSE))
  disc <- decor_layers(by_hand, "GeomPolygon")[[1]]$data
  expect_equal(mean(range(disc$.E)), 2686200, tolerance = 1)
  expect_equal(mean(range(disc$.N)), 1256800, tolerance = 1)

  none <- map_plot(decor = polar_map_decor(key = FALSE, scalebar = FALSE))
  expect_length(decor_layers(none, "GeomPolygon"), 0L)
})

test_that(".polar_map_corner puts furniture inside the panel, pad and all", {
  xlim <- c(0, 1000); ylim <- c(0, 500)

  expect_equal(.polar_map_corner("bottomleft", xlim, ylim, 50, 20, 0.02),
               c(0 + 20 + 50, 0 + 20 + 20))
  expect_equal(.polar_map_corner("topright", xlim, ylim, 50, 20, 0.02),
               c(1000 - 20 - 50, 500 - 20 - 20))
  expect_equal(.polar_map_corner(c(123, 456), xlim, ylim, 50, 20, 0.02),
               c(123, 456))
})


# --- the scale bar --------------------------------------------------------

test_that("the scale bar takes a round fraction of the map width", {
  p   <- map_plot(decor = polar_map_decor(key = FALSE, scalebar = "bottomright"))
  txt <- text_labels(p)

  expect_true(any(grepl("^[0-9.]+ (m|km)$", txt)))
})

test_that("scalebar_length is honoured, in metres, and labelled in km beyond 1000", {
  lab_of <- function(len) {
    p <- map_plot(decor = polar_map_decor(key = FALSE, scalebar_length = len))
    text_labels(p)
  }
  expect_true("500 m" %in% lab_of(500))
  expect_true("2 km" %in% lab_of(2000))
})

test_that("scalebar = FALSE removes the bar and its plaque", {
  with_bar <- map_plot(decor = polar_map_decor(key = FALSE, scalebar = "bottomright"))
  without  <- map_plot(decor = polar_map_decor(key = FALSE, scalebar = FALSE))

  expect_lt(length(without$layers), length(with_bar$layers))
  expect_false("GeomText" %in% layer_geoms(without))
})


# --- attribution ----------------------------------------------------------

test_that("the attribution can be overridden or suppressed", {
  s  <- fake_sites()
  bm <- structure(list(bbox = c(xmin = 2685500, ymin = 1255500,
                                xmax = 2687500, ymax = 1257500),
                       attribution = "Kartengrundlage: \u00a9 swisstopo"),
                  class = "swisstopo_basemap")

  own <- polar_map(fake_polar(s), s, radius = 300, quiet = TRUE,
                   decor = plain_decor(attribution = "Map data: swisstopo"))
  expect_equal(own$labels$caption, "Map data: swisstopo")

  off <- polar_map(fake_polar(s), s, radius = 300, quiet = TRUE,
                   decor = plain_decor(attribution = FALSE))
  expect_null(off$labels$caption)

  # and the basemap's own text is what appears when nothing is set
  expect_equal(bm$attribution, "Kartengrundlage: \u00a9 swisstopo")
})

test_that("a fully decorated map renders without warnings", {
  s <- fake_sites()
  expect_renders_clean(
    polar_map(fake_polar(s), s, radius = 100, quiet = TRUE,
              decor = polar_map_decor(key = "topright", key_title = "Wind")))
})

test_that("bg accepts NULL as no plaque and refuses a vector", {
  expect_true(is.na(polar_map_decor(bg = NULL)$bg))
  expect_true(is.na(polar_map_decor(bg = NA)$bg))
  expect_error(polar_map_decor(bg = c("white", "grey80")), "bg")

  # ... and a map drawn without a plaque still renders
  s <- fake_sites()
  expect_renders_clean(
    polar_map(fake_polar(s), s, radius = 100, quiet = TRUE,
              decor = polar_map_decor(bg = NULL, key = "topright")))
})


# --- the scale bar, on any map ----------------------------------------------
# It is drawn by annotation_scalebar() and only *configured* by
# polar_map_decor(), so a plain ggplot map over the same extent gets the
# identical bar. These tests assert both ends of that.

# The bar itself is the one-row segment layer; the caps are the two-row one,
# and on a polar_map the grid spokes are segments too. Reading the last
# one-row segment picks the bar on both kinds of plot.
scalebar_bar <- function(layers) {
  seg <- Filter(function(l) inherits(l$geom, "GeomSegment"), layers)
  d   <- Filter(function(x) nrow(x) == 1L, lapply(seg, function(l) l$data))
  d[[length(d)]]
}

scalebar_span <- function(layers) {
  d <- scalebar_bar(layers)
  unname(d$xend - d$x)
}

scalebar_x <- function(layers) {
  d <- scalebar_bar(layers)
  unname(c(d$x, d$xend))
}

scalebar_label <- function(layers) {
  txt <- Filter(function(l) inherits(l$geom, "GeomText"), layers)
  txt[[length(txt)]]$aes_params$label
}

test_that("the bar is a round distance, labelled in the unit it reaches", {
  bb <- c(xmin = 2680000, xmax = 2690000, ymin = 1250000, ymax = 1256000)

  # 25 % of a 10 km map is 2.5 km, and 2.5 is one of the round steps
  l <- annotation_scalebar(bb)
  expect_equal(scalebar_span(l), 2500)
  expect_equal(scalebar_label(l), "2.5 km")

  # under a kilometre it says metres
  small <- c(xmin = 0, xmax = 2000, ymin = 0, ymax = 1000)
  expect_equal(scalebar_label(annotation_scalebar(small)), "500 m")

  # an explicit length is taken as given
  expect_equal(scalebar_span(annotation_scalebar(bb, length_m = 1000)), 1000)
})

test_that("the bar sits inside the panel, in the corner it was asked for", {
  bb <- c(xmin = 2680000, xmax = 2690000, ymin = 1250000, ymax = 1256000)

  left  <- scalebar_x(annotation_scalebar(bb, position = "bottomleft"))
  right <- scalebar_x(annotation_scalebar(bb, position = "bottomright"))
  expect_lt(left[1], right[1])
  expect_gt(left[1], bb[["xmin"]])              # not hanging over the edge
  expect_lt(right[2], bb[["xmax"]])

  # an explicit centre overrides the corner logic
  mid <- annotation_scalebar(bb, position = c(2685000, 1251000))
  expect_equal(mean(scalebar_x(mid)), 2685000)
})

test_that("the plaque is optional and the bbox is checked", {
  bb <- c(xmin = 2680000, xmax = 2690000, ymin = 1250000, ymax = 1256000)

  rects <- function(l) sum(vapply(l, function(x) inherits(x$geom, "GeomRect"),
                                  logical(1)))
  expect_equal(rects(annotation_scalebar(bb)), 1)
  expect_equal(rects(annotation_scalebar(bb, bg = NA)), 0)
  expect_equal(rects(annotation_scalebar(bb, bg = NULL)), 0)   # not a length-0 if

  expect_error(annotation_scalebar(bb, bg = c("white", "grey")), "single colour")
  expect_error(annotation_scalebar(c(xmin = 0, xmax = 1)), "ymin")
  expect_error(annotation_scalebar("bottomleft"), "bbox")
})

test_that("polar_map routes its bar through the same layer", {

  p <- map_plot(decor = polar_map_decor(key = FALSE, scalebar = "bottomright"))

  # The bar cannot contradict its own label: the span is read off the drawn
  # segment, the number off the drawn text, and the two come from one L.
  span <- scalebar_span(p$layers)
  lab  <- scalebar_label(p$layers)
  said <- as.numeric(sub(" (km|m)$", "", lab)) *
    if (grepl("km$", lab)) 1000 else 1
  expect_equal(span, said)

  expect_equal(length(.polar_map_scalebar(polar_map_decor(scalebar = FALSE),
                                          c(0, 1), c(0, 1))), 0L)
  expect_equal(length(.polar_map_scalebar(polar_map_decor(scalebar = NULL),
                                          c(0, 1), c(0, 1))), 0L)
})


# --- key and scale bar in the same corner --------------------------------
# Until this existed, asking for both in one corner drew them on top of each
# other and the only way out was hand-computed coordinates -- which need `k`,
# the key radius and the bar length, none of which a caller has.

key_disc <- function(p) {
  pol <- Filter(function(l) inherits(l$geom, "GeomPolygon"), p$layers)
  pol[[length(pol)]]$data
}

test_that("a shared corner stacks the key above the bar on one vertical", {
  stacked <- function(pos) {
    p   <- suppressWarnings(map_plot(
      decor = polar_map_decor(key = pos, scalebar = pos)))
    bar <- scalebar_bar(p$layers)
    kd  <- key_disc(p)
    list(bar_x = mean(c(bar$x, bar$xend)), bar_y = bar$y,
         key_x = mean(range(kd$.E)), key_y = mean(range(kd$.N)),
         key_r = diff(range(kd$.N)) / 2)
  }

  b <- stacked("bottomright")
  expect_equal(b$key_x, b$bar_x)                 # one vertical, centred
  expect_gt(b$key_y, b$bar_y)                    # key above the bar
  expect_gt(b$key_y - b$bar_y, b$key_r)          # and clear of it

  t <- stacked("topright")
  expect_equal(t$key_x, t$bar_x)
  expect_lt(t$key_y, t$bar_y)                    # hanging from the top instead
  expect_gt(t$bar_y - t$key_y, t$key_r)

  l <- stacked("bottomleft")
  expect_lt(l$key_x, b$key_x)                    # and it honours the side
})

test_that("different corners are left exactly where they were", {
  apart  <- suppressWarnings(map_plot(
    decor = polar_map_decor(key = "topleft", scalebar = "bottomright")))
  only   <- suppressWarnings(map_plot(
    decor = polar_map_decor(key = "topleft", scalebar = FALSE)))
  expect_equal(mean(range(key_disc(apart)$.E)),
               mean(range(key_disc(only)$.E)))
  expect_equal(mean(range(key_disc(apart)$.N)),
               mean(range(key_disc(only)$.N)))
})

test_that("the bar length is decided in one place", {
  # .scalebar_length() exists so that polar_map() can know the length before
  # the bar is drawn; the drawn bar must agree with it.
  bb <- c(xmin = 2680000, xmax = 2690000, ymin = 1250000, ymax = 1256000)
  expect_equal(scalebar_span(annotation_scalebar(bb)),
               unname(.scalebar_length(NULL, bb[c("xmin", "xmax")])))
  expect_equal(.scalebar_length(750, c(0, 1e5)), 750)
})
