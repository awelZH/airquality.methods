# The stat is read twice: for the numbers it draws and for the legend that
# names them. Both come from the built or drawn plot, not from the helpers --
# what matters is what reaches the page.

fixture <- function() {
  # Two x values, two groups, values chosen so that every statistic differs.
  data.frame(x = rep(c(1, 2), each = 10),
             g = rep(c("a", "b"), times = 10),
             y = c(1:10, (1:10)^2))
}

by_stat <- function(ld, stat) ld[ld$statistic == stat, , drop = FALSE]

test_that("raw values are summarised as quantile(type = 7), median and mean", {
  d <- fixture()
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + stat_distribution() + scale_distribution()
  bands   <- ggplot2::layer_data(p, 1)
  centres <- ggplot2::layer_data(p, 2)

  v <- d$y[d$x == 2]
  q <- stats::quantile(v, c(0.10, 0.25, 0.75, 0.90), names = FALSE, type = 7)
  outer <- by_stat(bands, "P10\u2013P90")
  inner <- by_stat(bands, "P25\u2013P75")
  expect_equal(outer$ymin[outer$x == 2], q[1])
  expect_equal(outer$ymax[outer$x == 2], q[4])
  expect_equal(inner$ymin[inner$x == 2], q[2])
  expect_equal(inner$ymax[inner$x == 2], q[3])
  expect_equal(by_stat(centres, "Median")$y[2], stats::median(v))
  expect_equal(by_stat(centres, "Mittelwert")$y[2], mean(v))
  expect_equal(unique(bands$n), 10L)
})

test_that("summarised input draws exactly what the same statistics computed by the stat draw", {
  d <- fixture()
  raw <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + stat_distribution() + scale_distribution()

  s <- do.call(rbind, lapply(split(d$y, d$x), function(v) {
    q <- stats::quantile(v, c(0.10, 0.25, 0.5, 0.75, 0.90), names = FALSE)
    data.frame(p10 = q[1], p25 = q[2], med = q[3], p75 = q[4], p90 = q[5], avg = mean(v))
  }))
  s$x <- c(1, 2)
  given <- ggplot2::ggplot(s, ggplot2::aes(x)) +
    stat_distribution(ggplot2::aes(ymin = p10, lower = p25, middle = med,
                                   upper = p75, ymax = p90, y = avg),
                      summarised = TRUE) +
    scale_distribution()

  cols_b <- c("x", "ymin", "ymax", "statistic", "group")
  cols_c <- c("x", "y", "statistic", "group")
  expect_equal(ggplot2::layer_data(given, 1)[cols_b], ggplot2::layer_data(raw, 1)[cols_b])
  expect_equal(ggplot2::layer_data(given, 2)[cols_c], ggplot2::layer_data(raw, 2)[cols_c])
})

test_that("the legend names the four statistics, or the caller's labels", {
  d <- fixture()
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + stat_distribution() + scale_distribution()
  lab <- grob_labels(plot_gtable(p))
  expect_true(all(c("Median", "Mittelwert", "P25\u2013P75", "P10\u2013P90") %in% lab))

  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    stat_distribution(labels = c(median = "med", mean = "avg"),
                      band_labels = c("inner", "outer")) +
    scale_distribution()
  lab <- grob_labels(plot_gtable(p))
  expect_true(all(c("med", "avg", "inner", "outer") %in% lab))
  expect_false("Median" %in% lab)

  # The band labels are written from probs.
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    stat_distribution(probs = c(0.05, 0.95), centre = "median") + scale_distribution()
  expect_true("P5\u2013P95" %in% grob_labels(plot_gtable(p)))
})

test_that("the legend key shows the opacity the bands are drawn with", {
  # What band_key() could only promise by being handed the same two numbers:
  # here the drawn ribbon and its key are coloured by one scale.
  d <- fixture()
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    stat_distribution(centre_geom = NULL) +
    scale_distribution(alpha = c(0.4, 0.1))
  drawn <- ggplot2::layer_data(p, 1)
  expect_equal(unique(by_stat(drawn, "P25\u2013P75")$alpha), 0.4)
  expect_equal(unique(by_stat(drawn, "P10\u2013P90")$alpha), 0.1)

  key <- ggplot2::get_guide_data(p, "alpha")
  expect_equal(key$alpha[key$.label == "P25\u2013P75"], 0.4)
  expect_equal(key$alpha[key$.label == "P10\u2013P90"], 0.1)
})

test_that("every group and statistic is a ribbon of its own, in every facet", {
  d <- rbind(transform(fixture(), f = "one"), transform(fixture(), f = "two"))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, fill = g)) +
    stat_distribution(centre_geom = NULL) + scale_distribution() +
    ggplot2::facet_wrap(ggplot2::vars(f))
  ld <- ggplot2::layer_data(p, 1)
  # 2 groups x 2 bands per panel; within one, a single statistic and group.
  per_panel <- tapply(ld$group, ld$PANEL, function(g) length(unique(g)))
  expect_equal(as.vector(per_panel), c(4L, 4L))
  one <- ld[ld$PANEL == 1 & ld$group == ld$group[1], ]
  expect_length(unique(one$statistic), 1L)
  expect_length(unique(one$fill), 1L)
})

test_that("the box-like display maps line width and shape and keeps the groups for dodging", {
  d <- fixture()
  p <- ggplot2::ggplot(d, ggplot2::aes(factor(x), y, colour = g)) +
    stat_distribution(band_geom = "linerange", centre_geom = "point",
                      position = ggplot2::position_dodge(0.5)) +
    scale_distribution(linewidth = c(3, 1), shape = c(16, 4))
  expect_s3_class(p$layers[[1]]$geom, "GeomLinerange")
  expect_s3_class(p$layers[[2]]$geom, "GeomPoint")

  bands   <- ggplot2::layer_data(p, 1)
  centres <- ggplot2::layer_data(p, 2)
  expect_equal(unique(by_stat(bands, "P25\u2013P75")$linewidth), 3)
  expect_equal(unique(by_stat(bands, "P10\u2013P90")$linewidth), 1)
  expect_equal(unique(by_stat(centres, "Mittelwert")$shape), 4)
  # Inner band, outer band and centres of a group stand at one dodged x.
  key <- function(ld) unique(ld[c("colour", "x")])
  expect_equal(nrow(key(rbind(bands[c("colour", "x")], centres[c("colour", "x")]))), 4L)
})

test_that("several aesthetics on one part give one legend block", {
  # Counted on the drawn guide box: grob_labels() is unique(), so two blocks
  # naming "Median" twice would pass a label test unnoticed.
  n_legends <- function(p) {
    gt  <- plot_gtable(p)
    box <- gt$grobs[[which(gt$layout$name == "guide-box-right")]]
    sum(box$layout$name == "guides")
  }
  d <- fixture()
  centres <- stat_distribution(band_geom = NULL, centre_aes = c("linetype", "linewidth"))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + centres +
    scale_distribution(linewidth = c(0.6, 0.5), linewidth_part = "centres")
  ld <- ggplot2::layer_data(p, 1)
  expect_equal(unique(by_stat(ld, "Median")$linewidth), 0.6)
  expect_equal(unique(by_stat(ld, "Mittelwert")$linewidth), 0.5)
  expect_equal(n_legends(p), 1L)
  expect_renders_clean(p)   # merging two override.aes would warn here

  # ggplot2 merges only legends of the same order: with the line width
  # placed among the bands, the centres get two blocks.
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + centres +
    scale_distribution(linewidth = c(0.6, 0.5))
  expect_equal(n_legends(p), 2L)
})

test_that("a part can be left out, and given parameters of its own", {
  d <- fixture()
  expect_length(stat_distribution(band_geom = NULL), 1L)
  expect_length(stat_distribution(centre = character(0)), 1L)
  expect_length(stat_distribution(), 2L)

  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = g)) +
    stat_distribution(band_params = list(colour = NA)) + scale_distribution()
  expect_true(all(is.na(ggplot2::layer_data(p, 1)$colour)))
  expect_false(anyNA(ggplot2::layer_data(p, 2)$colour))
})

test_that("an x with fewer than min_n values becomes a gap, not a bridge", {
  # Three x positions; the middle one holds two values only.
  d <- data.frame(x = c(rep(1, 10), 2, 2, rep(3, 10)),
                  y = c(1:10, 5, 6, (1:10) * 2))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    stat_distribution(min_n = 5) + scale_distribution()
  bands <- ggplot2::layer_data(p, 1)
  centres <- ggplot2::layer_data(p, 2)
  expect_true(all(is.na(bands$ymin[bands$x == 2])))
  expect_true(all(is.na(centres$y[centres$x == 2])))
  expect_false(anyNA(centres$y[centres$x != 2]))
  expect_equal(unique(bands$n[bands$x == 2]), 2L)
  # The gap is intended, so drawing it raises nothing.
  expect_renders_clean(p)
})

test_that("what cannot be drawn as asked is refused", {
  expect_error(stat_distribution(probs = c(0.1, 0.5, 0.9)), "two or four")
  expect_error(stat_distribution(probs = c(0.9, 0.1)), "ascending")
  expect_error(stat_distribution(centre = "mode"), "median")
  expect_error(stat_distribution(band_geom = NULL, centre_geom = NULL), "Nothing to draw")
  expect_error(stat_distribution(band_geom = "text"), "band_aes")

  s <- data.frame(x = 1:2, p10 = 1:2, p90 = 3:4)
  p <- ggplot2::ggplot(s, ggplot2::aes(x, ymin = p10, ymax = p90)) +
    stat_distribution(summarised = TRUE, centre_geom = NULL)
  expect_error(ggplot2::ggplot_build(p), "lower")
})

test_that("both displays render clean", {
  d <- fixture()
  expect_renders_clean(
    ggplot2::ggplot(d, ggplot2::aes(x, y, fill = g, colour = g)) +
      stat_distribution(band_params = list(colour = NA)) + scale_distribution())
  expect_renders_clean(
    ggplot2::ggplot(d, ggplot2::aes(factor(x), y, colour = g)) +
      stat_distribution(band_geom = "linerange", centre_geom = "point",
                        position = ggplot2::position_dodge(0.5)) +
      scale_distribution())
})
