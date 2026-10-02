# A distribution drawn as percentile bands and centre lines, with its legend built by ggplot2.
# The stat returns one row per statistic and labels it in the computed variable `statistic`;
# mapping that onto an aesthetic (alpha for the bands, linetype for the centres, ...) is what
# makes ggplot2 lay out the legend -- from the same scale that sets what is drawn, so the key
# cannot show a shade or a line type the figure does not have. Replaces `band_key()` (0.9.0),
# which drew a made-up hump beside the figure for the same purpose.

#' Percentile bands and centre lines of a distribution, with their legend
#'
#' Summarises `y` at every `x` into one or two central percentile bands and
#' the median and/or the mean, and draws the bands with `band_geom` and the
#' centres with `centre_geom`. Each band and each centre is a row carrying
#' the computed variable `statistic` (e.g. `"P10-P90"`, `"Median"`),
#' which is mapped onto an aesthetic of its geom so that ggplot2 builds the
#' legend itself. Add [scale_distribution()], which sets what the statistics
#' look like -- without it ggplot2's default scales apply, and the default
#' alpha scale warns that it is mapped to a discrete variable.
#'
#' Two typical displays:
#' * **line and ribbon** over a continuous `x` (the default): bands as
#'   ribbons told apart by `alpha`, median and mean as lines told apart by
#'   `linetype`;
#' * **box-like** over a discrete `x`: `band_geom = "linerange"`,
#'   `centre_geom = "point"` -- bands as lines of different `linewidth`, the
#'   centres as points of different `shape`.
#'
#' @section Two kinds of input:
#' * **Raw values** (`summarised = FALSE`): map `x` and `y`; the stat computes
#'   `stats::quantile(y, probs, type = 7)`, the median and the mean at every
#'   `x` within every group. Like every ggplot2 stat it works on the
#'   transformed scale, so on a log `y` scale the mean is a geometric mean.
#' * **Already summarised** (`summarised = TRUE`), for data too large to hand
#'   to a plot: map the boxplot aesthetics of ggplot2 -- `ymin`/`ymax` the
#'   outer band, `lower`/`upper` the inner band (the only band if `probs` has
#'   two values), `middle` the median -- and `y` the mean. One row per `x`
#'   and group. `probs` then only *names* the bands: pass the percentiles the
#'   table was computed with.
#'
#' @param mapping,data,position,show.legend,inherit.aes As in
#'   [ggplot2::layer()]; `mapping` and `position` apply to both parts.
#' @param ... Fixed aesthetics and other parameters, passed to both layers.
#' @param probs Percentiles as fractions, ascending: two values for one
#'   central band, four for two (`c(outer, inner, inner, outer)`).
#' @param centre Which centres to draw: any of `"median"`, `"mean"`, or
#'   `character(0)` for none.
#' @param band_geom,centre_geom Geoms of the two parts, as a name (`"ribbon"`,
#'   `"linerange"`, `"line"`, `"point"`, ...) or a `Geom` object; `NULL`
#'   leaves that part out.
#' @param band_aes,centre_aes Aesthetic(s) that tell the statistics of a part
#'   apart. `NULL` takes them from the geom: `alpha` for ribbons, areas and
#'   rectangles, `linewidth` for line ranges and error bars, `linetype` for
#'   lines and paths, `shape` for points. Several aesthetics give one legend
#'   block, e.g. `centre_aes = c("linetype", "linewidth")` together with
#'   `scale_distribution(linewidth_part = "centres")`. An aesthetic already
#'   in `mapping` is left as it is.
#' @param labels Named legend labels of the centres.
#' @param band_labels Legend labels of the bands, inner first; `NULL` writes
#'   them from `probs` (`"P25-P75"`, `"P10-P90"`).
#' @param summarised `TRUE` if the data hold the statistics already (see
#'   below).
#' @param band_params,centre_params Parameters for one part only, e.g.
#'   `band_params = list(colour = NA)` so that a colour mapped for the
#'   centres does not outline the ribbons.
#' @param min_n Fewest values an `x` needs before its statistics are drawn
#'   (raw values only). Below it the statistics are `NA`, so ribbons and
#'   lines break there instead of joining the neighbours across a gap -- a
#'   line from 22 to 6 o'clock through a night with three values would draw
#'   a night nobody measured. The count is the computed variable `n`.
#' @param na.rm Remove missing values silently. Set automatically when
#'   `min_n > 1`, because the gaps it makes are intended.
#'
#' @return A list of up to two ggplot2 layers (bands, then centres), to be
#'   added with `+`.
#' @seealso [scale_distribution()]
#' @examples
#' library(ggplot2)
#'
#' # Line and ribbon: the distribution of highway mileage over displacement
#' d <- transform(mpg, displ = round(displ))
#' ggplot(d, aes(displ, hwy)) +
#'   stat_distribution() +
#'   scale_distribution()
#'
#' # Box-like over a discrete x, groups dodged; the centres in a dark grey,
#' # or they would vanish into a box of their own colour
#' ggplot(mpg, aes(class, hwy, colour = factor(year))) +
#'   stat_distribution(band_geom = "linerange", centre_geom = "point",
#'                     centre_params = list(colour = "grey10"),
#'                     position = position_dodge(0.6)) +
#'   scale_distribution()
#'
#' # Already summarised: the boxplot aesthetics, and y for the mean
#' s <- do.call(rbind, lapply(split(d$hwy, d$displ), function(v) {
#'   q <- stats::quantile(v, c(0.1, 0.25, 0.5, 0.75, 0.9), names = FALSE)
#'   data.frame(p10 = q[1], p25 = q[2], median = q[3], p75 = q[4],
#'              p90 = q[5], mean = mean(v))
#' }))
#' s$displ <- as.numeric(rownames(s))
#' ggplot(s, aes(displ)) +
#'   stat_distribution(aes(ymin = p10, lower = p25, middle = median,
#'                         upper = p75, ymax = p90, y = mean),
#'                     summarised = TRUE) +
#'   scale_distribution()
#' @export
stat_distribution <- function(mapping = NULL, data = NULL, ...,
                              probs = c(0.10, 0.25, 0.75, 0.90),
                              centre = c("median", "mean"),
                              band_geom = "ribbon", centre_geom = "line",
                              band_aes = NULL, centre_aes = NULL,
                              labels = c(median = "Median", mean = "Mittelwert"),
                              band_labels = NULL, summarised = FALSE, min_n = 1L,
                              band_params = list(), centre_params = list(),
                              position = "identity", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE) {

  .dist_check_probs(probs)
  centre <- unique(as.character(centre))
  bad <- setdiff(centre, c("median", "mean"))
  if (length(bad)) {
    cli::cli_abort("{.arg centre} takes {.val median} and {.val mean}, not {.val {bad}}.")
  }
  if (length(centre) && !all(centre %in% names(labels))) {
    cli::cli_abort("{.arg labels} must name every centre drawn: {.val {centre}}.")
  }
  band_labels <- band_labels %||% .dist_band_labels(probs)
  if (length(band_labels) != length(probs) / 2L) {
    cli::cli_abort("{.arg band_labels} needs one label per band ({length(probs) / 2L}), inner first.")
  }

  shared <- c(list(probs = probs, centre = centre,
                   labels = unname(labels[centre]),
                   band_labels = as.character(band_labels),
                   summarised = isTRUE(summarised), min_n = min_n,
                   na.rm = na.rm || min_n > 1),
              list(...))
  part_layer <- function(part, geom, aesthetics, params) {
    g <- .dist_geom(geom)
    aesthetics <- aesthetics %||% .dist_default_aes(g, geom, part)
    ggplot2::layer(
      data = data, mapping = .dist_mapping(mapping, aesthetics),
      stat = StatDistribution, geom = geom, position = position,
      show.legend = show.legend, inherit.aes = inherit.aes,
      params = utils::modifyList(
        c(list(part = part,
               # Ribbons and lines connect what shares a group, so every
               # statistic gets a group of its own there; points and ranges
               # keep the caller's groups, so that dodging moves the inner
               # band, the outer band and the centres of a group together.
               separate = is.null(g) || inherits(g, c("GeomRibbon", "GeomPath",
                                                      "GeomPolygon"))),
          shared),
        params))
  }

  out <- list()
  if (!is.null(band_geom)) {
    out <- c(out, list(part_layer("bands", band_geom, band_aes, band_params)))
  }
  if (!is.null(centre_geom) && length(centre)) {
    out <- c(out, list(part_layer("centres", centre_geom, centre_aes, centre_params)))
  }
  if (!length(out)) {
    cli::cli_abort("Nothing to draw: both {.arg band_geom} and the centres are switched off.")
  }
  out
}

#' Scales for the statistics of [stat_distribution()]
#'
#' Manual scales for every aesthetic [stat_distribution()] may map its
#' statistics onto. Values are matched in the order of the statistics: the
#' bands inner first, the centres in the order of `centre`. A scale whose
#' aesthetic no layer maps has no effect. The legend keys are drawn in one
#' neutral `colour`, because the colour of the figure usually tells groups
#' apart, which the statistics are not.
#'
#' @param alpha Opacity of the bands drawn as ribbons.
#' @param linetype Line types of the centres drawn as lines.
#' @param linewidth Line widths -- of the bands drawn as line ranges, or of
#'   the centres if `centre_aes` includes `"linewidth"`.
#' @param linewidth_part Which part `linewidth` distinguishes, `"bands"` or
#'   `"centres"`. ggplot2 merges two legends into one block only if they
#'   share their position in the order of legends, so a line width that
#'   tells the centres apart has to be placed with the centres' line type.
#' @param shape Point shapes of the centres drawn as points.
#' @param colour Colour of the legend keys.
#' @param name Legend title; `NULL` for none.
#'
#' @return A list of ggplot2 scales.
#' @seealso [stat_distribution()]
#' @examples
#' library(ggplot2)
#' d <- transform(mpg, displ = round(displ))
#' ggplot(d, aes(displ, hwy)) +
#'   stat_distribution() +
#'   scale_distribution(alpha = c(0.4, 0.15), linetype = c("solid", "dotted"))
#' @export
scale_distribution <- function(alpha = c(0.30, 0.18),
                               linetype = c("solid", "42"),
                               linewidth = c(2.4, 0.8),
                               shape = c(16, 4),
                               linewidth_part = c("bands", "centres"),
                               colour = "grey25", name = NULL) {
  linewidth_part <- match.arg(linewidth_part)
  # Centres first, bands second, as one reads the figure: the lines, then
  # how far around them the values spread.
  key <- function(order, ...) {
    ggplot2::guide_legend(order = order, override.aes = list(...))
  }
  list(
    ggplot2::scale_alpha_manual(values = alpha, name = name,
                                guide = key(2, fill = colour)),
    ggplot2::scale_linetype_manual(values = linetype, name = name,
                                   guide = key(1, colour = colour)),
    ggplot2::scale_linewidth_manual(values = linewidth, name = name,
                                    # Among the centres it merges with the
                                    # line type, which brings the colour
                                    # already; a second override.aes warns.
                                    guide = if (linewidth_part == "bands") {
                                      key(2, colour = colour)
                                    } else {
                                      ggplot2::guide_legend(order = 1)
                                    }),
    ggplot2::scale_shape_manual(values = shape, name = name,
                                guide = key(1, colour = colour)))
}

#' @keywords internal
StatDistribution <- ggplot2::ggproto(
  "StatDistribution", ggplot2::Stat,
  required_aes = "x",
  optional_aes = c("y", "ymin", "lower", "middle", "upper", "ymax"),
  dropped_aes = c("y", "ymin", "lower", "middle", "upper", "ymax", "weight"),

  compute_panel = function(self, data, scales, ..., separate = TRUE) {
    out <- ggplot2::ggproto_parent(ggplot2::Stat, self)$compute_panel(data, scales, ...)
    if (separate && nrow(out)) {
      out$group <- vctrs::vec_group_id(out[c("group", "statistic")])
    }
    out
  },

  # `separate` belongs to compute_panel(); it is listed here only because
  # ggplot2 reads the parameters of a stat from compute_group() whenever
  # compute_panel() takes `...`.
  compute_group = function(data, scales, part = "bands", probs, centre,
                           labels, band_labels, summarised = FALSE,
                           min_n = 1L, na.rm = FALSE, separate = TRUE) {
    wide <- if (summarised) {
      .dist_given(data, probs, part, centre)
    } else {
      .dist_summarise(data, probs, na.rm, min_n)
    }
    .dist_long(wide, part, probs, centre, labels, band_labels)
  }
)

# --- helpers -------------------------------------------------------------------

.dist_check_probs <- function(probs) {
  if (!is.numeric(probs) || !length(probs) %in% c(2L, 4L) || anyNA(probs) ||
      any(probs < 0 | probs > 1) || is.unsorted(probs, strictly = TRUE)) {
    cli::cli_abort(c(
      "{.arg probs} must be two or four ascending fractions between 0 and 1.",
      i = "Two give one central band, four give two: {.code c(outer, inner, inner, outer)}."))
  }
  invisible(probs)
}

# "P10-P90": the label is written from the number it names, so the two
# cannot disagree. Inner band first.
.dist_band_labels <- function(probs) {
  pct <- vapply(probs * 100, function(p) format(p, trim = TRUE, drop0trailing = TRUE),
                character(1))
  lab <- paste0("P", pct[seq_len(length(pct) / 2L)], "\u2013P",
                rev(pct)[seq_len(length(pct) / 2L)])
  rev(lab)
}

.dist_geom <- function(geom) {
  if (inherits(geom, "Geom")) return(geom)
  if (!is.character(geom) || length(geom) != 1L) return(NULL)
  parts <- strsplit(geom, "_", fixed = TRUE)[[1L]]
  name <- paste0("Geom", paste0(toupper(substr(parts, 1L, 1L)), substring(parts, 2L),
                                collapse = ""))
  get0(name, envir = asNamespace("ggplot2"), inherits = FALSE)
}

.dist_default_aes <- function(g, geom, part) {
  if (inherits(g, c("GeomRibbon", "GeomRect", "GeomPolygon"))) return("alpha")
  if (inherits(g, c("GeomLinerange", "GeomErrorbar"))) return("linewidth")
  if (inherits(g, "GeomPath")) return("linetype")
  if (inherits(g, "GeomPoint")) return("shape")
  arg <- if (part == "bands") "band_aes" else "centre_aes"
  cli::cli_abort(c(
    "Cannot tell which aesthetic should tell the statistics apart for this geom.",
    i = "Name it with {.arg {arg}}."))
}

.dist_mapping <- function(mapping, aesthetics) {
  given <- if (is.null(mapping)) list() else as.list(mapping)
  new <- setdiff(aesthetics, names(given))
  add <- rep(list(rlang::quo(ggplot2::after_stat(.data$statistic))), length(new))
  ggplot2::aes(!!!c(given, stats::setNames(add, new)))
}

# Raw values -> one wide row per x, in the boxplot vocabulary of ggplot2.
.dist_summarise <- function(data, probs, na.rm, min_n = 1L) {
  if (is.null(data$y)) {
    cli::cli_abort(c(
      "{.fn stat_distribution} needs {.field y} to summarise.",
      i = "For statistics computed beforehand use {.code summarised = TRUE}."))
  }
  miss <- is.na(data$y)
  if (any(miss)) {
    if (!na.rm) {
      cli::cli_warn("Removed {sum(miss)} row{?s} with missing {.field y} (stat_distribution).")
    }
    data <- data[!miss, , drop = FALSE]
  }
  xs <- vctrs::vec_sort(vctrs::vec_unique(data$x))
  by_x <- split(data$y, factor(vctrs::vec_match(data$x, xs), levels = seq_along(xs)))
  q <- vapply(by_x, stats::quantile, numeric(length(probs)), probs = probs,
              names = FALSE, type = 7)
  wide <- vctrs::data_frame(
    x = xs, n = lengths(by_x, use.names = FALSE),
    middle = vapply(by_x, stats::median, numeric(1), USE.NAMES = FALSE),
    y = vapply(by_x, mean, numeric(1), USE.NAMES = FALSE))
  cols <- if (length(probs) == 4L) c("ymin", "lower", "upper", "ymax") else c("lower", "upper")
  for (i in seq_along(cols)) wide[[cols[i]]] <- unname(q[i, ])
  thin <- wide$n < min_n
  for (col in c("middle", "y", cols)) wide[[col]][thin] <- NA_real_
  wide
}

# Summarised input: the statistics are the caller's, taken as mapped.
.dist_given <- function(data, probs, part, centre) {
  need <- if (part == "bands") {
    if (length(probs) == 4L) c("ymin", "lower", "upper", "ymax") else c("lower", "upper")
  } else {
    c(median = "middle", mean = "y")[centre]
  }
  miss <- setdiff(need, names(data))
  if (length(miss)) {
    cli::cli_abort(c(
      "With {.code summarised = TRUE} the {part} need the aesthetic{?s} {.field {miss}}.",
      i = "{.field ymin}/{.field ymax} = outer band, {.field lower}/{.field upper} = inner band,
           {.field middle} = median, {.field y} = mean."))
  }
  data[c("x", unname(need))]
}

# Wide -> one row per statistic, labelled in `statistic`.
.dist_long <- function(wide, part, probs, centre, labels, band_labels) {
  n <- wide$n
  if (part == "bands") {
    pairs <- list(c("lower", "upper"), c("ymin", "ymax"))[seq_along(band_labels)]
    # The outer band first, so that it is drawn underneath the inner one.
    rows <- lapply(rev(seq_along(pairs)), function(i) {
      vctrs::data_frame(x = wide$x, ymin = wide[[pairs[[i]][1L]]],
                        ymax = wide[[pairs[[i]][2L]]], statistic = band_labels[i],
                        n = n %||% NA_integer_)
    })
    levels <- band_labels
  } else {
    cols <- c(median = "middle", mean = "y")[centre]
    rows <- lapply(seq_along(cols), function(i) {
      vctrs::data_frame(x = wide$x, y = wide[[cols[[i]]]], statistic = labels[i],
                        n = n %||% NA_integer_)
    })
    levels <- labels
  }
  out <- vctrs::vec_rbind(!!!rows)
  out$statistic <- factor(out$statistic, levels = levels)
  out
}
