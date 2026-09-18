# =============================================================================
#  Guides: cap labels and whiskers
# =============================================================================

#' Extra parameters the capped guides understand
#' @keywords internal
.capped_guide_params <- list(
  cap                   = TRUE,
  include_cap_val       = TRUE,
  cap_labels            = TRUE,
  cap_min_dist          = NULL,
  whiskers              = TRUE,
  whisker_gap           = grid::unit(2, "mm"),
  whisker_avoid_overlap = TRUE,
  whisker_stem          = TRUE,
  whisker_width         = 1,
  whisker_colour        = NULL,
  whisker_linewidth     = NULL,
  whisker_linetype      = 1,
  whisker_labels        = TRUE,
  .capped               = NULL,  # internal: c(low_capped, high_capped)
  .whisker_labels       = NULL,  # internal: c(label_low, label_high)
  .cap_at               = NULL   # internal: positions of inserted edge breaks
)

#' Format a single value like a scale label
#'
#' The value arrives in transformed space and is back-transformed first.
#' Order of precedence:
#'   1. an explicitly supplied formatting function,
#'   2. the scale's `labels` function, if the user set one,
#'   3. default: the same precision as the other labels. Formatting the value
#'      on its own would give ">86.87799" for odd limits (e.g. from
#'      quantiles), while its neighbours read "50" and "75".
#'
#' @param ref Reference values (the other breaks, transformed) that the
#'   precision is based on.
#' @keywords internal
.capped_format <- function(scale, value, fun = TRUE, ref = NULL) {

  inv <- tryCatch(scale$get_transformation()$inverse, error = function(e) identity)
  ok  <- function(x) length(x) == 1L && is.character(x) && !is.na(x)

  if (is.function(fun)) {
    out <- tryCatch(as.character(fun(inv(value))), error = function(e) NULL)
    if (ok(out)) return(out)
  }

  if (!inherits(scale$labels, "waiver") && !is.null(scale$labels)) {
    out <- tryCatch(as.character(scale$get_labels(value)), error = function(e) NULL)
    if (ok(out)) return(out)
  }

  v <- inv(value)
  r <- if (length(ref)) inv(ref) else numeric(0)
  out <- tryCatch({
    l <- scales::label_number(accuracy = NULL, big.mark = "")(c(r, v))
    l[length(l)]
  }, error = function(e) NULL)
  if (ok(out)) return(out)

  format(v, trim = TRUE, digits = 3L)
}

#' Append a row to a guide key while preserving attributes
#' @keywords internal
.capped_key_add <- function(key, value, label, colour, aes_col) {
  new <- key[1L, , drop = FALSE]
  if (!is.na(aes_col) && aes_col %in% names(new)) new[[aes_col]] <- colour
  new$.value <- value
  new$.label <- label
  new$.cap   <- TRUE
  keep <- setdiff(names(attributes(key)), c("names", "row.names", "class"))
  at   <- attributes(key)[keep]
  out  <- rbind(key, new)
  rownames(out) <- NULL
  if (length(at)) attributes(out)[names(at)] <- at
  out[order(out$.value), , drop = FALSE]
}

#' Determine capping, extend the key, set "<"/">"
#'
#' @param space "value" = `key$.value` is still in data values (colourbar,
#'   before the parent's rescaling); "npc" = `key$.value` is already rescaled
#'   to \[0, 1\] (coloursteps, after the parent).
#' @keywords internal
.capped_annotate <- function(scale, params, space = c("value", "npc")) {

  space <- match.arg(space)

  dr  <- scale$range$range                 # uncapped data range
  lim <- tryCatch(scale$get_limits(), error = function(e) NULL)
  if (!is.numeric(dr) || length(dr) != 2L || !is.numeric(lim) || length(lim) != 2L) return(params)
  if (any(!is.finite(dr)) || any(!is.finite(lim))) return(params)

  tol <- 1e-8 * max(1, abs(diff(lim)))
  capped <- c(low = dr[1L] < lim[1L] - tol, high = dr[2L] > lim[2L] + tol)

  # Only capped when `oob` actually pulls the outlying values to the edge.
  # With oob = censor/keep they become NA and are drawn in `na.value` —
  # then "<0" would simply be wrong, and there's nothing to whisker either.
  # Checked functionally against the real data extremes, so custom oob
  # functions are recognised correctly too.
  if (any(capped)) {
    probe <- tryCatch(scale$oob(dr, range = lim), error = function(e) NULL)
    if (length(probe) != 2L) probe <- c(NA_real_, NA_real_)
    capped <- capped & c(isTRUE(abs(probe[1L] - lim[1L]) <= tol),
                         isTRUE(abs(probe[2L] - lim[2L]) <= tol))
  }

  params$.capped <- unname(capped)
  if (!any(capped)) return(params)

  # Reference for label precision: the scale's regular breaks (in
  # transformed space, regardless of how the guide scales the key). Cap and
  # whisker values are added so both ends of a label kind show the same
  # number of digits.
  brk <- tryCatch(scale$get_breaks(), error = function(e) NULL)
  brk <- brk[is.finite(brk)]
  ref_cap  <- c(brk, lim[capped])
  ref_whsk <- c(brk, dr[capped])

  # ---- Whisker labels --------------------------------------------------
  # FALSE means the whisker keeps its stroke but loses its number -- the same
  # thing FALSE means for every other switch in this guide. The drawing code
  # reads NA as "no label", so that is what FALSE turns into here.
  wl <- params$whisker_labels %||% TRUE
  params$.whisker_labels <- if (isFALSE(wl)) c(NA_character_, NA_character_) else c(
    if (capped[["low"]])  .capped_format(scale, dr[1L], wl, ref_whsk) else NA_character_,
    if (capped[["high"]]) .capped_format(scale, dr[2L], wl, ref_whsk) else NA_character_
  )

  if (!isTRUE(params$cap)) return(params)

  # ---- "<"/">" labels at the scale ends ---------------------------------
  key <- params$key
  if (is.null(key) || !nrow(key)) return(params)
  # The limit value belongs to the edge class (see include_cap_val), so
  # "≤"/"≥" is the accurate notation and the default, "<"/">" the
  # shorthand. Literal UTF-8 characters are safe here: DESCRIPTION declares
  # Encoding: UTF-8, so the package machinery reads this file consistently.
  sym <- if (isFALSE(params$include_cap_val)) c("<", ">") else c("\u2264", "\u2265")
  rev <- isTRUE(params$reverse)

  # Positions of the two scale ends in the unit of key$.value
  ends <- if (space == "value") lim else if (rev) c(1, 0) else c(0, 1)
  etol <- if (space == "value") tol else 1e-6
  if (is.null(key$.cap)) key$.cap <- rep(FALSE, nrow(key))

  for (i in which(capped)) {
    lab <- .capped_format(scale, lim[i], params$cap_labels %||% TRUE, ref_cap)
    if (!is.character(lab)) next
    lab <- paste0(sym[i], lab)

    hit <- which(abs(key$.value - ends[i]) <= etol)
    if (length(hit)) {
      # There's already a break at the scale end -> just set the prefix.
      # Break spacing stays as-is, so no thinning is needed either.
      key$.label[hit] <- lab
    } else {
      # New break at the scale end; neighbours that are too close are
      # removed only when drawing, where the bar length in cm is known
      # (.capped_thin_key()).
      key <- .capped_key_add(
        key,
        value   = ends[i],
        label   = lab,
        colour  = tryCatch(scale$map(lim[i]), error = function(e) NA),
        aes_col = params$aesthetic %||% NA_character_
      )
    }
  }

  params$key <- key
  params
}

#' Move the inserted-edge-break marker from the key into the parameters
#'
#' The key must not keep the extra column: when two guides are merged (e.g.
#' `colour` and `fill` on the same scale), ggplot2 joins the keys
#' column-wise and would duplicate the marker. Only the position is
#' remembered instead — it doesn't change between `extract_params()` and
#' drawing.
#' @keywords internal
.capped_stash_caps <- function(params) {
  cap <- params$key$.cap
  if (is.null(cap)) return(params)
  params$.cap_at    <- params$key$.value[which(cap)]
  params$key$.cap   <- NULL
  params
}

#' Thin out breaks that sit too close to an inserted edge break
#'
#' Called while drawing: `elements$height_cm`/`width_cm` gives the bar
#' length, `key$.value` the relative position — from that the distance in cm
#' can be determined and compared with the actual text extent. Without a
#' known bar length (e.g. a stretchable legend) this falls back to 5% of the
#' bar length.
#' @keywords internal
.capped_thin_key <- function(key, elements, params) {

  at <- params$.cap_at
  if (is.null(at) || !length(at) || nrow(key) < 2L) return(key)
  cap <- vapply(key$.value, function(v) any(abs(at - v) < 1e-9), logical(1))
  if (!any(cap)) return(key)

  vertical <- !identical(params$direction, "horizontal")
  len <- if (vertical) elements$height_cm else elements$width_cm
  len_ok <- is.numeric(len) && length(len) == 1L && is.finite(len) && len > 0
  md  <- params$cap_min_dist

  keep <- rep(TRUE, nrow(key))
  for (i in which(cap)) {
    for (j in which(keep & !cap)) {
      thr <- if (is.numeric(md) && length(md) == 1L && !is.na(md)) {
        md
      } else if (len_ok) {
        si <- .capped_text_size(elements$text, key$.label[i])
        need <- if (vertical) si$h else {
          (si$w + .capped_text_size(elements$text, key$.label[j])$w) / 2 + si$pad
        }
        need / len
      } else 0.05
      if (abs(key$.value[j] - key$.value[i]) < thr) keep[j] <- FALSE
    }
  }
  key[keep, , drop = FALSE]
}


#' Text extent in cm
#'
#' `w` is the measured text width, `h` the height of a full text line
#' (ggplot2 sets `lineheight` to 0.9, too tight for the gap between two
#' *different* labels — hence the floor at 1.2), `pad` a small extra so
#' neighbouring labels don't touch.
#' @keywords internal
.capped_text_size <- function(el, txt) {
  gp <- list(fontsize   = el$size %||% 11,
             fontfamily = el$family %||% "",
             fontface   = el$face %||% "plain",
             lineheight = max(el$lineheight %||% 1.2, 1.2))
  gp <- do.call(grid::gpar, gp[!vapply(gp, is.null, logical(1))])
  cm <- function(u) grid::convertHeight(u, "cm", valueOnly = TRUE)
  list(
    w   = grid::convertWidth(grid::grobWidth(grid::textGrob(txt, gp = gp)), "cm",
                             valueOnly = TRUE),
    h   = cm(grid::unit(gp$fontsize * gp$lineheight, "pt")),
    pad = cm(grid::unit(gp$fontsize * 0.25, "pt"))
  )
}

#' Label at the geometric bar end (for the collision check)
#' @keywords internal
.capped_end_label <- function(params, at_start) {
  key <- params$key
  if (is.null(key) || !nrow(key) || !is.numeric(key$.value)) return(NA_character_)
  j <- if (at_start) which.min(key$.value) else which.max(key$.value)
  if (abs(key$.value[j] - if (at_start) 0 else 1) > 0.02) return(NA_character_)
  lab <- key$.label[j]
  if (!is.character(lab) || is.na(lab)) NA_character_ else lab
}

#' Draw the whisker: cross-stroke at the data extreme, plus the stem
#'
#' Built like a boxplot — a line from the edge of the colour scale outward
#' (`stem`) and, crossing it, the mark for the extreme value.
#'
#' @param pos Position of the cross-stroke in the new cell (along the scale
#'   axis).
#' @param edge The edge of the same cell that faces the bar — the stem
#'   starts there.
#' @param width Length of the cross-stroke relative to the bar width.
#' @param vertical Orientation of the colour scale.
#' @param stem Draw the stem yes/no.
#' @keywords internal
.capped_whisker_grob <- function(pos, edge, width, gp, vertical, stem) {

  across <- grid::unit(c(0.5 - width / 2, 0.5 + width / 2), "npc")  # across the axis
  a0 <- across[1]
  a1 <- across[2]
  b0 <- pos                                                         # along the axis
  b1 <- pos

  if (isTRUE(stem)) {
    mid <- grid::unit(0.5, "npc")
    a0 <- grid::unit.c(a0, mid)
    a1 <- grid::unit.c(a1, mid)
    b0 <- grid::unit.c(b0, edge)
    b1 <- grid::unit.c(b1, pos)
  }

  if (vertical) {
    grid::segmentsGrob(x0 = a0, x1 = a1, y0 = b0, y1 = b1, gp = gp)
  } else {
    grid::segmentsGrob(x0 = b0, x1 = b1, y0 = a0, y1 = a1, gp = gp)
  }
}

#' Attach whisker grobs to the finished guide gtable
#'
#' Called from `assemble_drawing()` — `params` (including the resolved
#' `direction`) and `elements` are already available there in the same form
#' the guide itself uses for labels and ticks. That keeps whisker labels
#' positioned exactly like the other labels.
#'
#' Whiskers go into a newly inserted gtable row (vertical) or column
#' (horizontal); its size is `whisker_gap` plus half the label extent. The
#' cross-stroke is drawn in absolute units exactly `whisker_gap` from the bar
#' edge — the gap is thus independent of data range and bar size.
#' @keywords internal
.capped_add_whiskers <- function(gt, params, elements) {

  capped <- params$.capped %||% c(FALSE, FALSE)
  if (!isTRUE(params$whiskers) || !any(capped)) return(gt)
  if (!inherits(gt, "gtable")) return(gt)

  bar_names <- c("bar", "decor", "bars", "frame", "ticks")
  if (!any(gt$layout$name %in% bar_names)) return(gt)

  vertical <- !identical(params$direction, "horizontal")
  el_text  <- elements$text

  # The colour bar's ticks are deliberately thin and (in many themes) white
  # — no good as a free-standing stroke. So: label colour, theme baseline
  # weight.
  gp <- grid::gpar(
    col = params$whisker_colour %||% el_text$colour %||% "black",
    lwd = (params$whisker_linewidth %||% elements$whisker_line$linewidth %||% 0.5) *
      ggplot2::.pt,
    lty = params$whisker_linetype %||% 1,
    lineend = "butt"
  )

  gap0 <- params$whisker_gap %||% grid::unit(2, "mm")
  if (!grid::is.unit(gap0)) gap0 <- grid::unit(gap0, "mm")
  ww   <- params$whisker_width %||% 1
  labs <- params$.whisker_labels %||% c(NA_character_, NA_character_)
  rev  <- isTRUE(params$reverse)

  for (i in c(1L, 2L)) {

    if (!capped[i]) next
    txt <- labs[i]

    bar      <- gt$layout[which(gt$layout$name %in% bar_names)[1L], ]
    i_lab    <- grep("^label", gt$layout$name)
    lab_cell <- if (length(i_lab)) gt$layout[i_lab[1L], ] else NULL

    # Label grob for measuring (both margins -> cell requirement incl. gaps).
    has_lab <- !is.na(txt) && nzchar(txt)
    if (has_lab) {
      measure <- ggplot2::element_grob(el_text, label = txt, margin_x = TRUE, margin_y = TRUE)
      lab_w <- grid::convertWidth(grid::grobWidth(measure),  "cm", valueOnly = TRUE)
      lab_h <- grid::convertHeight(grid::grobHeight(measure), "cm", valueOnly = TRUE)
    } else {
      lab_w <- lab_h <- 0
    }

    # low (i = 1) sits at the bottom vertically, on the left horizontally —
    # unless reversed.
    at_start <- xor(i == 1L, rev)

    # The gap is fixed; it's only enlarged when the whisker label would
    # otherwise run into the bar's edge label (whose centre sits exactly on
    # the bar end).
    gap <- gap0
    if (has_lab && isTRUE(params$whisker_avoid_overlap)) {
      end_lab <- .capped_end_label(params, at_start)
      if (!is.na(end_lab) && nzchar(end_lab)) {
        sz   <- .capped_text_size(el_text, txt)
        need <- if (vertical) sz$h else {
          (sz$w + .capped_text_size(el_text, end_lab)$w) / 2 + sz$pad
        }
        gap <- grid::unit.pmax(gap, grid::unit(need, "cm"))
      }
    }

    # Position of the cross-stroke in the new cell, and the edge of that
    # same cell facing the bar (the stem starts there).
    pos_u <- if (at_start) grid::unit(1, "npc") - gap else gap
    edge  <- if (at_start) grid::unit(1, "npc") else grid::unit(0, "npc")
    stem  <- isTRUE(params$whisker_stem)

    if (vertical) {

      lab_col  <- lab_cell$l %||% NA_integer_
      lab_grob <- if (has_lab && !is.na(lab_col)) {
        ggplot2::element_grob(el_text, label = txt, y = pos_u,
                              margin_x = TRUE, margin_y = FALSE)
      } else NULL

      after <- if (at_start) bar$b else bar$t - 1L   # insert row *after* ...
      gt    <- gtable::gtable_add_rows(gt, gap + grid::unit(max(lab_h / 2, 0.02), "cm"),
                                       pos = after)
      row <- after + 1L

      seg <- .capped_whisker_grob(pos_u, edge, ww, gp, TRUE, stem)
      gt <- gtable::gtable_add_grob(gt, seg, t = row, b = row, l = bar$l, r = bar$r,
                                    clip = "off", name = paste0("whisker-", i))
      if (!is.null(lab_grob)) {
        gt <- gtable::gtable_add_grob(gt, lab_grob, t = row, b = row, l = lab_col, r = lab_col,
                                      clip = "off", name = paste0("whisker-label-", i))
        gt$widths[lab_col] <- grid::unit.pmax(gt$widths[lab_col], grid::unit(lab_w, "cm"))
      }

    } else {

      lab_row  <- lab_cell$t %||% NA_integer_
      lab_grob <- if (has_lab && !is.na(lab_row)) {
        ggplot2::element_grob(el_text, label = txt, x = pos_u,
                              margin_x = FALSE, margin_y = TRUE)
      } else NULL

      after <- if (at_start) bar$l - 1L else bar$r   # insert column *after* ...
      gt    <- gtable::gtable_add_cols(gt, gap + grid::unit(max(lab_w / 2, 0.02), "cm"),
                                       pos = after)
      col <- after + 1L

      seg <- .capped_whisker_grob(pos_u, edge, ww, gp, FALSE, stem)
      gt <- gtable::gtable_add_grob(gt, seg, t = bar$t, b = bar$b, l = col, r = col,
                                    clip = "off", name = paste0("whisker-", i))
      if (!is.null(lab_grob)) {
        gt <- gtable::gtable_add_grob(gt, lab_grob, t = lab_row, b = lab_row, l = col, r = col,
                                      clip = "off", name = paste0("whisker-label-", i))
        gt$heights[lab_row] <- grid::unit.pmax(gt$heights[lab_row], grid::unit(lab_h, "cm"))
      }
    }
  }

  gt
}


# --- Guide classes -----------------------------------------------------------

#' @keywords internal
GuideColourbarCapped <- ggplot2::ggproto(
  "GuideColourbarCapped", ggplot2::GuideColourbar,

  params = c(ggplot2::GuideColourbar$params, .capped_guide_params),

  extract_params = function(self, scale, params, ...) {
    # Colourbar: key$.value is still in data values -> before the parent.
    params <- .capped_annotate(scale, params, space = "value")
    .capped_stash_caps(
      ggplot2::ggproto_parent(ggplot2::GuideColourbar, self)$extract_params(scale, params, ...)
    )
  },

  override_elements = function(self, params, elements, theme) {
    elements <- ggplot2::ggproto_parent(ggplot2::GuideColourbar, self)$override_elements(params, elements, theme)
    elements$whisker_line <- ggplot2::calc_element("line", theme)
    elements
  },

  build_ticks = function(self, key, elements, params) {
    ggplot2::ggproto_parent(ggplot2::GuideColourbar, self)$build_ticks(
      .capped_thin_key(key, elements, params), elements, params
    )
  },

  build_labels = function(self, key, elements, params) {
    ggplot2::ggproto_parent(ggplot2::GuideColourbar, self)$build_labels(
      .capped_thin_key(key, elements, params), elements, params
    )
  },

  assemble_drawing = function(self, grobs, layout, sizes, params, elements) {
    gt <- ggplot2::ggproto_parent(ggplot2::GuideColourbar, self)$assemble_drawing(
      grobs, layout, sizes, params, elements
    )
    .capped_add_whiskers(gt, params, elements)
  }
)

#' @keywords internal
GuideColourstepsCapped <- ggplot2::ggproto(
  "GuideColourstepsCapped", ggplot2::GuideColoursteps,

  params = c(ggplot2::GuideColoursteps$params, .capped_guide_params),

  extract_params = function(self, scale, params, ...) {
    # Coloursteps: the parent rescales (and censors) key$.value to [0, 1]
    # and can make the bins equal width (even.steps) -> after that.
    params <- ggplot2::ggproto_parent(ggplot2::GuideColoursteps, self)$extract_params(scale, params, ...)
    .capped_stash_caps(.capped_annotate(scale, params, space = "npc"))
  },

  override_elements = function(self, params, elements, theme) {
    elements <- ggplot2::ggproto_parent(ggplot2::GuideColoursteps, self)$override_elements(params, elements, theme)
    elements$whisker_line <- ggplot2::calc_element("line", theme)
    elements
  },

  build_ticks = function(self, key, elements, params) {
    ggplot2::ggproto_parent(ggplot2::GuideColoursteps, self)$build_ticks(
      .capped_thin_key(key, elements, params), elements, params
    )
  },

  build_labels = function(self, key, elements, params) {
    ggplot2::ggproto_parent(ggplot2::GuideColoursteps, self)$build_labels(
      .capped_thin_key(key, elements, params), elements, params
    )
  },

  assemble_drawing = function(self, grobs, layout, sizes, params, elements) {
    gt <- ggplot2::ggproto_parent(ggplot2::GuideColoursteps, self)$assemble_drawing(
      grobs, layout, sizes, params, elements
    )
    .capped_add_whiskers(gt, params, elements)
  }
)


#' Colour bar guide with cap labels and whiskers
#'
#' Behaves like [ggplot2::guide_colourbar()] or [ggplot2::guide_coloursteps()],
#' but additionally draws:
#'   * "≤"/"≥" on the lowest/highest label once data lie outside
#'     `limits` (the break at the scale end is added if needed),
#'   * the true data minimum/maximum as a whisker at a fixed distance outside
#'     the colour bar, with its own label. The whisker is built like a
#'     boxplot's: a stem from the bar edge outward, and crossing it, the mark
#'     for the extreme value.
#'
#' Both only appear when the scale actually pulls out-of-range values to the
#' edge (`oob = scales::oob_squish`). With `oob = scales::oob_censor` or
#' `oob_keep` such values become `NA` — then there's no edge class to label,
#' and both the guide and the whisker are omitted.
#'
#' @param ... Arguments for [ggplot2::guide_colourbar()] /
#'   [ggplot2::guide_coloursteps()] (e.g. `title`, `theme`, `reverse`,
#'   `barheight` via `theme`, `even.steps`, `show.limits`).
#' @param cap `TRUE`/`FALSE` — comparison-sign labelling of the scale ends.
#' @param include_cap_val Does the limit value itself belong to the edge
#'   class? With `oob = scales::oob_squish` the answer is yes: the function
#'   leaves the limit unchanged and pulls everything beyond it to that value.
#'   The limit therefore gets the same colour as the capped values; for
#'   binned scales it falls into the same class thanks to `include.lowest`,
#'   for both `right = TRUE` and `FALSE`. The edge class is thus
#'   "x ≤ lim\[1\]" or "x ≥ lim\[2\]", and `TRUE` (default) labels
#'   it accordingly with "≤" and "≥". `FALSE` selects the common
#'   shorthand "<" and ">", which omits the limit value.
#' @param cap_labels Formatting of the cap value: `TRUE` (default) uses the
#'   scale's `labels` function if set, and otherwise the same precision as
#'   the other labels. Alternatively a function (e.g.
#'   `scales::label_number(accuracy = 0.1)`).
#' @param cap_min_dist Minimum distance between a newly inserted edge break
#'   and the existing breaks; closer breaks are dropped so labels don't
#'   overlap. Default `NULL` = automatic, from the actual text extent and the
#'   bar length. A number forces a fixed fraction of the bar length instead
#'   (e.g. `0.05`), `0` disables thinning.
#' @param whiskers `TRUE`/`FALSE` — draw whiskers for the data extremes.
#' @param whisker_gap Fixed gap between the bar edge and the whisker (`unit`).
#'   With `whisker_stem = TRUE` this is also the stem length.
#' @param whisker_avoid_overlap When `TRUE` (default), `whisker_gap` is only
#'   enlarged if the whisker label would otherwise run into the label at the
#'   bar end (e.g. ">100"). `FALSE` forces an exactly constant gap — then
#'   `whisker_gap` must be chosen large enough.
#' @param whisker_stem `TRUE` (default) connects the bar edge and the
#'   cross-stroke with a line, as in a boxplot. `FALSE` leaves the gap empty.
#' @param whisker_width Length of the cross-stroke relative to the bar width
#'   (1 = as wide as the bar).
#' @param whisker_colour,whisker_linewidth,whisker_linetype Appearance of the
#'   whisker. Default: legend text colour (`legend.text`) and the theme's
#'   baseline weight (`line`, usually 0.5).
#' @param whisker_labels `TRUE` (labelled as described under `cap_labels`),
#'   `FALSE` (whisker drawn, but unlabelled), or a function that formats the
#'   value (e.g. `scales::label_number(accuracy = 0.1)`).
#' @param steps `TRUE` for binned scales (coloursteps), otherwise a colour
#'   bar.
#'
#' @return A guide object.
#' @export
guide_colourbar_capped <- function(...,
                                   cap                   = TRUE,
                                   include_cap_val       = TRUE,
                                   cap_labels            = TRUE,
                                   cap_min_dist          = NULL,
                                   whiskers              = TRUE,
                                   whisker_gap           = grid::unit(2, "mm"),
                                   whisker_avoid_overlap = TRUE,
                                   whisker_stem          = TRUE,
                                   whisker_width         = 1,
                                   whisker_colour        = NULL,
                                   whisker_linewidth     = NULL,
                                   whisker_linetype      = 1,
                                   whisker_labels        = TRUE,
                                   steps                 = FALSE) {

  base <- if (steps) {
    ggplot2::guide_coloursteps(...)
  } else {
    ggplot2::guide_colourbar(...)
  }

  extra <- list(
    cap = cap, include_cap_val = include_cap_val, cap_labels = cap_labels,
    cap_min_dist = cap_min_dist,
    whiskers = whiskers, whisker_gap = whisker_gap,
    whisker_avoid_overlap = whisker_avoid_overlap, whisker_stem = whisker_stem,
    whisker_width = whisker_width,
    whisker_colour = whisker_colour, whisker_linewidth = whisker_linewidth,
    whisker_linetype = whisker_linetype, whisker_labels = whisker_labels
  )

  super <- if (steps) GuideColourstepsCapped else GuideColourbarCapped
  ggplot2::ggproto(NULL, super,
          params        = utils::modifyList(base$params, extra),
          available_aes = base$available_aes)
}

#' @rdname guide_colourbar_capped
#' @export
guide_coloursteps_capped <- function(...) guide_colourbar_capped(..., steps = TRUE)

# US spelling alias
#' @rdname guide_colourbar_capped
#' @export
guide_colorbar_capped <- guide_colourbar_capped
