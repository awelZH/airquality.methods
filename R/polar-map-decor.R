# =============================================================================
#  Map furniture: key rose, scale bar, site markers and labels
# -----------------------------------------------------------------------------
#  polar_raster() spends the plot's y-axis on wind speed. On a map the y-axis
#  is the northing, so that channel is gone and the wind-speed scale has to be
#  shown some other way.
#
#  The key rose is that other way, and it is drawn with the *same* metres-per-
#  (m/s) factor `k` as the data roses. Its rings are therefore physically the
#  same size as theirs -- not "kept in sync", but the same number used twice, so
#  a mismatch is unrepresentable rather than merely unlikely. A patchwork inset
#  sized in npc could not promise that.
#
#  Two rulers end up on the map and they measure different things: rose radius
#  is wind speed, map distance is metres. Both are labelled, deliberately.
# =============================================================================

#' Configure the map furniture
#'
#' Bundles everything that is neither the surface nor the polar grid, so
#' [polar_map()]'s signature stays lean -- the same split [polar_grid()] makes
#' for [polar_raster()].
#'
#' @param key Where to draw the reference rose: `"bottomleft"` (default),
#'   `"bottomright"`, `"topleft"`, `"topright"`, `FALSE` for none, or an
#'   explicit `c(E, N)` in map coordinates.
#' @param key_title Heading above the key. `NULL` (default) or `FALSE` = none,
#'   as with [polar_raster()]'s `axis_title`: every ring label already carries
#'   the unit, so a heading would say it a second time. A string sets one.
#' @param key_compass Compass labelling of the key rose, taking the values
#'   [polar_grid()] does. It is set here rather than inherited from the grid
#'   because on a map the roses carry no compass of their own: they are all
#'   oriented alike, so the orientation is stated once, here.
#' @param axis_unit Unit appended to the key's ring labels, as in
#'   [polar_raster()].
#' @param key_scale Size of the key rose relative to a data rose. `1
#'   (default) draws it at the map's own ruler, so a ring in the key is the
#'   ring on the roses -- that is what makes the key describe them rather
#'   than merely resemble them, and it is why the argument exists at 1 and
#'   not as a free choice. Below 1 the key becomes a *legend*: the labels
#'   still say which wind speed each ring is, but the radii no longer match
#'   the map. Worth it on a map of one rose drawn at a kilometre-scale
#'   radius, where a full-size key is furniture the size of the data;
#'   wrong on a map of several, where the shared ruler is the point.
#' @param scalebar Where to draw the distance bar, same values as `key`;
#'   default `"bottomright"`.
#' @param scalebar_length Bar length in metres. `NULL` = a round fraction of
#'   the map width.
#' @param labels Site names: `TRUE` (default) or `"above"` draws them above
#'   each rose, `"below"` under it, `FALSE` not at all. Above by default
#'   because a name read before the rose is a caption, and because the space
#'   under a rose is where the next site's name comes up on a dense map.
#' @param marker Draw a point at each site's true coordinate.
#' @param bg,bg_alpha Plaque behind key and scale bar, for legibility over the
#'   basemap. `bg = NA` (or `NULL`) for none.
#' @param colour Colour of the furniture (bar, ticks, text).
#' @param label_size,label_gap Site label size, and the distance of the label
#'   from the outer ring, as a fraction of the radius. The default sits the
#'   name just off the rim. With a compass on the roses
#'   (`polar_grid(compass = TRUE)`, letters at `1 + expand/2`) raise it.
#' @param marker_size,marker_colour Appearance of the site markers.
#' @param key_colour Colour of the key rose's own rings and spokes.
#'   `NULL` (default) takes the grid's colour, which is right as long as the
#'   grid is drawn for the basemap. It stops being right as soon as the grid
#'   is tuned *against* the basemap: a white grid reads beautifully over a
#'   dark rose and a dense map, and disappears completely on the key's own
#'   pale plaque. The key is furniture standing on its own background, so it
#'   is allowed a colour of its own — the geometry is what makes it describe
#'   the roses, not the ink.
#' @param key_size,title_size Text size in the key.
#' @param key_labels How many of the key's rings may carry a label
#'   (default 3, the outermost always among them); `Inf` labels every ring.
#'   The rings themselves are all drawn either way -- this is only about the
#'   text. A key is a small disc with text of a fixed point size in it, so
#'   the more rings the map has the less room each label has: at `WS_MAX =
#'   10` the five labels of a full key overprint each other and spill past
#'   its plaque. Thinning from the outside keeps the ring a reader is most
#'   likely to measure against -- the rim -- and keeps the spacing even.
#' @param pad Distance of key and scale bar from the panel edge, as a fraction
#'   of the map width.
#' @param attribution Caption text. `NULL` = the basemap's own attribution
#'   (required by swisstopo when a basemap is drawn), `FALSE` = none.
#'
#' @return A list with the settings.
#' @examples
#' polar_map_decor(key = "topright", scalebar = FALSE)
#' @export
polar_map_decor <- function(key             = "bottomleft",
                            key_title       = NULL,
                            key_compass     = TRUE,
                            axis_unit       = "m/s",
                            key_scale       = 1,
                            scalebar        = "bottomright",
                            scalebar_length = NULL,
                            labels          = TRUE,
                            marker          = TRUE,
                            bg              = "white",
                            bg_alpha        = 0.85,
                            colour          = "grey20",
                            label_size      = 2.9,
                            label_gap       = 0.06,
                            marker_size     = 1.1,
                            marker_colour   = "grey15",
                            key_colour      = NULL,
                            key_size        = 2.7,
                            key_labels      = 3,
                            title_size      = 2.9,
                            pad             = 0.02,
                            attribution     = NULL) {

  # `NULL` reads as "no plaque" to anyone who has met the rest of this API,
  # and a vector reads as a gradient that is not on offer. Both would
  # otherwise surface much later as a zero-length or vectorised `if`.
  if (is.null(bg)) bg <- NA
  if (length(bg) != 1L) {
    cli::cli_abort(c(
      "{.arg bg} must be a single colour.",
      i = "{.code NA} or {.code NULL} draws no plaque."
    ))
  }

  list(key = key, key_title = key_title, key_compass = key_compass,
       axis_unit = axis_unit, key_scale = key_scale,
       scalebar = scalebar, scalebar_length = scalebar_length,
       labels = labels, marker = marker, bg = bg, bg_alpha = bg_alpha,
       colour = colour, label_size = label_size, label_gap = label_gap,
       marker_size = marker_size, marker_colour = marker_colour,
       key_colour = key_colour,
       key_size = key_size, key_labels = key_labels,
       title_size = title_size, pad = pad,
       attribution = attribution)
}

#' Corner position for a piece of furniture
#' @keywords internal
.polar_map_corner <- function(pos, xlim, ylim, half_w, half_h, pad) {
  if (is.numeric(pos) && length(pos) == 2L) return(c(pos[1L], pos[2L]))
  p  <- pad * diff(xlim)
  cx <- if (grepl("left", pos))   xlim[1L] + p + half_w else xlim[2L] - p - half_w
  cy <- if (grepl("top",  pos))   ylim[2L] - p - half_h else ylim[1L] + p + half_h
  c(cx, cy)
}

#' Where do the site names go?
#'
#' `TRUE` means above, which is the default; the strings say it explicitly.
#' @keywords internal
.polar_map_label_position <- function(labels) {

  if (isTRUE(labels))  return("above")
  if (isFALSE(labels)) return("none")

  if (is.character(labels) && length(labels) == 1L) {
    pos <- switch(labels, above = "above", below = "below", none = "none", NULL)
    if (!is.null(pos)) return(pos)
  }

  cli::cli_abort(c(
    "{.arg labels} must be {.code TRUE}, {.code FALSE}, {.val above},
     {.val below}, or {.val none}.",
    x = "You supplied {.val {labels}}."
  ))
}

#' Is there a heading above the key?
#'
#' `NULL` and `FALSE` both mean no, so the two places that need to know --
#' the layout, which reserves headroom, and the drawing -- ask the same
#' question rather than each testing for themselves.
#' @keywords internal
.polar_map_has_title <- function(decor) {
  !is.null(decor$key_title) && !isFALSE(decor$key_title)
}

#' A filled disc, used as the plaque behind the key
#' @keywords internal
.polar_map_disc <- function(cx, cy, r) {
  d <- .polar_circle(r)
  data.frame(.E = cx + d$.u, .N = cy + d$.v)
}

#' The key's ring labels: a tick on the vertical, the number beside it
#'
#' One builder for both keys -- [polar_map()]'s, which lives in map metres,
#' and [polar_key()]'s, which lives in the u/v space of a rose -- so that a
#' reader who has learnt to read one has learnt to read the other. That is
#' the whole reason it takes `cx`, `cy` and `k`: the geometry differs, the
#' drawing must not.
#'
#' Vertical rather than along some bearing, because the north spoke is the
#' one line every rose has, and a number sitting on a tick against it reads
#' as an axis. `geom_label`, because on a map the text runs past the plaque
#' and over the basemap; on a plain panel the backing is invisible.
#' @keywords internal
.polar_key_ring_labels <- function(rings, r_out, k = 1, cx = 0, cy = 0,
                                   unit = "m/s", n_max = 3, size = 2.7,
                                   colour = "grey20", fill = "white",
                                   alpha = 0.85) {
  br <- .polar_key_labelled(rings[rings > 0], n_max)
  if (!length(br)) return(list())
  tick <- 0.035 * r_out * k
  list(
    ggplot2::geom_segment(
      data = data.frame(.E = cx, .N = cy + br * k,
                        .E1 = cx + tick, .N1 = cy + br * k),
      ggplot2::aes(x = .data$.E, y = .data$.N,
                   xend = .data$.E1, yend = .data$.N1),
      inherit.aes = FALSE, colour = colour, linewidth = 0.3),
    ggplot2::geom_label(
      data = data.frame(.E = cx + 1.6 * tick, .N = cy + br * k,
                        .lab = .polar_axis_labels(br, unit)),
      ggplot2::aes(.data$.E, .data$.N, label = .data$.lab),
      inherit.aes = FALSE, hjust = 0, vjust = 0.5,
      size = size, colour = colour, fill = fill, alpha = alpha,
      linewidth = 0, label.padding = grid::unit(0.6, "pt")))
}


#' The backing colour of the key's own text
#'
#' The plaque when there is one, white when there is not: the labels sit
#' partly outside the disc, and text on a topographic basemap needs
#' something behind it either way.
#' @keywords internal
.polar_key_fill <- function(decor) {
  if (isTRUE(is.na(decor$bg))) "white" else decor$bg
}


#' Which of the key's rings carry a label
#'
#' Counted from the outside, so the rim -- the ring a reader measures the
#' longest arm of a rose against -- is always named, and the kept rings are
#' evenly spaced. Added 2026-08-27, when `WS_MAX = 10` gave the key five
#' labels and they overprinted one another.
#' @keywords internal
.polar_key_labelled <- function(br, n_max) {
  br <- sort(br)
  if (!length(br) || !is.finite(n_max) || length(br) <= n_max) return(br)
  n_max <- max(1L, floor(n_max))
  step  <- ceiling(length(br) / n_max)
  rev(rev(br)[seq(1L, length(br), by = step)])
}


#' The reference rose
#'
#' Built from the same `.polar_circle()` / `.polar_compass()` helpers and the
#' same `k` as the data roses, so it describes them exactly.
#' @keywords internal
.polar_map_key <- function(decor, grid, rings, r_out, k, xlim, ylim,
                           centres = NULL) {

  if (isFALSE(decor$key) || is.null(decor$key)) return(list())

  # The key may be drawn smaller than a data rose (`key_scale`); everything
  # about its geometry then follows this one number, so the rings, the
  # spokes, the ticks and the compass letters cannot come apart.
  kk <- k * (decor$key_scale %||% 1)
  # The key stands on its own plaque, so it is neither cased nor obliged to
  # take a grid colour that was chosen for the basemap.
  grid$casing <- NULL
  grid$colour <- decor$key_colour %||% grid$colour
  R  <- r_out * (1 + grid$expand) * kk
  # Extra headroom for the title, which sits above the plaque and would
  # otherwise be clipped when the key is in a top corner.
  ttl_h <- if (.polar_map_has_title(decor)) 0.16 * R else 0
  xy <- .polar_map_corner(decor$key, xlim, ylim, R, R + ttl_h / 2, decor$pad)
  if (grepl("top", if (is.character(decor$key)) decor$key else "")) {
    xy[2L] <- xy[2L] - ttl_h / 2
  }
  cx <- xy[1L]; cy <- xy[2L]

  # The key is furniture laid over the data, so a collision with a rose is a
  # real defect in the figure, not a cosmetic one. Say which corners are free
  # rather than just complaining.
  #
  # Boxes, not centre distances: what actually collides is the key's title
  # against a site's name label, both of which hang outside their discs. A
  # centre-to-centre test misses exactly that case.
  # Deliberately not gated on `quiet`: that argument silences the routine
  # note about the chosen radius, not a genuine defect in the figure.
  if (!is.null(centres) && nrow(centres)) {
    # Slack, because text is sized in points and its extent in map metres is
    # not knowable until the device is known. A quarter of the radius is
    # roughly one line of a site name at the default sizes, and erring toward
    # a false warning is much cheaper than shipping a figure with the key
    # sitting on a label.
    slack   <- 0.25 * R
    lab_pos <- .polar_map_label_position(decor$labels)
    lab_gap <- if (lab_pos == "none") 0 else R * decor$label_gap + slack
    # The label hangs off whichever side it was put on, and that is the side
    # the box has to grow on -- a fixed downward allowance would miss every
    # collision with a name sitting above its rose.
    site_box <- list(
      x0 = centres$e - R - slack, x1 = centres$e + R + slack,
      y0 = centres$n - R - if (lab_pos == "below") lab_gap else 0,
      y1 = centres$n + R + if (lab_pos == "above") lab_gap else 0)
    hits <- function(qx, qy) {
      qx0 <- qx - R; qx1 <- qx + R
      qy0 <- qy - R; qy1 <- qy + R + ttl_h
      qx0 < site_box$x1 & qx1 > site_box$x0 &
        qy0 < site_box$y1 & qy1 > site_box$y0
    }
    hit <- hits(cx, cy)
    if (any(hit)) {
      corners <- c("bottomleft", "bottomright", "topleft", "topright")
      free <- Filter(function(p) {
        q <- .polar_map_corner(p, xlim, ylim, R, R + ttl_h / 2, decor$pad)
        if (grepl("top", p)) q[2L] <- q[2L] - ttl_h / 2
        !any(hits(q[1L], q[2L]))
      }, corners)
      cli::cli_warn(c(
        "!" = "The key rose overlaps {.val {centres$site[hit]}}.",
        i = if (length(free))
          "{cli::qty(length(free))}Free corner{?s}: {.val {free}} -- set
           {.code polar_map_decor(key = \"{free[1]}\")}."
        else
          "No corner is clear; place it by hand with
           {.code polar_map_decor(key = c(E, N))} or drop it with
           {.code polar_map_decor(key = FALSE)}."
      ))
    }
  }

  out <- list()

  if (!isTRUE(is.na(decor$bg))) {
    # Fill only, no edge: the grid's outer ring below is already a circle, and
    # a second one 17 % further out (the compass margin) reads as a mistake
    # rather than as a panel. The lightened fill is enough to lift the key off
    # the basemap.
    out <- c(out, list(ggplot2::geom_polygon(
      data = .polar_map_disc(cx, cy, R),
      ggplot2::aes(.data$.E, .data$.N), inherit.aes = FALSE,
      fill = decor$bg, alpha = decor$bg_alpha, colour = NA)))
  }

  # Rings, exactly as on the data roses.
  ring_df <- dplyr::bind_rows(lapply(rings, .polar_circle))
  ring_df$.E <- cx + kk * ring_df$.u
  ring_df$.N <- cy + kk * ring_df$.v
  if (nrow(ring_df)) {
    out <- c(out, list(ggplot2::geom_path(
      data = ring_df,
      ggplot2::aes(.data$.E, .data$.N, group = .data$.r), inherit.aes = FALSE,
      colour = grid$colour, linewidth = grid$linewidth, linetype = grid$linetype)))
  }
  # Direction spokes, exactly as on the data roses: the key is a rose, and a
  # rose without them reads as a different kind of object than the ones it is
  # supposed to explain.
  if (length(grid$spokes)) {
    a_sp <- grid$spokes * pi / 180
    out <- c(out, list(ggplot2::geom_segment(
      data = data.frame(.E = cx, .N = cy,
                        .E1 = cx + r_out * kk * sin(a_sp),
                        .N1 = cy + r_out * kk * cos(a_sp)),
      ggplot2::aes(x = .data$.E, y = .data$.N,
                   xend = .data$.E1, yend = .data$.N1),
      inherit.aes = FALSE, colour = grid$colour,
      linewidth = grid$linewidth, linetype = grid$linetype)))
  }

  out <- c(out, list(ggplot2::geom_path(
    data = transform(.polar_circle(r_out), .E = cx + kk * .u, .N = cy + kk * .v),
    ggplot2::aes(.data$.E, .data$.N), inherit.aes = FALSE,
    colour = grid$colour, linewidth = grid$linewidth * 1.4,
    linetype = grid$outer_linetype)))

  # The ticks need something to sit on. The north spoke already is that line,
  # so draw one only when the grid has no spoke at 0 degrees -- otherwise the
  # key would carry two lines up the same bearing.
  if (!any(abs((grid$spokes %% 360)) < 1e-8)) {
    out <- c(out, list(ggplot2::geom_segment(
      data = data.frame(.E = cx, .N = cy, .E1 = cx, .N1 = cy + r_out * kk),
      ggplot2::aes(x = .data$.E, y = .data$.N, xend = .data$.E1, yend = .data$.N1),
      inherit.aes = FALSE, colour = decor$colour, linewidth = 0.3)))
  }

  # Every ring is drawn; only some of them are labelled. See `key_labels`.
  out <- c(out, .polar_key_ring_labels(
    rings, r_out, k = kk, cx = cx, cy = cy, unit = decor$axis_unit,
    n_max = decor$key_labels %||% 3, size = decor$key_size,
    colour = decor$colour, fill = .polar_key_fill(decor),
    alpha = decor$bg_alpha))

  # The key's own setting, not the grid's: with no compass on the roses --
  # the default on a map -- this is the one place north is named.
  cmp <- .polar_compass(decor$key_compass)
  if (length(cmp)) {
    a <- cmp * pi / 180
    out <- c(out, list(ggplot2::geom_label(
      data = data.frame(
        .E = cx + r_out * (1 + grid$expand / 2) * kk * sin(a),
        .N = cy + r_out * (1 + grid$expand / 2) * kk * cos(a),
        .lab = names(cmp) %||% as.character(cmp)),
      ggplot2::aes(.data$.E, .data$.N, label = .data$.lab),
      inherit.aes = FALSE, size = decor$key_size,
      fontface = grid$compass_fontface, colour = decor$colour,
      fill = .polar_key_fill(decor), alpha = decor$bg_alpha,
      linewidth = 0, label.padding = grid::unit(0.6, "pt"))))
  }

  if (.polar_map_has_title(decor)) {
    ttl <- decor$key_title
    # The title sits outside the plaque, so it needs its own backing to stay
    # readable over the map.
    out <- c(out, list(ggplot2::geom_label(
      data = data.frame(.E = cx, .N = cy + R * 1.04, .lab = ttl),
      ggplot2::aes(.data$.E, .data$.N, label = .data$.lab),
      inherit.aes = FALSE, size = decor$title_size, vjust = 0,
      fontface = "bold", colour = decor$colour,
      fill = if (isTRUE(is.na(decor$bg))) "white" else decor$bg,
      alpha = decor$bg_alpha, linewidth = 0,
      label.padding = grid::unit(1.2, "pt"))))
  }
  out
}


#' Stack the key above the scale bar when both want the same corner
#'
#' Two pieces of furniture in one corner overlap, and until now the only way
#' out was to hand `key` explicit coordinates -- which needs `k`, the key's
#' radius and the bar's length, none of which the caller has. So it is
#' resolved here, where all three are already in hand: the bar keeps the
#' corner, the key moves up by the bar's own height, and both are centred on
#' one vertical, so that the pair reads as a single block of furniture.
#'
#' Returns `decor` with `key` and `scalebar` turned into explicit centres, so
#' everything downstream stays unchanged.
#' @keywords internal
.polar_map_stack <- function(decor, grid, r_out, k, xlim, ylim) {

  key <- decor$key
  sb  <- decor$scalebar
  same <- is.character(key) && is.character(sb) &&
    length(key) == 1L && length(sb) == 1L && identical(key, sb)
  if (!same) return(decor)

  R     <- r_out * (1 + grid$expand) * k * (decor$key_scale %||% 1)
  ttl_h <- if (.polar_map_has_title(decor)) 0.16 * R else 0
  L     <- .scalebar_length(decor$scalebar_length, xlim)
  h     <- 0.012 * diff(ylim)          # the bar's cap height, as it draws it
  p     <- decor$pad * diff(xlim)
  gap   <- 1.2 * h

  # One vertical for both, set so the wider of the two still clears the edge.
  cx <- if (grepl("left", sb)) xlim[1L] + p + max(R, L / 2)
        else                   xlim[2L] - p - max(R, L / 2)

  if (grepl("top", sb)) {
    y_sb  <- ylim[2L] - p - 2.2 * h
    y_key <- y_sb - h - gap - (R + ttl_h / 2)
  } else {
    y_sb  <- ylim[1L] + p + 2.2 * h
    # 3.4 h is where the bar's own plaque ends, above the label
    y_key <- y_sb + 3.4 * h + gap + (R + ttl_h / 2)
  }

  decor$scalebar <- c(cx, y_sb)
  decor$key      <- c(cx, y_key)
  decor
}
#' The map's distance scale bar
#'
#' The drawing is [annotation_scalebar()]'s; this only unpacks the decor. A map
#' that is not a [polar_map()] uses that one directly.
#' @keywords internal
.polar_map_scalebar <- function(decor, xlim, ylim) {

  if (isFALSE(decor$scalebar) || is.null(decor$scalebar)) return(list())

  .scalebar_layers(xlim = xlim, ylim = ylim, position = decor$scalebar,
                   length_m = decor$scalebar_length, colour = decor$colour,
                   bg = decor$bg, bg_alpha = decor$bg_alpha,
                   size = decor$key_size, pad = decor$pad)
}

#' Site markers and name labels
#' @keywords internal
.polar_map_sites_layers <- function(decor, centres, radius, expand) {
  out <- list()
  if (isTRUE(decor$marker)) {
    out <- c(out, list(ggplot2::geom_point(
      data = centres, ggplot2::aes(.data$e, .data$n), inherit.aes = FALSE,
      size = decor$marker_size, colour = decor$marker_colour)))
  }
  pos <- .polar_map_label_position(decor$labels)
  if (pos != "none") {
    # Measured from the outer ring, not from the drawn footprint: the
    # `expand` margin exists for the compass letters, and on a map the roses
    # carry none (the key states the directions once). Sitting the name out
    # there would leave a band of empty basemap between rose and label.
    gap <- radius * (1 + decor$label_gap)
    up  <- pos == "above"
    # geom_label, not geom_text: a topographic basemap is dense black line
    # work, and plain text laid over it is unreadable at any size. The filled
    # box is the cheap way to get a halo without a shadowtext dependency.
    out <- c(out, list(ggplot2::geom_label(
      data = transform(centres, .N = if (up) n + gap else n - gap),
      ggplot2::aes(.data$e, .data$.N, label = .data$site), inherit.aes = FALSE,
      size = decor$label_size, vjust = if (up) 0 else 1, colour = decor$colour,
      fill = if (isTRUE(is.na(decor$bg))) "white" else decor$bg,
      alpha = decor$bg_alpha, linewidth = 0,
      label.padding = grid::unit(0.9, "pt"))))
  }
  out
}
