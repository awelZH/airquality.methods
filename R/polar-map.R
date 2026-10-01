# =============================================================================
#  polar_map() — polar plots at their true coordinates on a map
# -----------------------------------------------------------------------------
#  polar_raster() answers "which wind directions bring the pollutant *here*".
#  It cannot show how that answer changes across a town, because one plot is
#  one place. polar_map() puts one rose at each site's real coordinate, so the
#  spatial gradient and any shared lobe become one picture.
#
#  How it works, and why:
#
#   * each site's u/v grid is shifted and scaled into LV95 metres, and drawn
#     as its own geom_raster() layer. One layer per site is not an accident:
#     geom_raster() checks that its data is an evenly spaced grid, and it
#     checks per layer, so eight separate layers are eight valid grids while
#     one combined layer would be a single invalid one.
#
#   * all layers share one fill scale, hence one legend. That is the whole
#     point — a map whose panels could not be compared to each other would
#     answer nothing.
#
#   * LV95 is metric and nothing is reprojected, so coord_fixed(ratio = 1) IS
#     the correct projection here. That is why this file imports no sf, no
#     terra, and no geospatial stack at all. Please do not add one.
#
#   * the wind-speed scale `k` (metres per m/s) is a single number for the
#     whole plot, never per site — see the note on the key rose in
#     R/polar_map_decor.R.
# =============================================================================

#' Polar plots at their true coordinates on a map
#'
#' Draws one [polar_raster()]-style rose per site, centred on the site's Swiss
#' LV95 coordinate, optionally over a swisstopo basemap. All roses share one
#' colour scale and one wind-speed scale, so they can be read against each
#' other.
#'
#' @param x Per-site polar data: a data frame with a site column plus `u`,
#'   `v`, `z` — the [dplyr::bind_rows()] idiom — or a named list of such data
#'   frames, as produced by
#'   `lapply(split(d, d$site), polar_bin, "pollutant")`. Output of
#'   [polar_bin()], [polar_sector()], or [openair::polarPlot()] all work.
#' @param sites Data frame with one row per site: the site name and its
#'   coordinates.
#' @param site Name of the site column, in both `x` and `sites`.
#' @param east,north Names of the coordinate columns in `sites`. Note that in
#'   the default site tables the **easting** is `x_lv95` and the **northing** `y_lv95` —
#'   the letters invert the Swiss convention. Nothing is auto-detected, and a
#'   transposed pair is an error rather than a wrong map.
#' @param u,v,z Column names of the Cartesian wind coordinates and the value.
#' @param radius Radius of a rose's outer ring, in metres. `NULL` (default)
#'   takes the largest radius at which no two roses overlap, rounded down to a
#'   readable number, and reports it.
#' @param r_max Wind speed drawn at `radius`. `NULL` (default) uses the
#'   largest radius present across all sites. A smaller value **clips** the
#'   roses rather than rescaling them — see Details.
#' @param basemap A [basemap_swisstopo()] object, or `NULL` for none.
#' @param grid Polar grid settings, see [polar_grid()]. Unlike in
#'   [polar_raster()] the compass is off by default: eight sites times four
#'   letters is clutter, and since every rose is oriented alike the key rose
#'   states it once for all of them (`polar_map_decor(key_compass = )`).
#' @param decor Map furniture — key rose, scale bar, labels; see
#'   [polar_map_decor()].
#' @param scale Colour scale. `NULL` (default) builds
#'   [ggplot2::scale_fill_viridis_c()] from `...`.
#' @param facet Faceting, as in [polar_raster()]. Default `NA` = none; a map
#'   is already faceted by place.
#' @param geom `"auto"` (default) picks raster or polygon from the data,
#'   `"raster"`, `"tile"`, or `"polygon"` force it.
#' @param interpolate Passed to [ggplot2::geom_raster()]; default `FALSE`.
#' @param na.rm Drop cells with a missing value. `TRUE` by default and rarely
#'   worth changing — see Details.
#' @param xlim,ylim Map extent in LV95. `NULL` = the basemap's extent, or the
#'   sites plus `margin`.
#' @param margin Extra space around the sites, as a fraction of the extent.
#' @param theme Plot theme. Default [theme_polar_map()].
#' @param quiet Suppress the informational messages about the chosen radius.
#' @param ... Arguments for the default colour scale, e.g. `limits`, `name`,
#'   `option`.
#'
#' @details
#' **One wind-speed scale for all sites.** `k = radius / r_max` is a single
#' number for the whole plot. Per-site normalisation would make a given ring
#' radius mean a different wind speed at each site, so the one key rose could
#' no longer describe the figure, and a reader comparing two lobes would be
#' comparing two rulers with nothing on the page saying so. If one site's tail
#' dominates, clip all of them with `r_max` instead — cells beyond it are
#' dropped before drawing, which keeps the roses inside `radius` and keeps the
#' non-overlap guarantee true.
#'
#' **`na.rm = TRUE` by default**, unlike [polar_raster()]. A
#' [openair::polarPlot()] grid is a full square whose corners are `NA`, and a
#' continuous fill scale paints missing values in an opaque grey — which on a
#' map is a grey square sitting over the basemap at every site. Dropping them
#' leaves a clean disc.
#'
#' **No north arrow.** Every rose already carries N/E/S/W, and LV95 grid north
#' is up. A fourth piece of furniture in the corners would earn nothing.
#'
#' @return A ggplot object, with the wind-speed scale recorded in the
#'   `polar_map_scale` attribute.
#'
#' @examples
#' # Three synthetic sites 1 km apart — no basemap, so this needs no network.
#' sites <- data.frame(
#'   site   = c("A", "B", "C"),
#'   x_lv95 = c(2686000, 2687000, 2686500),
#'   y_lv95 = c(1256000, 1256000, 1257000)
#' )
#' g <- expand.grid(u = seq(-8, 8, 1), v = seq(-8, 8, 1))
#' g <- g[sqrt(g$u^2 + g$v^2) <= 8, ]
#' dat <- do.call(rbind, lapply(sites$site, function(k) {
#'   d <- g
#'   d$z <- 100 - 8 * sqrt(d$u^2 + d$v^2) + d$u
#'   d$site <- k
#'   d
#' }))
#' polar_map(dat, sites, basemap = NULL, name = "value")
#'
#' \dontrun{
#' # With a swisstopo basemap (needs network on first call, then cached)
#' s  <- read.csv("site_metadata.csv")   # site, x_lv95 (E), y_lv95 (N)
#' bm <- basemap_swisstopo(bbox_lv95(s, radius = 200))
#' polar_map(dat, s, basemap = bm, name = "NOx")
#' }
#' @export
polar_map <- function(x,
                      sites,
                      site        = "site",
                      east        = "x_lv95",
                      north       = "y_lv95",
                      u           = "u",
                      v           = "v",
                      z           = "z",
                      radius      = NULL,
                      r_max       = NULL,
                      basemap     = NULL,
                      grid        = polar_grid(compass = FALSE),
                      decor       = polar_map_decor(),
                      scale       = NULL,
                      facet       = NA,
                      geom        = c("auto", "raster", "tile", "polygon"),
                      interpolate = NULL,
                      na.rm       = TRUE,
                      xlim        = NULL,
                      ylim        = NULL,
                      margin      = 0.06,
                      theme       = theme_polar_map(),
                      quiet       = FALSE,
                      ...) {

  geom <- match.arg(geom)
  # Rings, spokes, and the key rose take their look from the theme unless
  # polar_grid() was given an explicit one -- see polar.grid in R/zzz.R.
  grid <- .polar_grid_style(grid, theme)

  # --- Data, sites, and the join ------------------------------------------
  dat  <- .polar_map_bind(x, site)
  miss <- setdiff(c(u, v, z), names(dat))
  if (length(miss)) {
    cli::cli_abort(c(
      "Column(s) not found in {.arg x}: {.val {miss}}.",
      i = "Available: {.val {names(dat)}}."
    ))
  }

  shape <- if (geom != "auto") geom else {
    attr(dat, "polar_geom") %||% if (".cell" %in% names(dat)) "polygon" else "raster"
  }
  interpolate <- interpolate %||% attr(dat, "polar_interpolate") %||% FALSE

  if (isTRUE(na.rm)) {
    keep <- !is.na(dat[[z]])
    if (!any(keep)) cli::cli_abort("No non-missing values in {.field {z}}.")
    dat <- dat[keep, , drop = FALSE]
  }

  centres <- .polar_map_centres(sites, site, east, north)
  j       <- .polar_map_join(dat, centres, site, quiet)
  dat     <- j$x
  centres <- j$centres

  # --- Radius, rings, and the wind-speed scale ----------------------------
  radius <- .polar_map_resolve_radius(radius, centres, grid$expand, quiet)

  rad <- sqrt(dat[[u]]^2 + dat[[v]]^2)
  if (!is.null(r_max)) {
    # Clip, don't rescale: a rose that ran past `radius` would break the
    # non-overlap guarantee the default radius is built on.
    drop <- rad > r_max
    # For sector data a row is a polygon *vertex*, not a cell. Dropping
    # vertices would leave a cell that straddles r_max with its inner arc and
    # no outer one, which geom_polygon() closes into a sliver -- a shape the
    # data contains nowhere. So the unit of clipping is the cell, and a cell
    # goes as soon as any of its vertices reaches past r_max. `.cell` ids
    # restart per site, hence the site in the key.
    if (!is.null(dat$.cell)) {
      key  <- paste(dat[[site]], dat$.cell)
      drop <- key %in% unique(key[drop])
    }
    if (all(drop)) {
      cli::cli_abort(c(
        "{.arg r_max} = {r_max} removes every cell.",
        i = "The smallest radius in the data is {round(min(rad), 2)}."
      ))
    }
    if (any(drop) && !quiet) {
      # Count cells, not rows: with polygon data one cell is many rows, and
      # "drops 504 of 1512 cells" would then be off by the vertex count.
      n_all  <- if (is.null(dat$.cell)) length(drop) else
        length(unique(paste(dat[[site]], dat$.cell)))
      n_drop <- if (is.null(dat$.cell)) sum(drop) else
        length(unique(paste(dat[[site]], dat$.cell)[drop]))
      cli::cli_inform(c(i = "{.arg r_max} = {r_max} drops {n_drop} of
                            {n_all} cells."))
    }
    dat <- dat[!drop, , drop = FALSE]
    rad <- rad[!drop]
  }

  r_data <- max(rad, na.rm = TRUE)
  rings  <- grid$rings %||% {
    b <- pretty(c(0, r_data), grid$n)
    b[b > 0 & b <= r_data]
  }
  r_out <- max(r_max %||% r_data, rings, na.rm = TRUE)
  k     <- radius / r_out                       # metres per (m/s)

  # --- Transform into map coordinates -------------------------------------
  i <- match(dat[[site]], centres$site)
  dat$.E <- centres$e[i] + k * dat[[u]]
  dat$.N <- centres$n[i] + k * dat[[v]]

  # --- Extent --------------------------------------------------------------
  R <- radius * (1 + grid$expand)
  if (is.null(xlim) || is.null(ylim)) {
    if (!is.null(basemap)) {
      b <- basemap$bbox
      xlim <- xlim %||% c(b[["xmin"]], b[["xmax"]])
      ylim <- ylim %||% c(b[["ymin"]], b[["ymax"]])
    } else {
      ex <- margin * max(diff(range(centres$e)) + 2 * R,
                         diff(range(centres$n)) + 2 * R)
      xlim <- xlim %||% (range(centres$e) + c(-1, 1) * (R + ex))
      ylim <- ylim %||% (range(centres$n) + c(-1, 1) * (R + ex))
    }
  }

  # --- Surface -------------------------------------------------------------
  fc <- if (length(facet) == 1L && is.na(facet)) character(0) else
    .polar_facets(dat, facet, c(u, v, z, ".E", ".N", site))

  surface <- if (shape == "polygon") {
    if (is.null(dat$.cell)) {
      cli::cli_abort(c(
        "Polygon data without a {.field .cell} column.",
        i = "There's no way to tell which points belong to which cell."
      ))
    }
    # One layer for every site: geom_polygon() has no regularity constraint,
    # so all it needs is a group that is unique across sites. The
    # after_scale(fill) outline hides the seams between neighbouring cells —
    # on a basemap a light seam reads as a hole, so it matters more here than
    # it does in polar_raster().
    dat$.grp <- interaction(dat[[site]], dat$.cell, drop = TRUE)
    list(ggplot2::geom_polygon(
      data = dat,
      ggplot2::aes(.data$.E, .data$.N, fill = .data[[z]], group = .data$.grp,
                   colour = ggplot2::after_scale(.data$fill)),
      inherit.aes = FALSE, linewidth = 0.2))
  } else {
    .polar_map_raster_layers(dat, site, u, v, z, fc, geom, interpolate)
  }

  # --- Assemble ------------------------------------------------------------
  gl <- .polar_map_grid_layers(centres, grid, rings, r_out, k)

  # Key and scale bar in the same corner would sit on top of each other; this
  # turns that into a stack. A no-op unless both name the same corner.
  decor <- .polar_map_stack(decor, grid, r_out, k, xlim, ylim)

  p <- ggplot2::ggplot()
  if (!is.null(basemap)) p <- p + annotation_basemap(basemap)
  p <- p + if (isTRUE(grid$below)) c(gl, surface) else c(surface, gl)
  p <- p +
    .polar_map_key(decor, grid, rings, r_out, k, xlim, ylim, centres) +
    .polar_map_scalebar(decor, xlim, ylim) +
    .polar_map_sites_layers(decor, centres, radius, grid$expand) +
    (scale %||% .polar_scale(...)) +
    ggplot2::coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE) +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(big.mark = "\u2019", accuracy = 1)) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(big.mark = "\u2019", accuracy = 1)) +
    ggplot2::labs(x = "E [m]", y = "N [m]") +
    theme

  cap <- decor$attribution %||% basemap$attribution
  if (!isFALSE(cap) && !is.null(cap)) p <- p + ggplot2::labs(caption = cap)

  if (length(fc)) {
    p <- p + ggplot2::facet_wrap(
      stats::as.formula(paste("~", paste(fc, collapse = " + "))))
  }

  attr(p, "polar_map_scale") <- c(k = k, radius = radius, r_out = r_out)
  p
}

#' One raster layer per site
#'
#' The per-site split is what keeps `geom_raster()` legal: it validates even
#' spacing within a layer, and eight roses at eight different offsets are not
#' one evenly spaced grid.
#' @keywords internal
.polar_map_raster_layers <- function(dat, site, u, v, z, fc, geom, interpolate) {

  idx <- split(seq_len(nrow(dat)), dat[[site]], drop = TRUE)

  lapply(idx, function(i) {
    d <- dat[i, , drop = FALSE]

    # `geom`, not the resolved shape: only an explicit user choice overrides
    # the per-site check. Passing the resolved shape here would short-circuit
    # the check entirely, since it is already "raster" by that point.
    kind <- if (geom %in% c("raster", "tile")) geom else {
      grp <- if (length(fc)) interaction(d[fc], drop = TRUE) else
        factor(rep(1L, nrow(d)))
      # Same rule as polar_raster(): only a consistently even grid goes to
      # geom_raster(). A lattice looks like it should be fine — the gaps are
      # whole multiples of the cell size — but ggplot2 still reports "raster
      # pixels are placed at uneven intervals" and silently shifts them,
      # which on a map means cells drawn at the wrong coordinates. Gaps are
      # the norm for binned data with `min_n`, so this path is common.
      if (.polar_kind(d, u, v, grp) == "regular") "raster" else "tile"
    }

    if (kind == "tile") {
      ggplot2::geom_tile(
        data = d, ggplot2::aes(.data$.E, .data$.N, fill = .data[[z]]),
        inherit.aes = FALSE)
    } else {
      ggplot2::geom_raster(
        data = d, ggplot2::aes(.data$.E, .data$.N, fill = .data[[z]]),
        inherit.aes = FALSE, interpolate = interpolate)
    }
  })
}

#' Rings, outer circle, spokes and compass for every site
#'
#' One bind_rows'd layer per element rather than one per site: none of these
#' is a raster, so none carries a regularity constraint, and four layers beat
#' four times the number of sites.
#' @keywords internal
.polar_map_grid_layers <- function(centres, grid, rings, r_out, k) {

  ring_df <- function(radii) {
    dplyr::bind_rows(lapply(seq_len(nrow(centres)), function(i) {
      d <- dplyr::bind_rows(lapply(radii, .polar_circle))
      d$.E    <- centres$e[i] + k * d$.u
      d$.N    <- centres$n[i] + k * d$.v
      d$.site <- centres$site[i]
      d
    }))
  }

  gl  <- list()
  # Over a topographic basemap the grid needs a casing or it vanishes
  # wherever the map is dark; .polar_casing() is shared with the panel grid,
  # so a rose on a map and a rose in a facet are cased alike.
  cas <- .polar_casing(grid)

  if (length(rings)) {
    rdf <- ring_df(rings)
    raes <- ggplot2::aes(.data$.E, .data$.N,
                         group = interaction(.data$.site, .data$.r))
    if (!is.null(cas)) gl <- c(gl, list(ggplot2::geom_path(
      data = rdf, raes, inherit.aes = FALSE,
      colour = cas$colour, linewidth = cas$linewidth, alpha = cas$alpha)))
    gl <- c(gl, list(ggplot2::geom_path(
      data = rdf, raes,
      inherit.aes = FALSE, colour = grid$colour,
      linewidth = grid$linewidth, linetype = grid$linetype)))
  }

  if (length(grid$spokes)) {
    a  <- grid$spokes * pi / 180
    sp <- dplyr::bind_rows(lapply(seq_len(nrow(centres)), function(i) {
      data.frame(.E = centres$e[i], .N = centres$n[i],
                 .E1 = centres$e[i] + r_out * k * sin(a),
                 .N1 = centres$n[i] + r_out * k * cos(a))
    }))
    saes <- ggplot2::aes(x = .data$.E, y = .data$.N,
                         xend = .data$.E1, yend = .data$.N1)
    if (!is.null(cas)) gl <- c(gl, list(ggplot2::geom_segment(
      data = sp, saes, inherit.aes = FALSE,
      colour = cas$colour, linewidth = cas$linewidth, alpha = cas$alpha)))
    gl <- c(gl, list(ggplot2::geom_segment(
      data = sp, saes,
      inherit.aes = FALSE, colour = grid$colour,
      linewidth = grid$linewidth, linetype = grid$linetype)))
  }

  if (isTRUE(grid$outer)) {
    odf  <- ring_df(r_out)
    oaes <- ggplot2::aes(.data$.E, .data$.N, group = .data$.site)
    if (!is.null(cas)) gl <- c(gl, list(ggplot2::geom_path(
      data = odf, oaes, inherit.aes = FALSE,
      colour = cas$colour, linewidth = cas$linewidth, alpha = cas$alpha)))
    gl <- c(gl, list(ggplot2::geom_path(
      data = odf, oaes,
      inherit.aes = FALSE, colour = grid$colour,
      linewidth = grid$linewidth * 1.4, linetype = grid$outer_linetype)))
  }

  cmp <- .polar_compass(grid$compass)
  if (length(cmp)) {
    a  <- cmp * pi / 180
    cd <- dplyr::bind_rows(lapply(seq_len(nrow(centres)), function(i) {
      data.frame(.E = centres$e[i] + r_out * (1 + grid$expand / 2) * k * sin(a),
                 .N = centres$n[i] + r_out * (1 + grid$expand / 2) * k * cos(a),
                 .lab = names(cmp) %||% as.character(cmp))
    }))
    gl <- c(gl, list(ggplot2::geom_text(
      data = cd, ggplot2::aes(.data$.E, .data$.N, label = .data$.lab),
      inherit.aes = FALSE, size = grid$compass_size,
      fontface = grid$compass_fontface)))
  }
  gl
}
