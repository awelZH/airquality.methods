# =============================================================================
#  Sites: input shapes, coordinate validation, radius, overlap
# -----------------------------------------------------------------------------
#  Everything here is pure arithmetic on coordinates -- no ggplot, no network.
#  That is deliberate: it is the part that can be wrong in ways a picture
#  hides (a swapped coordinate pair still draws a perfectly nice map, just in
#  the wrong country), so it is also the part that is unit-tested.
# =============================================================================

# LV95 (EPSG:2056) covers Switzerland in these ranges. Eastings carry the
# 2'000'000 false easting and northings the 1'000'000 false northing, so in
# LV95 the easting is *always* larger than the northing -- which is what makes
# a transposed pair detectable rather than merely suspicious.
.LV95_E <- c(2485000, 2834000)
.LV95_N <- c(1075000, 1296000)

#' Bring the per-site data into one long data frame
#'
#' Accepts the long form (a data frame with a site column) or a named list of
#' data frames, which is what `lapply(split(d, d$site), polar_bin, ...)` gives
#' you. The list form binds with `.id`, so the names become the site column.
#' @keywords internal
.polar_map_bind <- function(x, site) {

  if (inherits(x, "openair")) x <- .polar_data(x)

  if (is.list(x) && !is.data.frame(x)) {
    if (!length(x)) cli::cli_abort("{.arg x} is an empty list.")
    if (is.null(names(x)) || any(!nzchar(names(x)))) {
      cli::cli_abort(c(
        "Every element of {.arg x} must be named when {.arg x} is a list.",
        i = "The names become the {.field {site}} column.",
        i = "{.code lapply(split(d, d$site), polar_bin, \"pn\")} names them for you."
      ))
    }
    # bind_rows() happens to keep the first element's attributes, but that is
    # undocumented behaviour to lean on -- and `polar_facets` decides what the
    # plot facets by, so losing it silently would change the figure, not just
    # an optimisation. Read them off the first element and set them again by
    # hand, so the guarantee is ours rather than dplyr's.
    keep <- attributes(x[[1L]])[c("polar_geom", "polar_interpolate", "polar_facets")]
    x <- dplyr::bind_rows(x, .id = site)
    for (k in names(keep)) if (!is.null(keep[[k]])) attr(x, k) <- keep[[k]]
    return(x)
  }

  if (!is.data.frame(x)) {
    cli::cli_abort(c(
      "{.arg x} must be a data frame, a named list of data frames, or an
       openair object.",
      x = "You supplied {.cls {class(x)}}."
    ))
  }
  as.data.frame(x)
}

#' Validate `sites` and return the centres as site/e/n
#'
#' Deliberately no column-name guessing. In the default site tables the easting
#' lives in a column called `x_lv95` and the northing in `y_lv95` -- the letters invert
#' the Swiss E/N convention, so anything that tried to be clever here would
#' eventually be clever in the wrong direction, silently.
#' @keywords internal
.polar_map_centres <- function(sites, site, east, north) {

  if (!is.data.frame(sites)) {
    cli::cli_abort(c(
      "{.arg sites} must be a data frame with one row per site.",
      x = "You supplied {.cls {class(sites)}}."
    ))
  }
  miss <- setdiff(c(site, east, north), names(sites))
  if (length(miss)) {
    cli::cli_abort(c(
      "Column(s) not found in {.arg sites}: {.val {miss}}.",
      i = "Available: {.val {names(sites)}}.",
      i = "Set {.arg site}, {.arg east}, and {.arg north} to match your data."
    ))
  }

  e <- as.numeric(sites[[east]])
  n <- as.numeric(sites[[north]])
  k <- as.character(sites[[site]])

  bad <- !stats::complete.cases(e, n)
  if (any(bad)) {
    cli::cli_abort(c(
      "Missing coordinates for {.val {k[bad]}}.",
      i = "Every site in {.arg sites} needs both {.field {east}} and {.field {north}}."
    ))
  }
  if (anyDuplicated(k)) {
    dup <- unique(k[duplicated(k)])
    cli::cli_abort(c(
      "Duplicated site{?s} in {.arg sites}: {.val {dup}}.",
      i = "Each site needs exactly one coordinate pair."
    ))
  }

  # The swap check. In LV95 easting > northing always, so a transposed pair
  # is a fact, not a guess.
  if (all(e < n)) {
    cli::cli_abort(c(
      "{.arg east} and {.arg north} look transposed.",
      x = "{.field {east}} ({round(stats::median(e))}) is smaller than
           {.field {north}} ({round(stats::median(n))}); in LV95 the easting
           is always the larger of the two.",
      i = "By default the easting is {.field x_lv95} and the northing
           {.field y_lv95}, despite the letters."
    ))
  }
  out <- range(e)
  if (out[1] < .LV95_E[1] || out[2] > .LV95_E[2] ||
      min(n) < .LV95_N[1] || max(n) > .LV95_N[2]) {
    cli::cli_warn(c(
      "Coordinates fall outside Switzerland.",
      # Parentheses because cli reads a leading dot inside braces as a style
      # name, not as a variable.
      i = "Expected LV95 easting {.val {(.LV95_E)}} and northing {.val {(.LV95_N)}}.",
      i = "Got easting {.val {round(range(e))}}, northing {.val {round(range(n))}}.",
      i = "Are these really EPSG:2056, and not WGS84 or LV03?"
    ))
  }

  data.frame(site = k, e = e, n = n, stringsAsFactors = FALSE)
}

#' Largest radius at which no two roses touch
#'
#' The drawn footprint is `radius * (1 + grid$expand)`, not `radius` -- the
#' compass letters sit out in that margin. So the usable radius is half the
#' smallest centre-to-centre distance, divided by the margin factor.
#' Returns `NULL` for a single site, where there is no distance to derive.
#' @keywords internal
.polar_map_radius <- function(e, n, expand = 0) {
  if (length(e) < 2L) return(NULL)
  d <- as.matrix(stats::dist(cbind(e, n)))
  diag(d) <- Inf
  min(d) / 2 / (1 + expand)
}

#' Round down to a readable number
#'
#' 204.5 -> 200, not 204.5. Map furniture that reads "202 m" invites the
#' reader to believe the number matters.
#' @keywords internal
.nice_down <- function(x, steps = c(1, 1.5, 2, 2.5, 3, 4, 5, 7.5)) {
  if (!is.finite(x) || x <= 0) return(x)
  p  <- 10^floor(log10(x))
  ok <- steps[steps * p <= x]
  if (!length(ok)) return(x)
  max(ok) * p
}

#' Closest pair of sites, as names and distance
#' @keywords internal
.polar_map_closest <- function(centres) {
  d <- as.matrix(stats::dist(cbind(centres$e, centres$n)))
  diag(d) <- Inf
  i <- which(d == min(d), arr.ind = TRUE)[1L, ]
  list(a = centres$site[i[1L]], b = centres$site[i[2L]], d = min(d))
}

#' Resolve `radius`, warning when the roses would overlap
#' @keywords internal
.polar_map_resolve_radius <- function(radius, centres, expand, quiet = FALSE) {

  r_fit <- .polar_map_radius(centres$e, centres$n, expand)

  if (is.null(radius)) {
    if (is.null(r_fit)) {
      cli::cli_abort(c(
        "{.arg radius} must be given when {.arg sites} has only one row.",
        i = "With no neighbouring site there is no distance to derive a
             non-overlapping radius from.",
        i = "{.code radius = 500} draws the outer ring 500 m from the centre."
      ))
    }
    radius <- .nice_down(r_fit)
    if (!quiet) {
      cl <- .polar_map_closest(centres)
      cli::cli_inform(c(
        "v" = "Using {.arg radius} = {radius} m.",
        i = "Largest non-overlapping radius here is {round(r_fit)} m
             ({.val {cl$a}} and {.val {cl$b}} are {round(cl$d)} m apart;
             the {.code polar_grid(expand = {expand})} compass margin is
             included)."
      ))
    }
    return(radius)
  }

  if (!is.numeric(radius) || length(radius) != 1L || !is.finite(radius) ||
      radius <= 0) {
    cli::cli_abort("{.arg radius} must be a single positive number, in metres.")
  }

  # Overlap is a warning, not an error: on a regional overview it is often the
  # deliberate, correct trade.
  if (!is.null(r_fit) && radius > r_fit) {
    foot <- radius * (1 + expand)
    d    <- as.matrix(stats::dist(cbind(centres$e, centres$n)))
    diag(d) <- Inf
    n_bad <- sum(d[upper.tri(d)] < 2 * foot)
    cl    <- .polar_map_closest(centres)
    cli::cli_warn(c(
      "!" = "Wind roses overlap: {.arg radius} = {radius} m draws a footprint
             of {round(foot)} m, but {n_bad} site pair{?s} {?is/are} closer
             than twice that.",
      i = "Closest: {.val {cl$a}} and {.val {cl$b}}, {round(cl$d)} m apart.",
      i = "The largest non-overlapping radius is {round(r_fit)} m."
    ))
  }
  radius
}

#' Match the data's sites against the coordinate table
#' @keywords internal
.polar_map_join <- function(x, centres, site, quiet = FALSE) {

  if (!site %in% names(x)) {
    cli::cli_abort(c(
      "Column {.val {site}} not found in {.arg x}.",
      i = "Available: {.val {names(x)}}.",
      i = "{.arg x} needs one column naming the site each row belongs to --
           bind your per-site results with
           {.code dplyr::bind_rows(.id = \"{site}\")}."
    ))
  }

  x[[site]] <- as.character(x[[site]])
  unknown   <- setdiff(unique(x[[site]]), centres$site)
  if (length(unknown)) {
    cli::cli_abort(c(
      "No coordinates for site{?s} {.val {unknown}}.",
      i = "Every site in {.arg x} must appear in {.arg sites}.",
      i = "{.arg sites} has: {.val {centres$site}}."
    ))
  }

  # The other direction is not an error: mapping a subset of a station network
  # is normal.
  unused <- setdiff(centres$site, unique(x[[site]]))
  if (length(unused) && !quiet) {
    cli::cli_inform(c(
      i = "No data for site{?s} {.val {unused}} -- not drawn."
    ))
  }
  list(x = x, centres = centres[centres$site %in% unique(x[[site]]), , drop = FALSE])
}
