# =============================================================================
#  Palettes for scale_capped()
# =============================================================================

#' Normalise a palette into a common form
#'
#' @param palette One of:
#'   * a viridis map by name — "Magma" (the default), "Inferno", "Plasma",
#'     "Viridis", "Cividis", "Rocket", "Mako", "Turbo" — or by the
#'     single-letter `option` code viridis itself uses,
#'   * a string naming a palette from `grDevices::hcl.pals()`
#'     (e.g. "YlOrRd", "Blue-Red 3", "Zissou 1"),
#'   * a vector of colours (e.g. `c("white", "steelblue", "black")`),
#'   * a function `f(n)` returning n colours (e.g. `scales::pal_viridis()`,
#'     `\(n) RColorBrewer::brewer.pal(n, "RdBu")`).
#' @param direction 1 = as given, -1 = reversed.
#' @param values Optional positions of the palette colours in \[0, 1\] (as in
#'   `scale_*_gradientn(values = )`); allows non-linear colour ramps.
#' @param space Interpolation space, "Lab" (default) or "rgb".
#'
#' @return A list with
#'   * `n_pal(n)` — exactly n colours (for discrete/binned),
#'   * `ramp(x)`  — colour(s) for x in \[0, 1\] (for continuous).
#' @keywords internal
.capped_palette <- function(palette = "Magma",
                            direction = 1,
                            values = NULL,
                            space = "Lab") {

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be 1 or -1.")
  }

  cols <- NULL   # explicit stop colours, if known
  n_pal <- NULL  # function n -> n colours

  if (is.function(palette)) {

    n_pal <- function(n) {
      n <- max(as.integer(n), 1L)
      # Many palettes (e.g. RColorBrewer) return too many colours for small n,
      # or error out -> catch defensively and re-interpolate.
      out <- tryCatch(suppressWarnings(palette(n)), error = function(e) NULL)
      if (is.null(out)) out <- suppressWarnings(palette(max(n, 3L)))
      if (length(out) != n) out <- grDevices::colorRampPalette(out, space = space)(n)
      out
    }

  } else if (is.character(palette)) {

    # The viridis maps come first and from scales, not from hcl.colors():
    # five of the eight are missing from hcl.pals() entirely (magma among
    # them), so resolving them here is what makes one spelling work for all
    # of them.
    vir <- c(magma = "A", inferno = "B", plasma = "C", viridis = "D",
             cividis = "E", rocket = "F", mako = "G", turbo = "H")
    key <- NA_character_
    if (length(palette) == 1L) {
      k <- if (toupper(palette) %in% vir) toupper(palette)
           else unname(vir[tolower(palette)])
      if (!is.na(k)) key <- k
    }

    hcl_names <- grDevices::hcl.pals()
    idx <- if (is.na(key) && length(palette) == 1L)
      match(tolower(palette), tolower(hcl_names)) else NA_integer_

    if (!is.na(key)) {
      n_pal <- function(n) scales::pal_viridis(option = key)(max(as.integer(n), 1L))
      cols  <- scales::pal_viridis(option = key)(256)

    } else if (!is.na(idx)) {
      nm <- hcl_names[idx]
      n_pal <- function(n) grDevices::hcl.colors(max(as.integer(n), 1L), palette = nm)
      cols  <- grDevices::hcl.colors(256, palette = nm)

    } else {
      cols  <- palette
      n_pal <- function(n) {
        n <- max(as.integer(n), 1L)
        if (n == length(cols)) cols else grDevices::colorRampPalette(cols, space = space)(n)
      }
    }

  } else {
    cli::cli_abort(
      "{.arg palette} must be a colour vector, a palette name, or a function f(n)."
    )
  }

  if (direction == -1) {
    n_pal_fwd <- n_pal
    n_pal <- function(n) rev(n_pal_fwd(n))
    if (!is.null(cols)) cols <- rev(cols)
  }

  ramp_cols <- cols %||% n_pal(256)
  if (length(ramp_cols) == 1L) ramp_cols <- rep(ramp_cols, 2L)

  list(
    n_pal = n_pal,
    ramp  = scales::gradient_n_pal(ramp_cols, values = values, space = space)
  )
}
