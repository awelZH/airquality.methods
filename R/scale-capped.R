# =============================================================================
#  scale_capped() — capped ggplot2 colour scales
# -----------------------------------------------------------------------------
#  A generic colour scale (colour/fill) for ggplot2 that
#
#   * caps the value scale at `limits` (out-of-bounds -> squish instead of NA),
#   * automatically prefixes the outermost label with "<=" or ">=",
#   * draws the true data minimum/maximum as a whisker at a fixed distance
#     outside the colour scale,
#   * can compute limits from data quantiles on request,
#   * works as a continuous, binned, or discrete scale,
#   * accepts any colour palette,
#   * passes all other arguments through to the underlying ggplot2 scale.
#
#  See R/scale_capped_palette.R, R/scale_capped_limits.R, and
#  R/scale_capped_guide.R for the palette, quantile-limit, and guide/whisker
#  internals respectively.
# =============================================================================

#' Capped colour scale for ggplot2
#'
#' Creates a continuous, binned, or discrete colour scale (`colour` and/or
#' `fill`) with a freely chosen palette. Values outside `limits` are not
#' turned into `NA` as usual, but squished to the edge (`scales::oob_squish`);
#' the legend flags this with "≤"/"≥" and shows the true data extremes as
#' whiskers at a fixed distance outside the colour bar.
#'
#' @section Limits:
#' * `limits = c(0, 100)` — cap on both sides.
#' * `limits = c(0, NA)` — cap only the low side; `NA` takes the data edge.
#' * `probs = c(0.02, 0.98)` — limits as data quantiles (across all layers
#'   mapped to this scale). `probs = 0.02` is shorthand for `c(0.02, 0.98)`;
#'   `probs = c(NA, 0.98)` caps only the high side.
#' * `limits` and `probs` are mutually exclusive.
#'
#' Cap symbols and whiskers only appear where data actually lie outside the
#' limits — and only when `oob` genuinely pulls those values to the edge (see
#' `oob` below).
#'
#' @section Scale types:
#' * `"continuous"` — smooth colour ramp, legend = colour bar.
#' * `"binned"` — classed ramp, legend = colour steps. Class boundaries via
#'   `breaks = ` or `n.breaks = ` (via `...`).
#' * `"discrete"` — categorical scale. Capping/whiskers don't apply here and
#'   are ignored; `limits` selects the factor levels as usual.
#'
#' @param aesthetics `"fill"`, `"colour"`/`"color"`, or a vector of these.
#' @param type `"continuous"`, `"binned"`, or `"discrete"`.
#' @param palette Palette name from `grDevices::hcl.pals()`, a colour vector,
#'   or a function `f(n)`; see [.capped_palette()].
#' @param limits Numeric vector of length 2 (`NA` = open end) or `NULL`. For
#'   `type = "discrete"`, the levels to show.
#' @param probs Quantiles for automatic limit calculation (see above).
#' @param direction 1 or -1 (reverse the palette).
#' @param values Optional positions of the palette colours in \[0, 1\] (only
#'   `type = "continuous"`; as in `scale_*_gradientn(values = )`).
#' @param space Colour interpolation space, `"Lab"` or `"rgb"`.
#' @param na.value Colour for `NA` values.
#' @param oob Handling of values outside the limits. Default
#'   `scales::oob_squish` — this is precisely what makes it a capped scale.
#'   With `scales::oob_censor` (or `oob_keep`) such values become `NA`; then
#'   there's no edge class, and cap symbols and whiskers are omitted
#'   automatically — the scale behaves like an ordinary one.
#' @param cap,include_cap_val,cap_labels,cap_min_dist,whiskers,whisker_gap,whisker_avoid_overlap,whisker_stem,whisker_width,whisker_colour,whisker_linewidth,whisker_linetype,whisker_labels
#'   See [guide_colourbar_capped()].
#' @param guide Custom guide. Default `NULL` = the matching capped guide.
#'   `"colourbar"`, `"coloursteps"`, `"legend"`, `"none"`, etc. switch back to
#'   the standard ggplot2 guides.
#' @param ... Further arguments for [ggplot2::continuous_scale()],
#'   [ggplot2::binned_scale()], or [ggplot2::discrete_scale()] — e.g. `name`,
#'   `breaks`, `n.breaks`, `labels`, `trans`/`transform`, `expand`, `right`,
#'   `show.limits`, `nice.breaks`, `rescaler`.
#'
#' @return A ggplot2 scale object.
#'
#' @examples
#' library(ggplot2)
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_raster() +
#'   scale_fill_capped(limits = c(0.005, 0.03), palette = "Viridis")
#'
#' # the default palette is "Magma", i.e. viridis option "A"
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_raster() +
#'   scale_fill_capped(limits = c(0.005, 0.03))
#'
#' @export
scale_capped <- function(aesthetics = "fill",
                         type       = c("continuous", "binned", "discrete"),
                         palette    = "Magma",
                         limits     = NULL,
                         probs      = NULL,
                         direction  = 1,
                         values     = NULL,
                         space      = "Lab",
                         na.value   = "grey80",
                         oob        = scales::oob_squish,
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
                         guide      = NULL,
                         ...) {

  type       <- match.arg(type)
  aesthetics <- sub("^color", "colour", aesthetics)

  if (!is.null(probs) && !is.null(limits)) {
    cli::cli_abort(c(
      "{.arg limits} and {.arg probs} are mutually exclusive.",
      i = "Supply fixed limits, or quantile probabilities \u2014 not both."
    ))
  }
  if (type == "discrete" && !is.null(probs)) {
    cli::cli_warn("{.arg probs} is ignored for {.code type = \"discrete\"}.")
    probs <- NULL
  }

  pal <- .capped_palette(palette, direction = direction, values = values, space = space)

  # --- Choose the guide -------------------------------------------------
  if (is.null(guide)) {
    guide <- switch(
      type,
      continuous = ,
      binned = guide_colourbar_capped(
        cap = cap, include_cap_val = include_cap_val, cap_labels = cap_labels,
        cap_min_dist = cap_min_dist,
        whiskers = whiskers, whisker_gap = whisker_gap,
        whisker_avoid_overlap = whisker_avoid_overlap, whisker_stem = whisker_stem,
        whisker_width = whisker_width,
        whisker_colour = whisker_colour, whisker_linewidth = whisker_linewidth,
        whisker_linetype = whisker_linetype, whisker_labels = whisker_labels,
        steps = type == "binned"
      ),
      discrete = "legend"
    )
  }

  # --- Build the scale ---------------------------------------------------
  sc <- switch(
    type,

    continuous = ggplot2::continuous_scale(
      aesthetics = aesthetics,
      palette    = pal$ramp,
      limits     = limits,
      oob        = oob,
      na.value   = na.value,
      guide      = guide,
      super      = ScaleContinuousCapped,
      ...
    ),

    # binned: ggplot2 calls the palette with the bin midpoints in [0, 1];
    # length() -> number of classes, so a palette with exactly that many
    # colours is used as-is (not interpolated).
    binned = ggplot2::binned_scale(
      aesthetics = aesthetics,
      palette    = function(x) pal$n_pal(length(x)),
      limits     = limits,
      oob        = oob,
      na.value   = na.value,
      guide      = guide,
      super      = ScaleBinnedCapped,
      ...
    ),

    discrete = ggplot2::discrete_scale(
      aesthetics = aesthetics,
      palette    = pal$n_pal,
      limits     = limits,
      na.value   = na.value,
      guide      = guide,
      ...
    )
  )

  if (!is.null(probs)) sc$qprobs <- .capped_check_probs(probs)

  sc
}


# --- Convenience wrappers -----------------------------------------------------

#' @rdname scale_capped
#' @export
scale_fill_capped   <- function(...) scale_capped(aesthetics = "fill",   type = "continuous", ...)
#' @rdname scale_capped
#' @export
scale_colour_capped <- function(...) scale_capped(aesthetics = "colour", type = "continuous", ...)
#' @rdname scale_capped
#' @export
scale_fill_capped_b   <- function(...) scale_capped(aesthetics = "fill",   type = "binned", ...)
#' @rdname scale_capped
#' @export
scale_colour_capped_b <- function(...) scale_capped(aesthetics = "colour", type = "binned", ...)
#' @rdname scale_capped
#' @export
scale_fill_capped_d   <- function(...) scale_capped(aesthetics = "fill",   type = "discrete", ...)
#' @rdname scale_capped
#' @export
scale_colour_capped_d <- function(...) scale_capped(aesthetics = "colour", type = "discrete", ...)

# US spelling
#' @rdname scale_capped
#' @export
scale_color_capped   <- scale_colour_capped
#' @rdname scale_capped
#' @export
scale_color_capped_b <- scale_colour_capped_b
#' @rdname scale_capped
#' @export
scale_color_capped_d <- scale_colour_capped_d
