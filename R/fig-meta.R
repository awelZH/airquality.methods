# Report metadata that travels with the plot but is never drawn. A figure is built by an analysis
# script and printed by a Quarto report; title, caption and methodological note belong to the
# object, not into its panel. Pairs with the plot catalog (plot-catalog.R), which carries the plot
# to the page. Moved from ufp25 (0.6.0).

#' Attach report metadata to a plot
#'
#' A figure is *built* by an analysis script and *printed* by a Quarto
#' report. The title, the one-sentence caption and the methodological note the
#' reader needs therefore belong to the plot object, not into its panel: drawn
#' into the graphic they would repeat the caption Quarto writes underneath it,
#' cost space, and have to be re-flowed whenever the figure is resized.
#'
#' `fig_meta()` stores those texts on the object and returns it, so it sits at
#' the end of the pipe that builds a figure. **Nothing it stores is drawn.**
#'
#' @details
#' Two carriers, one source of truth. The texts live in the object's `fig_meta`
#' attribute, which survives `+` and `ggsave()`. From them one alt text is
#' *generated* and written into ggplot2's own `alt` label, which knitr picks up
#' as `fig.alt` and screen readers read out — generated once, at attach time,
#' so the two cannot drift apart. Pass `alt` explicitly to write it yourself.
#'
#' For a patchwork composition only the attribute is set: adding `labs()` to a
#' patchwork would apply it to one of its panels rather than to the whole.
#'
#' In the report:
#'
#' ```
#' #| fig-cap: !expr airquality.methods::fig_caption(p1)
#' #| fig-alt: !expr airquality.methods::fig_alt(p1)
#' p1
#' ```
#'
#' @param p A ggplot (or patchwork) object.
#' @param title Short figure title. Not drawn — the report writes it, or
#'   Quarto's caption carries it.
#' @param caption One sentence saying what the figure shows, for `fig-cap`.
#' @param note The methodological detail: conditioning, statistic, why the
#'   figure is built the way it is. Belongs in the report body, not on the
#'   panel.
#' @param alt Alt text. Generated from `title` and `caption` when not given.
#' @return `p`, with the metadata attached.
#' @seealso [fig_caption()] and its siblings to read the texts back,
#'   [fig_index()] for the table of all figures of a script, [plot_catalog()]
#'   to carry the plots to the report.
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p <- fig_meta(p, title = "Verbrauch", caption = "Verbrauch nach Gewicht.")
#' fig_caption(p)
#' @export
fig_meta <- function(p, title = NULL, caption = NULL, note = NULL,
                     alt = NULL) {
  # The one mistake this catches is real and silent otherwise: `|>` binds
  # tighter than `+`, so `ggplot() + theme() |> fig_meta()` hands over the
  # *theme* and the metadata disappears with it.
  if (!ggplot2::is_ggplot(p) && !inherits(p, "patchwork")) {
    cli::cli_abort(c(
      "{.arg p} must be a ggplot or a patchwork, not {.cls {class(p)[[1]]}}.",
      i = "In a {.code +} chain write {.code fig_meta(p, ...)}: {.code |>}
           binds tighter than {.code +} and would take only the last piece."))
  }
  chr1 <- function(x, arg) {
    if (is.null(x)) return(NULL)
    if (!is.character(x) || length(x) != 1L || is.na(x)) {
      cli::cli_abort("{.arg {arg}} must be a single string.")
    }
    x
  }
  meta <- list(title   = chr1(title, "title"),
               caption = chr1(caption, "caption"),
               note    = chr1(note, "note"),
               alt     = chr1(alt, "alt"))
  # Keep what an earlier call attached: fig_meta() may be used twice, once
  # where the figure is built and once where it is placed.
  old <- attr(p, "fig_meta")
  if (!is.null(old)) meta <- utils::modifyList(old, Filter(Negate(is.null), meta))
  if (is.null(meta$alt)) meta$alt <- .fig_alt_text(meta)
  attr(p, "fig_meta") <- meta

  # ggplot2's own alt label, so knitr and screen readers see it too. Not for a
  # patchwork: there `+ labs()` would land on a single panel.
  if (inherits(p, "patchwork")) return(p)
  keep <- attr(p, "fig_meta")
  p <- p + ggplot2::labs(alt = meta$alt)
  attr(p, "fig_meta") <- keep
  p
}

#' The stored texts of a figure
#'
#' @param p A plot carrying [fig_meta()].
#' @param default Returned when the field is empty. `NULL` (the default) makes
#'   a missing field an error rather than a silently empty caption in the
#'   report.
#' @return A single string.
#' @rdname fig_meta_get
#' @export
fig_title <- function(p, default = NULL) .fig_field(p, "title", default)

#' @rdname fig_meta_get
#' @export
fig_caption <- function(p, default = NULL) .fig_field(p, "caption", default)

#' @rdname fig_meta_get
#' @export
fig_note <- function(p, default = NULL) .fig_field(p, "note", default)

#' @rdname fig_meta_get
#' @export
fig_alt <- function(p, default = NULL) .fig_field(p, "alt", default)

#' All figures of a script as a table
#'
#' What the report writer wants to see once: every figure with its title,
#' caption and note, so the text can be written against the set rather than
#' against one plot at a time.
#'
#' @param ... Named plots, or one named list of plots (e.g. from `mget()`).
#' @return A data frame with `figure`, `title`, `caption` and `note`.
#' @examples
#' library(ggplot2)
#' p <- fig_meta(ggplot(), title = "A", caption = "erste Abbildung")
#' fig_index(p1 = p)
#' @export
fig_index <- function(...) {
  ps <- list(...)
  if (length(ps) == 1L && is.null(names(ps)) && is.list(ps[[1L]])) ps <- ps[[1L]]
  if (is.null(names(ps)) || any(!nzchar(names(ps)))) {
    cli::cli_abort("Every plot must be named, e.g. {.code fig_index(p1 = p1)}.")
  }
  field <- function(f) vapply(ps, function(p) .fig_field(p, f, ""), character(1))
  data.frame(figure  = names(ps),
             title   = unname(field("title")),
             caption = unname(field("caption")),
             note    = unname(field("note")),
             row.names = NULL, stringsAsFactors = FALSE)
}

.fig_field <- function(p, field, default = NULL) {
  meta <- attr(p, "fig_meta")
  out  <- meta[[field]]
  if (!is.null(out) && nzchar(out)) return(out)
  if (!is.null(default)) return(default)
  cli::cli_abort(c(
    "This plot carries no {.field {field}}.",
    i = "Attach it where the figure is built: {.code fig_meta(p, {field} = \"...\")}."))
}

.fig_alt_text <- function(meta) {
  parts <- c(meta$title, meta$caption)
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) return(NULL)
  # "Titel. Ein Satz." -- a full stop is added where the writer left it out.
  paste(ifelse(grepl("[.!?]$", parts), parts, paste0(parts, ".")),
        collapse = " ")
}
