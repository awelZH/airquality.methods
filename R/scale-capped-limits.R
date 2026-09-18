# =============================================================================
#  Quantile limits — scale subclasses
# =============================================================================

#' Limits from data quantiles — scale subclasses
#'
#' ggplot2 allows `limits` to be a function, but only passes it the value
#' range (min/max) — not enough for quantiles. These subclasses therefore
#' remember the trained values in `train()` and compute the quantiles from
#' them in `get_limits()`. This has to go through `super =` rather than
#' overriding individual methods after the fact: ggplot2 clones scales when
#' building the plot (`Scale$clone()`), and only real ggproto methods then
#' bind to the cloned object.
#'
#' The buffer is indexed by data chunk, so training the same data repeatedly
#' doesn't grow it without bound.
#'
#' Everything runs in transformed space (`train()` receives already-
#' transformed values, `get_limits()` must return such values too). Since
#' quantiles are monotone-equivariant, the quantile of the transformed values
#' equals the transformed quantile.
#' @keywords internal
.capped_train_buffer <- function(self, x) {
  # Only a scale that was asked for quantiles needs the values kept. Without
  # this guard every capped scale -- most of which have fixed limits -- would
  # hold a second copy of its aesthetic column for the life of the plot.
  if (is.null(self$qprobs)) return(invisible(NULL))
  if (is.numeric(x) && length(x)) {
    xf <- x[is.finite(x)]
    if (length(xf)) {
      key <- paste(length(xf), sum(xf), xf[1L], xf[length(xf)], sep = "/")
      buf <- self$qprobs_buffer %||% list()
      buf[[key]] <- xf
      self$qprobs_buffer <- buf
    }
  }
  invisible(NULL)
}

#' @keywords internal
.capped_quantile_limits <- function(self, parent) {
  base  <- parent$get_limits()
  probs <- self$qprobs
  if (is.null(probs) || !is.numeric(base) || length(base) != 2L) return(base)
  v <- unlist(self$qprobs_buffer, use.names = FALSE)
  if (!length(v)) return(base)
  out <- as.numeric(base)
  ok  <- !is.na(probs)
  if (any(ok)) out[ok] <- unname(stats::quantile(v, probs[ok], na.rm = TRUE, names = FALSE))
  if (all(is.finite(out)) && out[1L] > out[2L]) out <- rev(out)
  out
}

#' @keywords internal
ScaleContinuousCapped <- ggplot2::ggproto(
  "ScaleContinuousCapped", ggplot2::ScaleContinuous,
  qprobs = NULL, qprobs_buffer = NULL,
  train = function(self, x) {
    .capped_train_buffer(self, x)
    ggplot2::ggproto_parent(ggplot2::ScaleContinuous, self)$train(x)
  },
  get_limits = function(self) {
    .capped_quantile_limits(self, ggplot2::ggproto_parent(ggplot2::ScaleContinuous, self))
  }
)

#' @keywords internal
ScaleBinnedCapped <- ggplot2::ggproto(
  "ScaleBinnedCapped", ggplot2::ScaleBinned,
  qprobs = NULL, qprobs_buffer = NULL,
  train = function(self, x) {
    .capped_train_buffer(self, x)
    ggplot2::ggproto_parent(ggplot2::ScaleBinned, self)$train(x)
  },
  get_limits = function(self) {
    .capped_quantile_limits(self, ggplot2::ggproto_parent(ggplot2::ScaleBinned, self))
  }
)

#' Check and normalise `probs`
#' @keywords internal
.capped_check_probs <- function(probs) {
  probs <- as.numeric(probs)
  if (length(probs) == 1L) probs <- c(probs, 1 - probs)
  if (length(probs) != 2L) {
    cli::cli_abort(c(
      "{.arg probs} must have length 1 or 2.",
      x = "You supplied length {length(probs)}."
    ))
  }
  if (any(probs < 0 | probs > 1, na.rm = TRUE)) {
    cli::cli_abort(c(
      "{.arg probs} must be between 0 and 1.",
      x = "You supplied {.val {probs}}."
    ))
  }
  probs
}
