# Shared expectations and accessors for the ggplot2 scales.

#' Render all the way to a gtable and fail on any warning *or* message.
#'
#' ggplot_build() is not enough: several warnings are only raised at draw time,
#' and those are precisely the ones that matter for a map.
expect_renders_clean <- function(p) {
  noise <- character(0)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  withCallingHandlers(
    ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)),
    warning = function(w) {
      noise <<- c(noise, paste("warning:", conditionMessage(w)))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      noise <<- c(noise, paste("message:", conditionMessage(m)))
      invokeRestart("muffleMessage")
    }
  )
  expect_equal(noise, character(0))
}

#' Draw a plot to a null device and return its gtable.
#'
#' Guides are only assembled during drawing, so anything about the legend --
#' cap labels, whiskers -- has to be asked of the gtable, not of the plot.
plot_gtable <- function(p) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
}

#' Every string drawn by a gtable's text grobs, in one character vector.
grob_labels <- function(gt) {
  out <- character(0)
  walk <- function(g) {
    if (inherits(g, "text") && !is.null(g$label)) out <<- c(out, as.character(g$label))
    kids <- g$children %||% (if (inherits(g, "gTree")) g$grobs else NULL)
    if (!is.null(kids)) for (k in kids) walk(k)
    if (inherits(g, "gtable")) for (k in g$grobs) walk(k)
  }
  walk(gt)
  unique(out)
}

#' Every grob name in a gtable, recursively.
#'
#' The capped guide adds its whiskers as named grobs inside the legend gtable,
#' so "is there a whisker" is a question about names, not pixels.
grob_names <- function(gt) {
  out <- character(0)
  walk <- function(g) {
    if (!is.null(g$name)) out <<- c(out, g$name)
    if (inherits(g, "gtable")) {
      # The name a grob was *placed* under lives in the layout, not on the
      # grob -- gtable_add_grob(name = ) does not rename what it is given.
      out <<- c(out, g$layout$name)
      for (k in g$grobs) walk(k)
    } else if (!is.null(g$children)) {
      for (k in g$children) walk(k)
    }
  }
  walk(gt)
  unique(out)
}
