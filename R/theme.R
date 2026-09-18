# ggplot2 themes for the recurring figure types.

#' Place the legend inside the panel
#'
#' Saves horizontal space on maps, where the panel usually has an empty corner.
#'
#' @param pos Position inside the panel, in relative units.
#' @param just Corner of the legend box anchored at `pos`.
#' @param mar Margin around the legend box.
#'
#' @return A ggplot2 theme, to be added to a plot.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   theme_legend_inside()
#'
#' @export
theme_legend_inside <- function(pos = c(0.975, 0.95),
                                just = c(1, 1),
                                mar = ggplot2::margin(4, 4, 4, 4)) {
  ggplot2::theme(
    legend.position = "inside",
    legend.position.inside = pos,
    legend.justification.inside = just,
    legend.background = ggplot2::element_rect(
      fill = scales::alpha("white", 0.6), color = "gray50"
    ),
    legend.margin = mar
  )
}

#' Theme for the recurring figure types
#'
#' Builds on the currently active theme ([ggplot2::theme_get()]) and strips what
#' the respective figure type does not need, so the data stays the loudest thing
#' on the page.
#'
#' @param type One of `"timeseries"`, `"scatter"`, `"flipped"`, `"map"` or
#'   `"default"`.
#' @param linecolor Colour of axis lines and ticks.
#'
#' @return A ggplot2 theme, to be added to a plot.
#'
#' @examples
#' library(ggplot2)
#' ggplot(economics, aes(date, unemploy)) +
#'   geom_line() +
#'   theme_custom("timeseries")
#'
#' @export
theme_custom <- function(type = c("timeseries", "scatter", "flipped", "map", "default"),
                         linecolor = "gray30") {
  type <- rlang::arg_match(type)

  base <- ggplot2::theme_get() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = ggplot2::rel(0.8)),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.8)),
      axis.line = ggplot2::element_line(color = linecolor),
      axis.ticks = ggplot2::element_line(color = linecolor),
      strip.text = ggplot2::element_text(size = ggplot2::rel(0.8), hjust = 0),
      legend.spacing.y = grid::unit(0, "mm"),
      plot.caption = ggplot2::element_text(
        hjust = 0, size = ggplot2::rel(0.5), color = "gray50"
      )
    )

  switch(type,
    timeseries = base + ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ),
    scatter = base + ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ),
    flipped = base + ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(),
      panel.grid.minor.x = ggplot2::element_blank()
    ),
    map = base + ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = ggplot2::rel(0.8), hjust = 0.5)
    ),
    default = base
  )
}
